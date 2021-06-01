{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Logger.Katip
  ( ElasticServer(..)
  , logSeverityToKSeverity
  , registerElastic
  , maybeElastic
  , KatipContextTState(..)
  , getKatipLogger
  , ourFormatter
  , ourMapping
  , katipIndexNameString
  , reopenableFileLog
  ) where

import           Control.EnvT
import           Control.Has.Katip ()
import           Control.Lens hiding ((.=))
import           Control.Logger.Internal
import           Control.Logger.Katip.Scribes.Reopenable
import           Data.Aeson hiding (Error)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy as P
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as T.Builder
import           Database.V5.Bloodhound
import           Katip as K hiding (logMsg)
import           Katip.Core (ItemFunc(..), LocJs(..), ProcessIDJs(..))
import           Katip.Monadic (KatipContextTState(..))
import           Katip.Scribes.ElasticSearch
import           Katip.Scribes.ElasticSearch.Internal as K
import           Network.HTTP.Client


data ElasticServer = ElasticServer
  { _esServer   :: !(Maybe Server)
  , _esUsername :: !(Maybe ByteString)
  , _esPassword :: !(Maybe ByteString)
  } deriving (Eq, Show)

instance FromJSON ElasticServer where
  parseJSON = withObject "ElasticServer" $ \obj -> ElasticServer
    <$> (obj .: "server")
    <*> (fmap T.encodeUtf8 <$> (obj .: "username"))
    <*> (fmap T.encodeUtf8 <$> (obj .: "password"))

instance ToJSON ElasticServer where
  toJSON es = object
    [ "server"   .= fmap fromServer (_esServer es)
    , "username" .= fmap T.decodeUtf8 (_esUsername es)
    , "password" .= fmap T.decodeUtf8 (_esPassword es)
    ]
    where
      fromServer (Server s) = s

logSeverityToKSeverity :: LogSeverity -> Severity
logSeverityToKSeverity = \case
  Debug  -> DebugS
  Info -> InfoS
  Warn -> WarningS
  Error  -> ErrorS

registerElastic
  :: Text
  -- ^ git revision
  -> Verbosity
  -> ElasticServer
  -> PermitFunc
  -> LogEnv
  -> IO LogEnv
registerElastic gitRev verbosity ElasticServer{..} permitF env =
  case _esServer of
    Nothing -> return env
    Just server -> do
      elastic <- do
        mgr <- newManager defaultManagerSettings
        let
          bloodhoundEnv = (mkBHEnv server mgr)
            { bhRequestHook = requestHook }
        mkEsScribe cfg bloodhoundEnv permitF verbosity
      registerScribe "elastic" elastic defaultScribeSettings env
    where
      cfg = (defaultEsScribeCfgV5 ixName mappingName)
        { essItemFormatter     = ourFormatter gitRev
        , essIndexMappingValue = ourMapping mappingName }
        where
          ixName  = IndexName katipIndexNameString
          mappingName = MappingName "logs"
      requestHook = return . case (_esUsername, _esPassword) of
        (Just u, Just p) -> applyBasicAuth u p
        _                -> id

-- | Doubles the value of @msg@ key into the @msg-keyword@ and adds
-- the @rev-hash@ value to each record. The @msg-keyword@ is needed to
-- group errors by message. The @rev-hash@ is needed for many purposes
-- and for grouping as well.
ourFormatter
  :: Text
  -- ^ git revision string
  -> Verbosity
  -> ItemFunc Value
ourFormatter gitRev verb = ItemFunc $ \item -> go $ ourItemJson verb item
  where
    go :: Value -> Value
    go = \case
      Object h -> Object $ setRev $ dupMsg h
      x        -> x -- drop exception here?
      where
        setRev :: HM.HashMap Text Value -> HM.HashMap Text Value
        setRev = HM.insert "rev-hash" (toJSON gitRev)
        dupMsg :: HM.HashMap Text Value -> HM.HashMap Text Value
        dupMsg m = HM.alter (const $ HM.lookup "msg" m) "msg-keyword" m

ourMapping :: K.MappingName ESV5 -> Value
ourMapping mn = object
  [fromMappingName prx mn .= object ["properties" .= object prs]]
  where
    prs =
      [ unanalyzedString "thread"
      , unanalyzedString "sev"
      , unanalyzedString "pid"
      -- ns is frequently fulltext searched
      , analyzedString "ns"
      -- we want message to be fulltext searchable
      , analyzedString "msg"
      -- we want to group by the message as well
      , unanalyzedString "msg-keyword"
      -- we want to group by revision hash
      , unanalyzedString "rev-hash"
      , "loc" .= locType
      , unanalyzedString "host"
      , unanalyzedString "env"
      , "at" .= dateType
      , unanalyzedString "app"
      ]
    prx = P.Proxy :: P.Proxy ESV5
    unanalyzedString k = k .= unanalyzedStringSpec prx
    analyzedString k = k .= analyzedStringSpec prx
    locType = object ["properties" .= object locPairs]
    locPairs =
      [ unanalyzedString "loc_pkg"
      , unanalyzedString "loc_mod"
      , unanalyzedString "loc_ln"
      , unanalyzedString "loc_fn"
      , unanalyzedString "loc_col"
      ]
    dateType = object ["format" .= esDateFormat, "type" .= String "date"]

katipIndexNameString :: Text
katipIndexNameString = "antorica-b2b-logs"

ourItemJson :: LogItem a => Verbosity -> Item a -> Value
ourItemJson verb item = formatItem $
  item & itemPayload %~ payloadObject verb
  where
  formatItem Item{..} = object
    [ "app"    .= T.intercalate "." (unNamespace _itemApp)
    , "env"    .= _itemEnv
    , "sev"    .= _itemSeverity
    , "thread" .= getThreadIdText _itemThread
    , "host"   .= _itemHost
    , "pid"    .= ProcessIDJs _itemProcess
    , "data"   .= _itemPayload
    , "msg"    .= T.Builder.toLazyText (unLogStr _itemMessage)
    , "at"     .= _itemTime
    , "ns"     .= T.intercalate "." (unNamespace _itemNamespace)
    , "loc"    .= fmap LocJs _itemLoc
    ]

-- | Return empty server if Nothing
maybeElastic :: Maybe ElasticServer -> ElasticServer
maybeElastic = \case
  Nothing -> ElasticServer Nothing Nothing Nothing
  Just a  -> a

instance LogItem Object where
  payloadKeys V0 _ = SomeKeys []
  payloadKeys _ _  = AllKeys

getKatipLogger :: KatipContextTState -> Logger
getKatipLogger katipCtx = Logger (toObject . ltsContext $ katipCtx)
  $ \ ctx stack s msg ->
    let ?callStack = stack
    in
      flip runEnvT katipCtx {ltsContext = liftPayload ctx}
        $ logLocM (logSeverityToKSeverity s) (logStr msg)
