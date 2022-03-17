{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Logger.Katip
  ( logSeverityToKSeverity
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
import qualified Data.HashMap.Strict as HM
import           Data.Proxy as P
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T.Builder
import           Katip as K hiding (logMsg)
import           Katip.Core (ItemFunc(..), LocJs(..), ProcessIDJs(..))
import           Katip.Monadic (KatipContextTState(..))
import           Katip.Scribes.ElasticSearch
import           Katip.Scribes.ElasticSearch.Internal as K


logSeverityToKSeverity :: LogSeverity -> Severity
logSeverityToKSeverity = \case
  Debug  -> DebugS
  Info -> InfoS
  Warn -> WarningS
  Error  -> ErrorS


-- | Adds the @rev-hash@ value to each record.
-- Early we copied `msg` to `msg-keyword` here but drop it
-- because purpose of this duplication is not clear.
ourFormatter
  :: Text
  -- ^ git revision string
  -> Verbosity
  -> ItemFunc Value
ourFormatter gitRev verb = ItemFunc $ \item -> go $ ourItemJson verb item
  where
    go :: Value -> Value
    go = \case
      Object h -> Object $ setRev h
      x        -> x -- drop exception here?
      where
        setRev :: HM.HashMap Text Value -> HM.HashMap Text Value
        setRev = HM.insert "rev-hash" (toJSON gitRev)

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
