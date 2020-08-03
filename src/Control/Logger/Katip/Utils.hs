{-# LANGUAGE ImplicitParams #-}

module Control.Logger.Katip.Utils
  ( katipIndexNameString
  , module Katip.Monadic
  , ElasticServer(..)
  , elasticServer
  , maybeElastic
  , registerElastic
  , LogFormat(..)
  , registerFileScribe
  , setLoggingCtx
  , ourFormatter
  , ourMapping
  , getKatipLogger
  , logSeverityToKSeverity
  ) where

import           Control.EnvT
import           Control.Exception
import           Control.Has
import           Control.Lens hiding ((.=))
import           Control.Logger as L
import           Control.Logger.Internal as L
import           Control.Logger.Katip as L
import           Data.Aeson hiding (Error)
import           Data.Aeson.Text
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B.Builder
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy.Builder as T.Builder
import qualified Data.Text.Lazy.Encoding as T.Lazy
import           Database.V5.Bloodhound as BH
import           Katip as K hiding (logMsg)
import           Katip.Core (ItemFunc(..))
import           Katip.Monadic (KatipContextTState(..))
import qualified Katip.Scribes.Handle as K
import           Options.Applicative
import           System.IO


elasticServer :: Parser ElasticServer
elasticServer =
  ElasticServer
    <$> optional elasticServerAddress
    <*> optional elasticServerUsername
    <*> optional elasticServerPassword

elasticServerAddress :: Parser Server
elasticServerAddress = Server <$> option
  (T.pack <$> str)
  (long "elastic-server" <> metavar "ELASTIC_SERVER" <> help
    "Address of elastic search server"
  )

elasticServerUsername :: Parser ByteString
elasticServerUsername = option
  (encodeUtf8 . T.pack <$> str)
  (long "elastic-server-username" <> metavar "ELASTIC_SERVER_USERNAME" <> help
    "Elastic server username"
  )

elasticServerPassword :: Parser ByteString
elasticServerPassword = option
  (encodeUtf8 . T.pack <$> str)
  (long "elastic-server-password" <> metavar "ELASTIC_SERVER_PASSWORD" <> help
    "Elastic server password"
  )

-- | How to format logs
data LogFormat
  = LogBracket -- ^ use `bracketFormat`
  | LogJson    -- ^ use `jsonFormat`
  deriving (Eq, Show)

registerFileScribe
  :: Text -> Verbosity -> LogFormat -> FilePath -> PermitFunc -> [(Text, Text)]
  -> LogEnv -> IO LogEnv
registerFileScribe gitRev verbosity format filePath permitF additions logenv =
  bracketOnError (openFile filePath AppendMode) hClose $ \h -> do
    scribe_ <- mkHandleScribeWithFormatter formatter
      (ColorLog False) h permitF verbosity
    let
      fileScribe = scribe_
        { scribeFinalizer = scribeFinalizer scribe_ `finally` hClose h
        }
    registerScribe "file" fileScribe defaultScribeSettings logenv
  where
    formatter :: forall a. LogItem a => ItemFormatter a
    formatter = case format of
      LogBracket -> customBracketFormat additions'
      LogJson    -> customJsonFormat gitRev additions'
    additions' = HM.fromList . map (fmap String) $ additions

-- | JSON formatter that adds the specified fields to the output
customJsonFormat
  :: forall a. LogItem a => Text -> HM.HashMap Text Value -> ItemFormatter a
customJsonFormat gitRev additions withColor verb i = (mappend "\n") $
  -- Here we reimplement the standard formater because we want the
  -- additional key/value pairs to be added at the top level.
  -- So we can't use the LogWithAdditions like we do for bracket format
  B.Builder.lazyByteString . T.Lazy.encodeUtf8 . T.Builder.toLazyText $
    K.colorBySeverity withColor (_itemSeverity i) $
      encodeToTextBuilder (extend $ applyItemFunc (ourFormatter gitRev verb) i)
  where
    extend (Object o) = Object $ HM.union additions o
    extend v          = v

-- | Bracket formatter that adds the specified fields to the output
customBracketFormat
  :: forall a. LogItem a => HM.HashMap Text Value -> ItemFormatter a
customBracketFormat additions withColor verb i =
  bracketFormat withColor verb (LogWithAdditions additions <$> i)

-- | Log item payload that adds the specified additional key/value pairs
-- to the output on at verbosity levels
data LogWithAdditions a = LogWithAdditions (HM.HashMap Text Value) a
  deriving (Show)

instance LogItem a => LogItem (LogWithAdditions a) where
  payloadKeys v (LogWithAdditions additions a) =
    case payloadKeys v a of
      AllKeys       -> AllKeys
      SomeKeys keys -> SomeKeys (HM.keys additions ++ keys)

instance ToObject a => ToObject (LogWithAdditions a) where
  toObject (LogWithAdditions additions a) =
    HM.union additions (toObject a)

setLoggingCtx
  :: (Has Logger r, Monad m)
  => Object
  -> ReaderT r m a
  -> ReaderT r m a
setLoggingCtx ctx' = local (part %~ go)
  where
    go (Logger ctx f) =
      Logger (ctx <> ctx') f
