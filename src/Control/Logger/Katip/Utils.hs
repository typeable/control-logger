{-# LANGUAGE CPP #-}

module Control.Logger.Katip.Utils
  ( katipIndexNameString
  , module Katip.Monadic
  , LogFormat(..)
  , registerFileScribe
  , getKatipLogger
  , logSeverityToKSeverity
  ) where

import           Control.Exception
import           Control.Logger.Katip as L
import           Data.Aeson hiding (Error)
import           Data.Aeson.Text
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as HM
import qualified Data.Aeson.Key as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import           Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T.Builder
import           Katip as K hiding (logMsg)
import           Katip.Monadic (KatipContextTState(..))
import qualified Katip.Scribes.Handle as K
import           System.IO


#if MIN_VERSION_aeson(2,0,0)
type KeyMap v = HM.KeyMap v
textToKey :: Text -> Key
textToKey = HM.fromText
keyToText :: Key -> Text
keyToText = HM.toText
#else
type KeyMap v = HM.HashMap Text v
type Key = Text
textToKey :: Text -> Key
textToKey = id
keyToText :: Key -> Text
keyToText = id
#endif

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
    additions'
      = HM.fromList
      . map (\(k, v) -> (textToKey k, String v))
      $ additions

-- | JSON formatter that adds the specified fields to the output
customJsonFormat
  :: forall a. LogItem a => Text -> KeyMap Value -> ItemFormatter a
customJsonFormat gitRev additions withColor verb i = mappend "\n" $
  -- Here we reimplement the standard formater because we want the
  -- additional key/value pairs to be added at the top level.
  -- So we can't use the LogWithAdditions like we do for bracket format
  T.Builder.fromText $
    K.colorBySeverity withColor (_itemSeverity i) $
      (T.toStrict . encodeToLazyText) (extend $ ourItemJson verb i)
  where
    extend (Object o) = Object $ HM.union additions' o
    extend v          = v
    additions' = HM.insert "rev-hash" (toJSON gitRev) additions

-- | Bracket formatter that adds the specified fields to the output
customBracketFormat
  :: forall a. LogItem a => KeyMap Value -> ItemFormatter a
customBracketFormat additions withColor verb i =
  bracketFormat withColor verb (LogWithAdditions additions <$> i)

-- | Log item payload that adds the specified additional key/value pairs
-- to the output on at verbosity levels
data LogWithAdditions a = LogWithAdditions (KeyMap Value) a
  deriving (Show)

instance LogItem a => LogItem (LogWithAdditions a) where
  payloadKeys v (LogWithAdditions additions a) =
    case payloadKeys v a of
      AllKeys       -> AllKeys
      SomeKeys keys -> SomeKeys (map keyToText (HM.keys additions) ++ keys)

instance ToObject a => ToObject (LogWithAdditions a) where
  toObject (LogWithAdditions additions a) =
    HM.union additions (toObject a)
