{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Logger.Katip
  ( logSeverityToKSeverity
  , KatipContextTState(..)
  , getKatipLogger
  , ourItemJson
  , katipIndexNameString
  ) where

import           Control.EnvT
import           Control.Has.Katip ()
import           Control.Lens hiding ((.=))
import           Control.Logger.Internal
import           Data.Aeson hiding (Error)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T.Builder
import           GHC.Stack
import           Katip as K hiding (logMsg)
import           Katip.Core (LocJs(..), ProcessIDJs(..))
import           Katip.Monadic (KatipContextTState(..))


logSeverityToKSeverity :: LogSeverity -> Severity
logSeverityToKSeverity = \case
  Debug  -> DebugS
  Info -> InfoS
  Warn -> WarningS
  Error  -> ErrorS


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
getKatipLogger katipCtx = withFrozenCallStack $
  Logger (toObject . ltsContext $ katipCtx) mempty
    $ \ ctx stack s msg ->
      let ?callStack = stack
      in
        flip runEnvT katipCtx {ltsContext = liftPayload ctx}
          $ logLocM (logSeverityToKSeverity s) (logStr msg)
