module Control.Logger.Internal
  ( Logger(..)
  , runLogger
  , LogSeverity(..)
  , LoggingMonad
  , logMsg
  , logMsgWith
  ) where

import Control.Has
import Control.DeepSeq
import Data.Aeson (ToJSON, FromJSON, Object)
import Data.Functor.Const
import Data.Text (Text)
import GHC.Stack


data LogSeverity
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Show, Ord, Generic)

instance ToJSON LogSeverity
instance FromJSON LogSeverity

data Logger
  = Logger
  { _loggerContext :: Object
  , _loggerAction  :: Object -> CallStack -> LogSeverity -> Text -> IO ()
  } deriving Generic

instance NFData Logger

instance Semigroup Logger where
  (Logger ctx1 l1) <> (Logger ctx2 l2) = Logger ctx'
    $ \ctx stack s t -> l1 ctx stack s t >> l2 ctx stack s t
    where ctx' = ctx1 <> ctx2

instance Monoid Logger where
  mempty = Logger mempty $ \_ _ _ _ -> pure ()

type LoggingMonad r m =
  (MonadReader r m, Has Logger r, MonadIO m, HasCallStack)

logMsg
  :: (LoggingMonad r m)
  => LogSeverity
  -> Text
  -> m ()
logMsg s t = withFrozenCallStack $ do
  logger <- asks (getConst . part Const)
  logMsgWith logger s t

logMsgWith
  :: (MonadIO m, HasCallStack)
  => Logger
  -> LogSeverity
  -> Text
  -> m ()
logMsgWith logger s t =
  liftIO $ runLogger logger callStack s t

runLogger :: Logger -> CallStack -> LogSeverity -> Text -> IO ()
runLogger (Logger ctx logger) = logger ctx
