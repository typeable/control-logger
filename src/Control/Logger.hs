{-# LANGUAGE CPP #-}
module Control.Logger
  ( Logger
  , loggerContext
  , logMsg
  , logMsgWith
  , logInfo
  , logDebug
  , logWarn
  , logError
  , logInfoWith
  , logDebugWith
  , logWarnWith
  , logErrorWith
  , tryLogError
  , tryJustLog
  , tryJustWarn
  , logExceptions
  , LogSeverity(..)
  , renderLogLevel
  , parseLogLevel
  , oneoffLog
  , LoggingMonad
  , LogSource
  , fileLogger
  , silentLogger
  , flushingFastFunc
  , flushingStderrLogger
  , stderrLogger
  , stdoutLogger
  , defaultFastFunc
  , compactCallStack
  , st
  , ToObject(..)
  ) where

import           Control.Has
import           Control.Logger.Internal
import           Control.Logger.Orphans  ()
import           Control.Monad.Catch
import qualified Data.List               as List
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Time
import           GHC.Stack
import           Katip                   (ToObject (..))
import           System.Log.FastLogger
import           Text.Shakespeare.Text   (st)
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
import Control.Monad.IO.Class
#endif

logError
  :: (LoggingMonad r m)
  => Text
  -> m ()
logError msg = withFrozenCallStack (logMsg Error msg)

logDebug
  :: (LoggingMonad r m)
  => Text
  -> m ()
logDebug msg = withFrozenCallStack (logMsg Debug msg)

logInfo
  :: (LoggingMonad r m)
  => Text
  -> m ()
logInfo msg = withFrozenCallStack (logMsg Info msg)

logWarn
  :: (LoggingMonad r m)
  => Text
  -> m ()
logWarn msg = withFrozenCallStack (logMsg Warn msg)

logInfoWith
  :: (MonadIO m, HasCallStack)
  => Logger
  -> Text
  -> m ()
logInfoWith logger msg = withFrozenCallStack (logMsgWith logger Info msg)

logDebugWith
  :: (MonadIO m, HasCallStack)
  => Logger
  -> Text
  -> m ()
logDebugWith logger msg = withFrozenCallStack (logMsgWith logger Debug msg)

logWarnWith
  :: (MonadIO m, HasCallStack)
  => Logger
  -> Text
  -> m ()
logWarnWith logger msg = withFrozenCallStack (logMsgWith logger Warn msg)

logErrorWith
  :: (MonadIO m, HasCallStack)
  => Logger
  -> Text
  -> m ()
logErrorWith logger msg = withFrozenCallStack (logMsgWith logger Error msg)

-- | Log exceptions occured in computation and just ignore them.
tryLogError
  :: (MonadIO m, MonadCatch m, Has Logger r, MonadReader r m)
  => m ()
  -> m ()
tryLogError a = try a >>= \case
  Left  (e :: SomeException) -> logError . Text.pack $ show e
  Right x                    -> return x

tryJustLog
  :: (MonadIO m, MonadCatch m, Has Logger r, MonadReader r m)
  => m a
  -> (Text -> Text)
  -- ^ generates error message from exception type + exception text
  -> m (Maybe a)
tryJustLog action genMsg = do
  let
    logAll :: SomeException -> Maybe SomeException
    logAll       = Just
    errorHandler (SomeException e) =
      logError (genMsg $ Text.pack $ show e) >> pure Nothing
  catchJust logAll (Just <$> action) errorHandler

tryJustWarn
  :: (MonadIO m, Has Logger r, MonadCatch m, MonadReader r m)
  => m a
  -> (Text -> Text)
  -- ^ generates warning message from exception type + exception text
  -> m (Maybe a)
tryJustWarn action genMsg = do
  let
    logAll :: SomeException -> Maybe SomeException
    logAll       = Just
    errorHandler (SomeException e) =
      logWarn (genMsg $ Text.pack $ show e) >> pure Nothing
  catchJust logAll (Just <$> action) errorHandler

logExceptions :: (MonadCatch m, MonadThrow m, LoggingMonad r m) => m a -> m a
logExceptions ma = catch ma $ \e -> do
  logError $ (Text.pack . show) (e :: SomeException)
  throwM e

oneoffLog :: MonadIO m => Logger -> LogSeverity -> Text -> m ()
oneoffLog logger s t =
  withFrozenCallStack (flip runReaderT logger $ logMsg s t)

fileLogger :: MonadIO m => FilePath -> m Logger
fileLogger fp = fastFunc <$> liftIO (newFileLoggerSet defaultBufSize fp)

silentLogger :: Logger
silentLogger = Logger mempty (\_ _ _ _ -> return ())

-- | Logger which flushes buffer after each message. This is useful with
-- stderrLogger.
flushingFastFunc
  :: LogSeverity -> LoggerSet -> Logger
flushingFastFunc minLogLevel loggerSet =
  Logger mempty $ \_ stack logLevel msg ->
    when (logLevel >= minLogLevel) $ do
      ts <- getCurrentTime
      pushLogStr loggerSet
        $ formatLogStr ts stack mempty logLevel (toLogStr msg)
      flushLogStr loggerSet

-- | Used only in tests.
stderrLogger :: MonadIO m => LogSource -> m Logger
stderrLogger loc =
  defaultFastFunc loc Debug <$> liftIO (newStderrLoggerSet defaultBufSize)

stdoutLogger :: MonadIO m => LogSource -> m Logger
stdoutLogger loc =
  defaultFastFunc loc Debug <$> liftIO (newStdoutLoggerSet defaultBufSize)

-- | same as 'stderrLogger' but flushes buffer after each line.
flushingStderrLogger :: MonadIO m => m Logger
flushingStderrLogger =
  flushingFastFunc Debug <$> liftIO (newStderrLoggerSet defaultBufSize)

-- | Make our outdated 'Logger' of fast-logger's 'LoggerSet'
fastFunc :: LoggerSet -> Logger
fastFunc loggerSet =
  Logger mempty $ \_ _ _ msg -> pushLogStr loggerSet $ toLogStr msg

type LogSource = Text

-- | Default logger tests and same things.
defaultFastFunc :: LogSource -> LogSeverity -> LoggerSet -> Logger
defaultFastFunc loc minLogLevel loggerSet =
  Logger mempty $ \_ stack s msg -> when (s >= minLogLevel) $ do
    ts <- getCurrentTime
    pushLogStr loggerSet $ formatLogStr ts stack loc s (toLogStr msg)

-- | Prepends to 'LogStr' string with "[time][log-level]". Used for
-- text logs only
formatLogStr
  :: UTCTime -> CallStack -> LogSource -> LogSeverity -> LogStr -> LogStr
formatLogStr ts _ src level msg =
  "[" <> tsStr <> "][" <> levelStr <> srcStr <> "] " <> msg <> "\n"
  where
    srcStr   = if Text.null src then mempty else "#" <> toLogStr src
    -- TODO: time-1.8 support padding: %6Q (microseconds)
    tsStr    = toLogStr $ formatTime defaultTimeLocale "%FT%T%Q" ts
    levelStr = case level of
      Debug -> "DEBUG"
      Info  -> "INFO"
      Warn  -> "WARN"
      Error -> "ERROR"

compactCallStack :: CallStack -> String
compactCallStack =
  unwords
    . List.intersperse "->"
    . map (\(f, l) -> f <> " (" <> compactLoc l <> ")")
    . getCallStack
  where compactLoc loc = srcLocFile loc <> ":" <> show (srcLocStartLine loc)

parseLogLevel :: Text -> Either String LogSeverity
parseLogLevel t = case Text.strip t of
  "debug" -> pure Debug
  "info"  -> pure Info
  "warn"  -> pure Warn
  "error" -> pure Error
  _       -> Left
    "Unreadable log level. Possible values: debug, info, warn and error"

renderLogLevel :: LogSeverity -> Text
renderLogLevel = \case
  Debug   -> "debug"
  Info    -> "info"
  Warn    -> "warn"
  Error   -> "error"
