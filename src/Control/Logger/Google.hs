module Control.Logger.Google
  ( toGoogleLogger
  ) where

import           Control.Logger.Internal
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Network.Google as G


toGoogleLogger :: Logger -> G.Logger
toGoogleLogger logger level s = case logLevelToSeverity level of
  Nothing -> return ()
  Just sev  -> logMsgWith logger sev
    $ T.decodeUtf8With (T.replace '?') . BS.toStrict . B.toLazyByteString $ s

logLevelToSeverity :: G.LogLevel -> Maybe LogSeverity
logLevelToSeverity G.Info  = Just Info
logLevelToSeverity G.Debug = Just Debug
logLevelToSeverity G.Error = Just Error
logLevelToSeverity G.Trace = Nothing
