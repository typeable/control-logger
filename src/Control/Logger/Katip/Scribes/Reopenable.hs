module Control.Logger.Katip.Scribes.Reopenable where

import Katip
import Katip.Scribes.Handle.FileOwner

-- | Utility function to create scribe using the 'FileOwner'
-- inside. Returns new scribe and action to reopen the log
-- file. Reopening is performed asyncronously.
reopenableFileLog
  :: FilePath
  -> (forall a. LogItem a => ItemFormatter a)
  -> PermitFunc
  -> Verbosity
  -> IO (Scribe, IO ())
reopenableFileLog fPath formatter permitF verbosity = do
  owner <- newFileOwner fPath defaultFileOwnerSettings
  s <- mkFileOwnerScribeWithFormatter formatter (ColorLog False)
    owner permitF verbosity
  return (s, fileOwnerControl owner ReopenMsg)
