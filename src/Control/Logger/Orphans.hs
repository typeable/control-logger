{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Logger.Orphans
  (
  ) where

import           Control.EnvT
import           Control.Has
import           Control.Logger.Internal
import           Control.Monad.Logger
import qualified Data.Text as T
import           GHC.Stack


instance (Has Logger r, MonadIO m)
  => MonadLogger (EnvT r m) where
  monadLoggerLog loc _ level msg =
    let ?callStack = freezeCallStack (locToCallStack loc)
    in withFrozenCallStack
      $ logMsg
        (mlLevelToLogSeverity level)
        (T.pack . show . toLogStr $ msg)

locToCallStack :: Loc -> CallStack
locToCallStack Loc {..} = fromCallSiteList
  [ ( ""
    , SrcLoc
      { srcLocFile      = loc_filename
      , srcLocPackage   = loc_package
      , srcLocModule    = loc_module
      , srcLocStartLine = fst loc_start
      , srcLocStartCol  = snd loc_start
      , srcLocEndLine   = fst loc_end
      , srcLocEndCol    = snd loc_end
      }
    )
  ]

mlLevelToLogSeverity :: LogLevel -> LogSeverity
mlLevelToLogSeverity = \case
  LevelDebug   -> Debug
  LevelInfo    -> Info
  LevelWarn    -> Warn
  LevelError   -> Error
  LevelOther _ -> Error
