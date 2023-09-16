{-# LANGUAGE OverloadedStrings #-}

module Supervisor (supervisor) where

import Control.Concurrent.Async
import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import Control.Exception.Safe (MonadMask (mask), bracket)
import qualified Data.Text as T

supervisor :: TQueue T.Text -> IO () -> IO ()
supervisor queue action = do
  atomically $ writeTQueue queue "SUPERVISOR: launch worker"
  loop
  where
    loop = do
      result <- mask $ \restore -> do
        result <-
          bracket
            (async $ restore action)
            cancel
            waitCatch
        case result of
          Left e -> atomically $ writeTQueue queue $ "SUPERVISOR: catch exception: " <> T.pack (show e)
          Right _ -> return ()
        return result
      case result of
        Left _e -> do
          atomically $ writeTQueue queue "SUPERVISOR: re-launch worker"
          loop
        Right _ -> return ()
