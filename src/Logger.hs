module Logger where

import Control.Concurrent.STM (TQueue, atomically, readTQueue)
import Control.Monad
import qualified Data.Text as T

logWriter :: TQueue T.Text -> (T.Text -> IO ()) -> IO ()
logWriter queue logfunc = forever $ do
  msg <- atomically $ readTQueue queue
  logfunc msg
