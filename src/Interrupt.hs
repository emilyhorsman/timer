module Interrupt (blockUntilInterrupt) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)

waitForInterrupt :: IO (MVar ())
waitForInterrupt = do
  v <- newEmptyMVar
  installHandler sigINT (CatchOnce (putMVar v ())) Nothing
  return v


blockUntilInterrupt :: IO a -> IO ()
blockUntilInterrupt computation =
  waitForInterrupt >>= (\v -> computation >> takeMVar v)
