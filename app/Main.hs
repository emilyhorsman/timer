module Main where

import           Control.Concurrent      (forkIO, killThread, myThreadId,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Lib                     (formatMicroseconds, getCurrentTime)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)

printCurrentTime :: Int -> IO ()
printCurrentTime startTime = do
  t <- getCurrentTime
  let dT = t - startTime
  threadDelay 1000000 >> putStr ("\r" ++ formatMicroseconds dT)


timer :: IO ()
timer =
  getCurrentTime >>= (\t -> forever $ printCurrentTime t)


waitForInterrupt :: IO (MVar ())
waitForInterrupt = do
  v <- newEmptyMVar
  installHandler sigINT (CatchOnce (putMVar v ())) Nothing
  return v


blockUntilInterrupt :: IO a -> IO ()
blockUntilInterrupt computation =
  waitForInterrupt >>= (\v -> computation >> takeMVar v)


main = do
  blockUntilInterrupt $ forkIO timer
  putStrLn "\nExiting Main!"
