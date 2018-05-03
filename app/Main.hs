module Main where

import           Control.Concurrent      (forkIO, killThread, myThreadId,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Data.List               (uncons)
import           Data.Maybe              (maybe)
import           Lib                     (formatMicroseconds, getCurrentTime)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)

tick :: Int -> IO ()
tick startTime = do
  t <- getCurrentTime
  let dT = t - startTime
  putStr ("\r" ++ formatMicroseconds dT) >> threadDelay 1000000


waitForInterrupt :: IO (MVar ())
waitForInterrupt = do
  v <- newEmptyMVar
  installHandler sigINT (CatchOnce (putMVar v ())) Nothing
  return v


blockUntilInterrupt :: IO a -> IO ()
blockUntilInterrupt computation =
  waitForInterrupt >>= (\v -> computation >> takeMVar v)


getCode :: IO (Maybe String)
getCode = do
  args <- getArgs
  return $ fst <$> uncons args


main = do
  code <- getCode
  maybe (die "Usage: <code>") putStrLn code
  startTime <- getCurrentTime
  hSetBuffering stdout NoBuffering
  blockUntilInterrupt $ forkIO $ forever $ tick startTime
  putStrLn "\nExiting Main!"
