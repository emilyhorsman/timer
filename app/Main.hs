module Main where

import           Control.Concurrent      (forkIO, killThread, myThreadId,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Data                    (appendEntry, createStorage)
import           Data.List               (intercalate, uncons)
import           Data.Maybe              (maybe)
import           Data.Time               (UTCTime)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)
import           Time                    (Time, formatAbsoluteDateTime,
                                          formatDelta, getCurrentTime)

interval :: Int
interval =
  100 * 1000


tick :: Time a => a -> IO ()
tick startTime = do
  t <- getCurrentTime
  putStr ("\r" ++ formatDelta startTime t) >> threadDelay interval


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


handleCompletion :: UTCTime -> UTCTime -> Maybe String -> IO ()
handleCompletion _ _ Nothing = return ()
handleCompletion startTime finishTime (Just code) =
  let
    path = "data.csv"
  in
    createStorage path >> appendEntry path (startTime, finishTime, code)


main = do
  code <- getCode
  maybe (die "Usage: <code>") putStrLn code
  startTime <- getCurrentTime :: IO UTCTime
  hSetBuffering stdout NoBuffering
  blockUntilInterrupt $ forkIO $ forever $ tick startTime
  finishTime <- getCurrentTime :: IO UTCTime
  handleCompletion startTime finishTime code
  putStrLn "\nExiting Main!"
