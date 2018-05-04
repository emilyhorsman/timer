module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever)
import           Data               (appendEntry, createStorage)
import           Data.Time          (UTCTime)
import           Interrupt          (blockUntilInterrupt)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.IO          (BufferMode (NoBuffering), hSetBuffering,
                                     stdout)
import           Time               (Time, formatAbsoluteDateTime, formatDelta,
                                     getCurrentTime)

tickInterval :: Int
tickInterval =
  100 * 1000


tick :: Time a => a -> IO ()
tick startTime = do
  t <- getCurrentTime
  putStr ("\r" ++ formatDelta startTime t) >> threadDelay tickInterval


handleCompletion :: UTCTime -> UTCTime -> String -> IO ()
handleCompletion startTime finishTime code =
  let
    path = "data.csv"
  in
    createStorage path >> appendEntry path (startTime, finishTime, code)


run :: [String] -> IO ()
run [code] = do
  putStrLn code
  startTime <- getCurrentTime :: IO UTCTime
  hSetBuffering stdout NoBuffering
  blockUntilInterrupt $ forkIO $ forever $ tick startTime
  finishTime <- getCurrentTime :: IO UTCTime
  handleCompletion startTime finishTime code
  putStrLn "\nExiting Main!"
run _ =
  die "Usage: <code>"


main =
  getArgs >>= run
