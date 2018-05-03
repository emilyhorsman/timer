module Main where

import           Control.Concurrent      (forkIO, killThread, myThreadId,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Data.List               (uncons, intercalate)
import           Data.Maybe              (maybe)
import           Data.Time               (UTCTime)
import           Time                     (Time, formatAbsoluteDate,
                                          formatAbsoluteTime, formatDelta,
                                          getCurrentTime)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)


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


handleCompletion :: Time a => Maybe String -> a -> a -> IO ()
handleCompletion Nothing _ _ = return ()
handleCompletion (Just code) startTime finishTime =
  let
    columns =
      [ formatAbsoluteDate startTime
      , code
      , formatAbsoluteTime startTime
      , formatAbsoluteTime finishTime
      ]
  in
    putStrLn $ "\n" ++ intercalate "," columns


main = do
  code <- getCode
  maybe (die "Usage: <code>") putStrLn code
  startTime <- getCurrentTime :: IO UTCTime
  hSetBuffering stdout NoBuffering
  blockUntilInterrupt $ forkIO $ forever $ tick startTime
  finishTime <- getCurrentTime :: IO UTCTime
  handleCompletion code startTime finishTime
  putStrLn "\nExiting Main!"
