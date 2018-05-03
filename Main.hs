import           Control.Concurrent      (forkIO, killThread, myThreadId,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           System.Posix.Signals    (Handler (CatchOnce), installHandler,
                                          sigINT)
import           Text.Printf             (printf)

getCurrentTime :: IO Int
getCurrentTime =
    truncate . (* 1000000) <$> getPOSIXTime


formatMicroseconds :: Int -> String
formatMicroseconds t =
  let
    total = t `div` 1000000
    (totalMinutes, seconds) = divMod total 60
    (hours, minutes) = divMod totalMinutes 60
  in
    printf "%dh%02dm%02ds" hours minutes seconds


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
