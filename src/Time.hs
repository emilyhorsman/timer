module Time
    ( getCurrentTime
    , Time
    , formatAbsoluteDate
    , formatAbsoluteTime
    , formatDelta
    ) where

import qualified Data.Time   as DT
import           Text.Printf (printf)


class Time a where
  getCurrentTime :: IO a
  formatDelta :: a -> a -> String
  formatAbsoluteDate :: a -> String
  formatAbsoluteTime :: a -> String


instance Time DT.UTCTime where
  getCurrentTime =
    DT.getCurrentTime

  formatDelta start t =
    let
      diff = DT.diffUTCTime t start
      totalSeconds = floor diff :: Int
      (totalMinutes, seconds) = divMod totalSeconds 60
      (hours, minutes) = divMod totalMinutes 60
    in
      printf "%dh%02dm%02ds" hours minutes seconds

  formatAbsoluteDate =
    DT.formatTime DT.defaultTimeLocale "%Y-%m-%d"

  formatAbsoluteTime =
    DT.formatTime DT.defaultTimeLocale "%H:%M:%S"
