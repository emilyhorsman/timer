module Time
    ( getCurrentTime
    , Time
    , formatAbsoluteDateTime
    , parseAbsoluteDateTime
    , formatDelta
    ) where

import           Control.Monad.Fail (MonadFail)
import qualified Data.Time          as DT
import           Text.Printf        (printf)


class Time a where
  getCurrentTime :: IO a
  formatDelta :: a -> a -> String
  formatAbsoluteDateTime :: a -> String
  parseAbsoluteDateTime :: MonadFail m => String -> m a


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

  formatAbsoluteDateTime =
    DT.formatTime DT.defaultTimeLocale $ DT.iso8601DateFormat $ Just "%H:%M:%S"

  parseAbsoluteDateTime =
    DT.parseTimeM False DT.defaultTimeLocale $ DT.iso8601DateFormat $ Just "%H:%M:%S"
