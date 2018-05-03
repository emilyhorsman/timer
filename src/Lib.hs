module Lib
    ( getCurrentTime
    , formatMicroseconds
    ) where

import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Text.Printf           (printf)

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
