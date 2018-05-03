module Data where

import qualified Data.ByteString.Char8 as B
import           Data.Csv              (FromField, ToField, parseField, toField)
import           Data.Time             (UTCTime)
import           Time                  (formatAbsoluteDateTime,
                                        parseAbsoluteDateTime)


instance FromField UTCTime where
  parseField = parseAbsoluteDateTime . B.unpack


instance ToField UTCTime where
  toField = B.pack . formatAbsoluteDateTime
