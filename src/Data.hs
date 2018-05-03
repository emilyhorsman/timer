{-# LANGUAGE DeriveGeneric #-}
module Data (appendEntry, createStorage, parseStorage) where

import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as B'
import           Data.Csv              (DefaultOrdered, FromField,
                                        FromNamedRecord, ToField, ToNamedRecord,
                                        ToRecord, decodeByName, encode,
                                        encodeDefaultOrderedByName, parseField,
                                        toField)
import           Data.Time             (UTCTime)
import           Data.Vector           (toList)
import           GHC.Generics          (Generic)
import           System.Directory      (doesFileExist)
import           Time                  (formatAbsoluteDateTime,
                                        parseAbsoluteDateTime)


instance FromField UTCTime where
  parseField = parseAbsoluteDateTime . B.unpack


instance ToField UTCTime where
  toField = B.pack . formatAbsoluteDateTime


data Entry = Entry
  { startTime :: UTCTime
  , endTime   :: UTCTime
  , code      :: String
  }
  deriving (Generic, Show)

instance FromNamedRecord Entry
instance ToRecord Entry
instance ToNamedRecord Entry
instance DefaultOrdered Entry


createStorage :: FilePath -> IO ()
createStorage path =
  let
    empty = [] :: [Entry]
  in
    doesFileExist path >>= bool (B'.writeFile path $ encodeDefaultOrderedByName empty)
      (return ())


appendEntry :: FilePath -> (UTCTime, UTCTime, String) -> IO ()
appendEntry path (startTime, endTime, code) =
  B'.appendFile path $ encode [Entry startTime endTime code]


parseStorage :: FilePath -> IO [Entry]
parseStorage path = do
  contents <- B'.readFile path
  case decodeByName contents of
    Left err           -> putStrLn err >> return []
    Right (_, entries) -> return $ toList entries
