module Utils
  ( joinWith
  , upperCase
  , trim
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

joinWith :: String -> [String] -> String
joinWith separator strings =
  BS.unpack $
  BS.intercalate (BS.pack separator) (filter (not . BS.null) (BS.pack <$> strings))

trim :: String -> String
trim str = T.unpack $ T.strip $ T.pack str

upperCase :: String -> String
upperCase = T.unpack . T.toUpper . T.pack
