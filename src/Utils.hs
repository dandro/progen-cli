{-|
Module: Utils
Description: Utility functions

Collection of generic utility functions.
-}
module Utils
  ( joinWith
  , upperCase
  , trim
  , pathStartsWith
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

-- | Join a collection of strings with a separator
joinWith ::
     String -- ^ Separator to join strings with
  -> [String] -- ^ Collections of strings to join
  -> String
joinWith separator strings =
  BS.unpack $ BS.intercalate (BS.pack separator) (filter (not . BS.null) (BS.pack <$> strings))

-- | Trim start and end of word. It uses Text.strip under the hood.
trim :: String -> String
trim str = T.unpack $ T.strip $ T.pack str

-- | Transform word to upper case
upperCase :: String -> String
upperCase = T.unpack . T.toUpper . T.pack

-- | Check if string starts with another string
pathStartsWith :: String -> FilePath -> Bool
pathStartsWith str path = BS.isPrefixOf (BS.pack str) (BS.pack path)
