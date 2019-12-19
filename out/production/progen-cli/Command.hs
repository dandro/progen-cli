{-# LANGUAGE LambdaCase #-}

module Command
  ( GenCommand(GenCommand, what, name, sub)
  , parserOptions
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map          as M
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import           Options.Applicative   (Parser, eitherReader, fullDesc, header,
                                        help, helper, info, long, option,
                                        progDesc, short, strOption, value,
                                        (<**>))
import           Utils                 (joinWith, trim)

data GenCommand =
  GenCommand
    { what :: String
    , name :: String
    , sub  :: M.Map String String
    }
  deriving (Show)

toListOfStr :: String -> [[String]]
-- This function is meant to split the str by ','
-- first and then split each result by ':' and
-- then trim each string in the inner List. The
-- IDE helped format it this way but It's a tad hard
-- to grok.
-- e.g.
--      if str is "$A$:a, $B$:b"
--      then the result is [["A", "a"], ["B", "b"]]
toListOfStr str =
  map (map (trim . T.unpack) . T.splitOn (T.pack ":")) $
  T.splitOn (T.pack ",") $ T.pack str

hasEmptyStrings :: [String] -> Bool
hasEmptyStrings = any null

mkPair :: [String] -> Either String (String, String)
mkPair pair =
  if length pair /= 2 || hasEmptyStrings pair
    then Left $ "Invalid substitution value for: " ++ joinWith " " pair
    else Right (head pair, last pair)

safeInsert ::
     (String, String) -> M.Map String String -> Either String (M.Map String String)
safeInsert pair m =
  if M.member (fst pair) m
    then Left $
         "Duplicate substitution value: " ++
         fst pair ++ " has '" ++ (m M.! fst pair) ++ "' and '" ++ snd pair ++ "'."
    else Right $ uncurry M.insert pair m

mkSubstitutions :: [[String]] -> Either String (M.Map String String)
mkSubstitutions listOfPairs =
  traverse mkPair listOfPairs >>= foldr (\v acc -> acc >>= safeInsert v) (Right M.empty)

makeGenCommand :: Parser GenCommand
makeGenCommand =
  GenCommand <$>
  strOption (long "what" <> short 'w' <> help "What do you want to generate") <*>
  strOption (long "name" <> short 'n' <> help "Name of file you're generating") <*>
  option
    (eitherReader $ mkSubstitutions . toListOfStr)
    (long "substitution" <> short 's' <> value M.empty <>
     help
       "Values to substitue in the template. The format is '-s \"$KEY_ONE$:value-one,$KEY_TWO$:value-two.\"'")

parserOptions =
  info
    (makeGenCommand <**> helper)
    (fullDesc <> progDesc "Let's make your life easier" <>
     header "Welcome to Progen Cli - Generate whatever you want for free")
