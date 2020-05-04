{-|
Module: Command
Description: Parse cli command

Module in charge of parsing the CLI command and constructing a domain
specific representation of the instruction to execute.
-}
module Command
  ( GenCommand(what, name, sub, asModule, output)
  , parserOptions
  , mkGenCommand
  , CommandError
  ) where

import           Data.Bifunctor        (first)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import           Options.Applicative   (Parser, ParserInfo, eitherReader,
                                        fullDesc, header, help, helper, info,
                                        long, option, progDesc, short,
                                        strOption, switch, value, (<**>))
import           System.Path           (RelDir, parse)
import           Utils                 (joinWith, trim)

{-|
  GenCommand represents the instructions the program will execute. It contains what we
  are going to copy, its name and content substitutions as well as whether or not to treat
  the artifact as a module.
-}
data GenCommand =
  GenCommand
    { what     :: String -- ^ What do we want to copy, or which template do we want to use.
    , name     :: String -- ^ The name that will be given to the new files.
    , sub      :: M.Map String String -- ^ Mapping of values to substitute in the templates content
    , asModule :: Bool -- ^ Should the output files be treated as a module. If it is a module it will create a directory with the name and put the files inside.
    , output   :: Maybe RelDir -- ^ Alternate output directory. Use to override config parameter
    }
  deriving (Show)

{-|
  Encompasses all possible errors in the Command module
-}
data CommandError
  = InvalidSubstitutionError String -- ^ If substitution does is not a key/value pair or it contains empty strings
  | DuplicateSubstitutionError String -- ^ If more than one value has been passed for the same substitution key.
  deriving (Show)

{-|
  This function is meant to split the str by ','
  first and then split each result by ':' and
  then trim each string in the inner List. The
  IDE helped format it this way but it's a tad hard
  to grok.
  e.g.
       if str is "$A$:a, $B$:b"
       then the result is [["A", "a"], ["B", "b"]]
-}
toListOfStr :: String -> [[String]]
toListOfStr str =
  map (map (trim . T.unpack) . T.splitOn (T.pack ":")) $
  T.splitOn (T.pack ",") $ T.pack str

hasEmptyStrings :: [String] -> Bool
hasEmptyStrings = any null

mkPair :: [String] -> Either CommandError (String, String)
mkPair pair =
  if length pair /= 2 || hasEmptyStrings pair
    then Left $
         InvalidSubstitutionError
           ("ERROR: Invalid substitution value for: " ++
            joinWith " " pair ++ ". e.g -s \"ONE:one, TWO:two\".")
    else Right (head pair, last pair)

safeInsert ::
     (String, String) -> M.Map String String -> Either CommandError (M.Map String String)
safeInsert pair m =
  if M.member (fst pair) m
    then Left $
         DuplicateSubstitutionError
           ("Duplicate substitution value: " ++
            fst pair ++ " has '" ++ (m M.! fst pair) ++ "' and '" ++ snd pair ++ "'.")
    else Right $ uncurry M.insert pair m

mkSubstitutions :: [[String]] -> Either String (M.Map String String)
mkSubstitutions listOfPairs =
  first show $
  traverse mkPair listOfPairs >>= foldr (\v acc -> acc >>= safeInsert v) (Right M.empty)

mkOptionalOutput :: String -> Either String (Maybe RelDir)
mkOptionalOutput str = (parse str :: Either String RelDir) <&> Just

-- | GenCommand factory
mkGenCommand ::
     String -- ^ What do we want to copy, or which template do we want to use.
  -> String -- ^ The name that will be given to the new files.
  -> M.Map String String -- ^ Mapping of values to substitute in the templates content.
  -> Bool -- ^ Should the output files be treated as a module. If it is a module it will create a directory with the name and put the files inside.
  -> Maybe RelDir -- ^ Alternate output directory. Use to override config parameter
  -> GenCommand
mkGenCommand = GenCommand

mkGenCommandParser :: Parser GenCommand
mkGenCommandParser =
  GenCommand <$>
  strOption (long "what" <> short 'w' <> help "What do you want to generate") <*>
  strOption (long "name" <> short 'n' <> help "Name of file you're generating") <*>
  option
    (eitherReader $ mkSubstitutions . toListOfStr)
    (long "substitution" <> short 's' <> value M.empty <>
     help
       "Values to substitue in the template. The format is '-s \"$KEY_ONE$:value-one, $KEY_TWO$:value-two.\"'") <*>
  switch
    (long "as-module" <> short 'm' <>
     help "Treat as module. This will create a directory in the output location") <*>
  option
    (eitherReader mkOptionalOutput)
    (long "output" <> short 'o' <> value Nothing <>
     help
       "Override the output from the configuration. The value must be a valid relative path.")

{-|
  This is the configuration given to Opt-Parser Applicative. This is what will be used
  in the cli program, the messages and banners displayed. It is a parser for GenCommand.
-}
parserOptions :: ParserInfo GenCommand
parserOptions =
  info
    (mkGenCommandParser <**> helper)
    (fullDesc <> progDesc "Let's make your life easier" <>
     header "Welcome to Progen Cli - Generate whatever you want for free")
