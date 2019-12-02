{-# LANGUAGE LambdaCase #-}

module Config
  ( GenConfig(projectDir, language, templatesDir)
  , Language(..)
  , getConfig
  ) where

import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.HashMap               as M
import           Data.JsonStream.Parser     (decode, parseByteString, value,
                                             (.:))
import qualified Data.Map.Strict            as MM
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           System.Directory           (findFile, getCurrentDirectory)
import           Utils                      (upperCase)

data Language
  = JavaScript
  | Flow
  | TypeScript

data GenConfigOption =
  GenConfigOption
    { projectDirOption   :: Last String
    , languageOption     :: Last Language
    , templatesDirOption :: Last String
    , outputDirsOption   :: Last (MM.Map String String)
    }

instance Semigroup GenConfigOption where
  a <> b =
    GenConfigOption
      { projectDirOption = projectDirOption a <> projectDirOption b
      , languageOption = languageOption a <> languageOption b
      , templatesDirOption = templatesDirOption a <> templatesDirOption b
      , outputDirsOption = outputDirsOption a <> outputDirsOption b
      }

data GenConfig =
  GenConfig
    { projectDir   :: String
    , language     :: Language
    , templatesDir :: String
    , outputDirs   :: MM.Map String String
    }

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: GenConfigOption
makeDefaultConfig =
  GenConfigOption
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/")
    (Last $ Just JavaScript)
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/.progenrc/templates/")
    (Last $ Just MM.empty)

safeHead :: [String] -> Maybe String
safeHead (h:_) = Just h
safeHead _     = Nothing

safeGetStr :: String -> String -> Maybe String
safeGetStr key content =
  safeHead (parseByteString (T.pack key .: value) (C.pack content) :: [String])

toLang :: String -> Maybe Language
toLang str =
  case upperCase str of
    "JAVASCRIPT" -> Just JavaScript
    "FLOW"       -> Just Flow
    "TYPESCRIPT" -> Just TypeScript
    _            -> Nothing

emptyConfigOption :: GenConfigOption
emptyConfigOption =
  GenConfigOption (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

safeGetMap :: String -> String -> Maybe (MM.Map String String)
safeGetMap key content =
  safeGetStr key content >>= (\x -> decode (LazyC.pack x) :: Maybe (MM.Map String String))

findConfig :: IO GenConfigOption
findConfig =
  getCurrentDirectory >>=
  (\pwd -> findFile [pwd] dotfile >>= traverse readFile) <&>
  (\case
     Just content ->
       GenConfigOption
         (Last $ safeGetStr "projectDir" content)
         (Last $ toLang =<< safeGetStr "language" content)
         (Last $ safeGetStr "templatesDir" content)
         (Last $ safeGetMap "outputDirs" content)
     Nothing -> emptyConfigOption)

getConfig :: IO (Either String GenConfig)
getConfig = do
  conf <- findConfig
  let combined = makeDefaultConfig <> conf
  let result =
        GenConfig <$> getLast (projectDirOption combined) <*>
        getLast (languageOption combined) <*>
        getLast (templatesDirOption combined) <*>
        getLast (outputDirsOption combined)
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: There is valid no configuration."
