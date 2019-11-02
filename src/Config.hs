{-# LANGUAGE LambdaCase #-}

module Config
  ( GenConfig(projectDir, language, templatesDir)
  , Language(..)
  , getConfig
  ) where

import qualified Data.ByteString.Char8  as C
import           Data.Functor           ((<&>))
import           Data.JsonStream.Parser (parseByteString, value, (.:))
import           Data.Monoid            (Last (Last), getLast)
import qualified Data.Text              as T
import           System.Directory       (findFile, getCurrentDirectory)
import           Utils                  (upperCase)

data Language
  = JavaScript
  | Flow
  | TypeScript

data GenConfigOption =
  GenConfigOption
    { projectDirOption   :: Last String
    , languageOption     :: Last Language
    , templatesDirOption :: Last String
    }

instance Semigroup GenConfigOption where
  a <> b =
    GenConfigOption
      { projectDirOption = projectDirOption a <> projectDirOption b
      , languageOption = languageOption a <> languageOption b
      , templatesDirOption = templatesDirOption a <> templatesDirOption b
      }

data GenConfig =
  GenConfig
    { projectDir   :: String
    , language     :: Language
    , templatesDir :: String
    }

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: GenConfigOption
makeDefaultConfig =
  GenConfigOption
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/")
    (Last $ Just JavaScript)
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/.progenrc/templates/")

safeHead :: [String] -> Maybe String
safeHead (h:_) = Just h
safeHead _     = Nothing

safeGetProjectDirValue :: String -> String -> Maybe String
safeGetProjectDirValue key content =
  safeHead (parseByteString (T.pack key .: value) (C.pack content) :: [String])

toLang :: String -> Maybe Language
toLang str =
  case upperCase str of
    "JAVASCRIPT" -> Just JavaScript
    "FLOW"       -> Just Flow
    "TYPESCRIPT" -> Just TypeScript
    _            -> Nothing

findConfig :: IO GenConfigOption
findConfig =
  getCurrentDirectory >>=
  (\pwd -> findFile [pwd] dotfile >>= traverse readFile) <&>
  (\case
     Just content ->
       GenConfigOption
         (Last $ safeGetProjectDirValue "projectDir" content)
         (Last $ toLang =<< safeGetProjectDirValue "language" content)
         (Last $ safeGetProjectDirValue "templatesDir" content)
     Nothing -> GenConfigOption (Last Nothing) (Last Nothing) (Last Nothing))

getConfig :: IO (Either String GenConfig)
getConfig = do
  conf <- findConfig
  let combined = makeDefaultConfig <> conf
  let result =
        GenConfig <$> getLast (projectDirOption combined) <*>
        getLast (languageOption combined) <*>
        getLast (templatesDirOption combined)
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: There is valid no configuration."
