{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( GenConfig(projectDir, language, templatesDir)
  , Language(..)
  , getConfig
  ) where

import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as M
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           System.Directory           (findFile, getCurrentDirectory)
import           Utils                      (upperCase)
import GHC.Generics (Generic)
import Data.Aeson (decode, FromJSON)

data Language
  = JavaScript
  | Flow
  | TypeScript deriving (Generic, Show)

instance FromJSON Language

data GenConfigOption =
  GenConfigOption
    { projectDirOption   :: Last String
    , languageOption     :: Last Language
    , templatesDirOption :: Last String
    , outputDirsOption   :: Last (M.Map String String)
    } deriving (Generic, Show)

instance FromJSON GenConfigOption

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
    , outputDirs   :: M.Map String String
    } deriving (Show)

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: GenConfigOption
makeDefaultConfig =
  GenConfigOption
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/")
    (Last $ Just JavaScript)
    (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/.progenrc/templates/")
    (Last $ Just M.empty)

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

decodeConfig :: Maybe String -> GenConfigOption
decodeConfig content = do
    let result = content >>= (\c -> decode (LazyC.pack c) :: Maybe GenConfigOption)
    case result of
      Just conf -> conf
      Nothing -> emptyConfigOption

mkConfig :: GenConfigOption -> Maybe GenConfig
mkConfig configOption = 
  GenConfig <$> getLast (projectDirOption configOption) <*>
          getLast (languageOption configOption) <*>
          getLast (templatesDirOption configOption) <*>
          getLast (outputDirsOption configOption)

getConfig :: IO (Either String GenConfig)
getConfig = do
  pwd <- getCurrentDirectory
  content <- findFile [pwd] dotfile >>= traverse readFile
  let conf = decodeConfig content
  let combined = makeDefaultConfig <> conf
  let result = mkConfig combined
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: There is valid no configuration."
