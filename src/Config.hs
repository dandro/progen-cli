{-# LANGUAGE DeriveGeneric #-}

module Config
  ( GenConfig(projectDir, language, templatesDir, outputDirs,
          separator)
  , Language(..)
  , getConfig
  ) where

import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           System.Directory           (findFile, getCurrentDirectory)
import           Utils                      (upperCase)

data Language
  = JavaScript
  | Flow
  | TypeScript
  deriving (Generic, Show)

instance FromJSON Language

data GenConfigOption =
  GenConfigOption
    { projectDirOption   :: Last String
    , languageOption     :: Last Language
    , templatesDirOption :: Last String
    , outputDirsOption   :: Last (M.Map String String)
    , separatorOption    :: Last Char
    }
  deriving (Generic, Show)

instance FromJSON GenConfigOption

instance Semigroup GenConfigOption where
  a <> b =
    GenConfigOption
      { projectDirOption = projectDirOption a <> projectDirOption b
      , languageOption = languageOption a <> languageOption b
      , templatesDirOption = templatesDirOption a <> templatesDirOption b
      , outputDirsOption = outputDirsOption a <> outputDirsOption b
      , separatorOption = separatorOption a <> separatorOption b
      }

data GenConfig =
  GenConfig
    { projectDir   :: String
    , language     :: Language
    , templatesDir :: String
    , outputDirs   :: M.Map String String
    , separator    :: Char
    }
  deriving (Show)

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: GenConfigOption
makeDefaultConfig = GenConfigOption (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last $ Just '.')

emptyConfigOption :: GenConfigOption
emptyConfigOption = GenConfigOption (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: Maybe String -> GenConfigOption
decodeConfig content = do
  let result = content >>= (\c -> decode (LazyC.pack c) :: Maybe GenConfigOption)
  fromMaybe emptyConfigOption result

mkConfig :: GenConfigOption -> Maybe GenConfig
mkConfig configOption =
  GenConfig <$> getLast (projectDirOption configOption) <*> getLast (languageOption configOption) <*>
  getLast (templatesDirOption configOption) <*>
  getLast (outputDirsOption configOption) <*>
  getLast (separatorOption configOption)

getConfig :: IO (Either String GenConfig)
getConfig = do
  pwd <- getCurrentDirectory
  content <- findFile [pwd] dotfile >>= traverse readFile
  let result = mkConfig $ makeDefaultConfig <> decodeConfig content
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: Coud not make a valid configuration."
