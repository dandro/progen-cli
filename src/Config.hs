{-# LANGUAGE DeriveGeneric #-}

module Config
  ( GenConfig(projectDir, templatesDir, outputDirs, separator)
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

data Dotfile =
  Dotfile
    { root              :: Last String
    , templates         :: Last String
    , output            :: Last (M.Map String String)
    , filenameSeparator :: Last Char
    }
  deriving (Generic, Show)

instance FromJSON Dotfile

instance Semigroup Dotfile where
  a <> b =
    Dotfile
      { root = root a <> root b
      , templates = templates a <> templates b
      , output = output a <> output b
      , filenameSeparator = filenameSeparator a <> filenameSeparator b
      }

data GenConfig =
  GenConfig
    { projectDir   :: String
    , templatesDir :: String
    , outputDirs   :: M.Map String String
    , separator    :: Char
    }
  deriving (Show)

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: Dotfile
makeDefaultConfig = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last $ Just '.')

emptyConfigOption :: Dotfile
emptyConfigOption = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: Maybe String -> Dotfile
decodeConfig content = do
  let result = content >>= (\c -> decode (LazyC.pack c) :: Maybe Dotfile)
  fromMaybe emptyConfigOption result

mkConfig :: Dotfile -> Maybe GenConfig
mkConfig configOption =
  GenConfig <$> getLast (root configOption) <*> getLast (templates configOption) <*> getLast (output configOption) <*>
  getLast (filenameSeparator configOption)

getConfig :: IO (Either String GenConfig)
getConfig = do
  pwd <- getCurrentDirectory
  content <- findFile [pwd] dotfile >>= traverse readFile
  let result = mkConfig $ makeDefaultConfig <> decodeConfig content
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: Coud not make a valid configuration."
