{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Config
Description: Application configuration

Contains metadata for the application as well as values needed for Progen to run.
-}
module Config
  ( GenConfig(templatesDir, outputDirs, separator)
  , mkConfig
  , mkDotfile
  , dotfileName
  , handleConfigResult
  , Dotfile
  , ConfigError
  ) where

import           Data.Aeson                 (decode)
import           Data.Aeson.Types           (FromJSON, Parser, parseJSON,
                                             withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           System.Path                (AbsDir, Dir, RelDir, absDir,
                                             relDir)
import           System.Path.Generic        ((</>))
import           Utils                      (upperCase)

{-|
  It represents the data coming from the configuration file. More data can be needed but may not be configurable.
  The Dotfile is used to create a GenConfig which will be used by the application.
-}
data Dotfile =
  Dotfile
    { templates         :: Last RelDir -- ^ Templates directory path relative to the root.
    , output            :: Last (M.Map String RelDir) -- ^ Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    , filenameSeparator :: Last (Maybe Char) -- ^ Character used to separate the template filename and make use of suffix.
    }
  deriving (Generic, Show)

{-|
  Encompasses all possible errors in the Config module
-}
newtype ConfigError =
  CouldNotMakeConfig String -- ^ Cannot create a valid config from all inputs
  deriving (Show)

{-|
  Transform maybe config to an Either of the correct Domain error or the valid Config
-}
handleConfigResult :: Maybe GenConfig -> Either ConfigError GenConfig
handleConfigResult (Just config) = Right config
handleConfigResult Nothing = Left $ CouldNotMakeConfig "ERROR: Coud not make a valid configuration."

mkDirPath :: (String -> Dir os) -> T.Text -> Last (Dir os)
mkDirPath toPath = Last . Just . toPath . T.unpack

mkOutputDirs :: M.Map String String -> Last (M.Map String RelDir)
mkOutputDirs m = Last $ Just (M.foldrWithKey (\k v result -> M.insert k (relDir v) result) M.empty m)

instance FromJSON Dotfile where
  parseJSON =
    withObject
      "Dotfile"
      (\o -> do
         templates' <- mkDirPath relDir <$> (o .: "templates" :: Parser T.Text)
         output' <- mkOutputDirs <$> (o .: "output" :: Parser (M.Map String String))
         filenameSeparator' <-  Last <$> o .:? "filenameSeparator"
         return $ mkDotfile templates' output' filenameSeparator')

instance Semigroup Dotfile where
  a <> b =
    Dotfile
      { templates = templates a <> templates b
      , output = output a <> output b
      , filenameSeparator = filenameSeparator a <> filenameSeparator b
      }

{-|
  This is the configuration used by the application wtih all the metadata needed to generate code.
-}
data GenConfig =
  GenConfig
    { templatesDir :: RelDir -- ^ Templates directory path relative to the root.
    , outputDirs   :: M.Map String RelDir -- ^ Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    , separator    :: Maybe Char -- ^ Character used to separate the template filename and make use of suffix.
    }
  deriving (Show, Eq)

-- | Name for the dotfile where the configuration will be read from
dotfileName :: String
dotfileName = ".progenrc"

makeDefaultConfig :: Dotfile
makeDefaultConfig = Dotfile (Last Nothing) (Last Nothing) (Last $ Just Nothing)

emptyConfigOption :: Dotfile
emptyConfigOption = Dotfile (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: String -> Dotfile
decodeConfig content = fromMaybe emptyConfigOption (decode (LazyC.pack content) :: Maybe Dotfile)

{-|
  Factory function for constructing a Dotfile
-}
mkDotfile ::
     Last RelDir -- ^ Templates directory path relative to the root.
  -> Last (M.Map String RelDir) -- ^ Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
  -> Last (Maybe Char) -- ^ Character used to separate the template filename and make use of suffix.
  -> Dotfile
mkDotfile = Dotfile

{-|
  Factory to make a GenConfig from a JSONString. It will attempt to construct a Dotfile from its values
  and then combine it with a default config. The result will be the GenConfig that will be returned.

  >>> mkConfig "{ \"root\": \"/dummy/project\", \"templates\": \".progen/templates\", \"filenameSeparator\": \".\", \"output\": { \"component\": \"./components\" }}"
-}
mkConfig :: Dotfile -> String -> Maybe GenConfig
mkConfig dotfile content =
  GenConfig <$> getLast (templates configOption) <*> getLast (output configOption) <*>
  getLast (filenameSeparator configOption)
  where
    configOption = makeDefaultConfig <> decodeConfig content <> dotfile
