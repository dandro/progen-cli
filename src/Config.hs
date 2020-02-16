{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Config
Description: Application configuration

Contains metadata for the application as well as values needed for Progen to run.
-}
module Config
  ( GenConfig(projectDir, templatesDir, outputDirs, separator)
  , mkConfig
  , dotfileName
  ) where

import           Data.Aeson                 (decode)
import           Data.Aeson.Types           (FromJSON, Parser, parseJSON,
                                             withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           System.Path                (AbsDir, RelDir, absDir, relDir)
import           Utils                      (upperCase)

{-|
  It represents the data coming from the configuration file. More data can be needed but may not be configurable.
  The Dotfile is used to create a GenConfig which will be used by the application.
-}
data Dotfile =
  Dotfile
    { root              :: Last AbsDir -- ^ Absolute directory path pointing to the root of the project using Progen.
    , templates         :: Last AbsDir -- ^ Where are the templates. At the moment it can be out side of the root.
    , output            :: Last (M.Map String RelDir) -- ^ Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    , filenameSeparator :: Last Char -- ^ Character used to separate the template filename and make use of suffix.
    }
  deriving (Generic, Show)

mkAbsDir :: T.Text -> Last AbsDir
mkAbsDir = Last . Just . absDir . T.unpack

mkOutputDirs :: M.Map String String -> Last (M.Map String RelDir)
mkOutputDirs m = Last $ Just (M.foldrWithKey (\k v result -> M.insert k (relDir v) result) M.empty m)

instance FromJSON Dotfile where
  parseJSON =
    withObject
      "Dotfile"
      (\o -> do
         root_ <- mkAbsDir <$> (o .: "root" :: Parser T.Text)
         templates_ <- mkAbsDir <$> (o .: "templates" :: Parser T.Text)
         output_ <- mkOutputDirs <$> (o .: "output" :: Parser (M.Map String String))
         filenameSeparator_ <- o .: "filenameSeparator"
         return $ Dotfile root_ templates_ output_ filenameSeparator_)

instance Semigroup Dotfile where
  a <> b =
    Dotfile
      { root = root a <> root b
      , templates = templates a <> templates b
      , output = output a <> output b
      , filenameSeparator = filenameSeparator a <> filenameSeparator b
      }

{-|
  This is the configuration used by the application wtih all the metadata needed to generate code.
-}
data GenConfig =
  GenConfig
    { projectDir   :: AbsDir -- ^ Absolute directory path pointing to the root of the project using Progen.
    , templatesDir :: AbsDir -- ^ Where are the templates. At the moment it can be out side of the root.
    , outputDirs   :: M.Map String RelDir -- ^ Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    , separator    :: Char -- ^ Character used to separate the template filename and make use of suffix.
    }
  deriving (Show, Eq)

-- | Name for the dotfile where the configuration will be read from
dotfileName :: String
dotfileName = ".progenrc"

makeDefaultConfig :: Dotfile
makeDefaultConfig = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last $ Just '.')

emptyConfigOption :: Dotfile
emptyConfigOption = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: String -> Dotfile
decodeConfig content = fromMaybe emptyConfigOption (decode (LazyC.pack content) :: Maybe Dotfile)

{-|
  Factory to make a GenConfig from a JSONString. It will attempt to construct a Dotfile from its values
  and then combine it with a default config. The result will be the GenConfig that will be returned.

  >>> mkConfig "{ \"root\": \"/dummy/project\", \"templates\": \"/dummy/project/.progen/templates\", \"filenameSeparator\": \".\", \"output\": { \"component\": \"./components\" }}"
-}
mkConfig :: String -> Maybe GenConfig
mkConfig content =
  GenConfig <$> getLast (root configOption) <*> getLast (templates configOption) <*> getLast (output configOption) <*>
  getLast (filenameSeparator configOption)
  where
    configOption = makeDefaultConfig <> decodeConfig content
