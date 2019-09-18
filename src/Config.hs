module Config
  ( GenConfig(projectDir, language)
  , Language(..)
  , getConfig
  ) where

import           Data.Functor     ((<&>))
import           Data.Maybe       (Maybe (Just))
import           Data.Semigroup   (Last (Last), getLast)
import           System.Directory (findFile, getCurrentDirectory)

data Language
  = JavaScript
  | Flow
  | TypeScript

data GenConfigOption =
  GenConfigOption
    { projectDirOption :: Last (Maybe String)
    , languageOption   :: Last (Maybe Language)
    }

instance Semigroup GenConfigOption where
  a <> b =
    GenConfigOption
      { projectDirOption = projectDirOption a <> projectDirOption b
      , languageOption = languageOption a <> languageOption b
      }

data GenConfig =
  GenConfig
    { projectDir :: String
    , language   :: Language
    }

dotfile :: String
dotfile = ".progenrc"

makeDefaultConfig :: GenConfigOption
makeDefaultConfig =
  GenConfigOption (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/") (Last $ Just Flow)

findConfig :: IO GenConfigOption
findConfig =
  getCurrentDirectory >>=
  (\pwd -> findFile [pwd] dotfile) <&>
  (\f -> GenConfigOption (Last $ Just "/Users/daniel.martinez/Documents/js/dummy-project/") (Last $ Just JavaScript))

getConfig :: IO (Maybe GenConfig)
getConfig =
  (\(GenConfigOption projectDir' language') -> GenConfig <$> getLast projectDir' <*> getLast language') .
  (<> makeDefaultConfig) <$>
  findConfig
