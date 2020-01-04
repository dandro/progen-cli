{-# LANGUAGE DeriveGeneric #-}

module Config
  ( GenConfig(projectDir, templatesDir, outputDirs, separator)
  , dotfileName
  , mkConfig
  ) where

import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as LazyC
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Last (Last), getLast)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
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

dotfileName :: String
dotfileName = ".progenrc"

makeDefaultConfig :: Dotfile
makeDefaultConfig = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last $ Just '.')

emptyConfigOption :: Dotfile
emptyConfigOption = Dotfile (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: String -> Dotfile
decodeConfig content = fromMaybe emptyConfigOption (decode (LazyC.pack content) :: Maybe Dotfile)

mkConfig :: String -> Maybe GenConfig
mkConfig content =
  GenConfig <$> getLast (root configOption) <*> getLast (templates configOption) <*> getLast (output configOption) <*>
  getLast (filenameSeparator configOption)
  where
    configOption = makeDefaultConfig <> decodeConfig content
