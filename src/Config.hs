{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( GenConfig(projectDir, templatesDir, outputDirs, separator)
  , dotfileName
  , mkConfig
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

data Dotfile =
  Dotfile
    { root              :: Last AbsDir
    , templates         :: Last AbsDir
    , output            :: Last (M.Map String RelDir)
    , filenameSeparator :: Last Char
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

data GenConfig =
  GenConfig
    { projectDir   :: AbsDir
    , templatesDir :: AbsDir
    , outputDirs   :: M.Map String RelDir
    , separator    :: Char
    }
  deriving (Show, Eq)

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
