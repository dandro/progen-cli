{-# LANGUAGE LambdaCase #-}

module Template
  ( Template(name, suffix, content, extension, sourcePath)
  , mkTemplate
  , getTemplateFiles
  ) where

import qualified Command               as Comm
import           Config                (GenConfig, separator, templatesDir)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor          ((<&>))
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.Path.Generic   (absDir, relFile, toString, (</>))
import           Utils                 (joinWith, pathStartsWith)

data Template =
  Template
    { name       :: String
    , suffix     :: String
    , content    :: String
    , extension  :: String
    , sourcePath :: String
    }
  deriving (Show, Eq)

toTemplate :: Char -> String -> (String, String) -> Template
toTemplate separator' name (path, content) = Template name (mkSuffix separator' path) content (ext separator' path) path

mkSuffix :: Char -> String -> String
mkSuffix separator' path = findSuffix $ map BS.unpack $ BS.split separator' (BS.pack path)
  where
    findSuffix xs =
      if hasSuffix xs
        then joinWith [separator'] $ take (length xs - 2) (tail xs)
        else ""
    hasSuffix xs = length xs > 2

ext :: Char -> String -> String
ext separator' path = BS.unpack $ last $ BS.split separator' (BS.pack path)

mkTemplate :: String -> String -> String -> String -> String -> Template
mkTemplate = Template

getTemplateFiles :: GenConfig -> Comm.GenCommand -> IO [Template]
getTemplateFiles config command =
  doesDirectoryExist (toString templatesPath) >>=
  (\case
     True ->
       listDirectory (toString templatesPath) <&> filter pred >>=
       (\paths -> do
          contents <- traverse (readFile . toString . (</>) templatesPath) (relFile <$> paths) -- TODO: Handle error when file does not exist
          pure $ zip paths contents)
     False -> pure []) <&> -- TODO: This should return an Either left of no template found
  (<$>) (toTemplate (separator config) (Comm.name command))
  where
    templatesPath = templatesDir config
    pred = pathStartsWith $ Comm.what command
