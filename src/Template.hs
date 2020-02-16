{-# LANGUAGE LambdaCase #-}

{-|
Module : Template
Description: Files to be written

Templates represent the file to be written while holding information from the source template.
-}
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

-- | A Template represents the file that is going to be written. It
-- contains the content from the @source template@ as well as the
-- @source path@ which is the filename of the @source template@
data Template =
  Template
    { name       :: String -- ^ Name of the Template. The name comes from the Command.
    , suffix     :: String -- ^ Suffix for the file name. It comes from the source path and it is identified when the source path is split by rhe filenameSeparator. It is also optional.
    , content    :: String -- ^ Actual contents of the template.
    , extension  :: String -- ^ Filename extension.
    , sourcePath :: String -- ^ Original filename of the template.
    }
  deriving (Show, Eq)

toTemplate :: Char -> String -> (String, String) -> Template
toTemplate separator' name (path, content) = Template name (mkSuffix separator' path) content (ext separator' path) path

{-|
  Use the filename separator to find and return the @suffix@ for the template.
  If there is no suffix the function returns an empty string.
-}
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

-- |Template factory
mkTemplate :: String -> String -> String -> String -> String -> Template
mkTemplate = Template

{-|
  Iterates through all files in Progen template's source directory.
  For each file in the directory it will match all files that start
  with the `what` passed in the command.
-}
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
