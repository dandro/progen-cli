{-# LANGUAGE LambdaCase #-}

module Template
  ( Template(filename, content, extension, sourcePath)
  , mkTemplate
  , getTemplateFiles
  ) where

import           Command               (GenCommand (GenCommand), name, what)
import           Config                (GenConfig,
                                        Language (Flow, JavaScript, TypeScript),
                                        getConfig, language, templatesDir)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor          ((<&>))
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.FilePath       ((</>))
import           Utils                 (joinWith, pathStartsWith)

data Template =
  Template
    { filename   :: String
    , content    :: String
    , extension  :: String
    , sourcePath :: String
    }

toTemplate :: String -> (String, String) -> Template
toTemplate name (path, content) = Template (joinWith "." [name, suffix path]) content (ext path) path -- TODO: Here the . is the "separator" and should come from the config

suffix :: String -> String
suffix path = findSuffix $ map BS.unpack $ BS.split '.' (BS.pack path) -- TODO: Take the "separator" form the config
  where
    findSuffix xs =
      if hasSuffix xs
        then joinWith "." $ take (length xs - 2) (tail xs) -- TODO: Take the "separator" from the config
        else ""
    hasSuffix xs = length xs > 2

ext :: String -> String
ext path = BS.unpack $ last $ BS.split '.' (BS.pack path) -- TODO: Take the "separator" from the config

mkTemplate :: String -> String -> String -> String -> Template
mkTemplate = Template

getTemplateFiles :: GenConfig -> GenCommand -> IO [Template]
getTemplateFiles config command =
  doesDirectoryExist templatesPath >>=
  (\case
     True ->
       listDirectory templatesPath <&> filter pred >>=
       (\paths -> do
          contents <- traverse (readFile . (</>) templatesPath) paths -- TODO: Handle error when file does not exist
          pure $ zip paths contents)
     False -> pure []) <&> -- TODO: This should return an Either left of no template found
  (<$>) (toTemplate $ name command)
  where
    templatesPath = templatesDir config
    pred = pathStartsWith $ what command
