{-# LANGUAGE LambdaCase #-}

module Template
  ( Template(filename, content, extension)
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
import           Utils                 (joinWith)

data Template =
  Template
    { filename  :: String
    , content   :: String
    , extension :: String
    }

toPred :: String -> FilePath -> Bool
toPred str path = BS.isPrefixOf (BS.pack str) (BS.pack path)

toTemplate :: String -> (String, String) -> Template
toTemplate name (path, content) = Template (joinWith "." [name, suffix path]) content (ext path)

suffix :: String -> String
suffix path = findSuffix $ map BS.unpack $ BS.split '.' (BS.pack path)
  where
    findSuffix xs =
      if hasSuffix xs
        then joinWith "." $ take (length xs - 2) (tail xs)
        else ""
    hasSuffix xs = length xs > 2

ext :: String -> String
ext path = BS.unpack $ last $ BS.split '.' (BS.pack path)

mkTemplate :: String -> String -> String -> Template
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
    pred = toPred $ what command
