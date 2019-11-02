{-# LANGUAGE LambdaCase #-}

module Template
  ( Template(filename, content, extension)
  , getTemplateFiles
  ) where

import           Command               (GenCommand (GenCommand),
                                        What (Component, Reducer), name, what)
import           Config                (GenConfig,
                                        Language (Flow, JavaScript, TypeScript),
                                        getConfig, language, templatesDir)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor          ((<&>))
import           System.Directory      (doesDirectoryExist, listDirectory)
import           Utils                 (joinWith)
import System.FilePath ((</>))

data Template =
  Template
    { filename  :: String
    , content   :: String
    , extension :: String
    }

toPred :: What -> FilePath -> Bool
toPred Component path = BS.isPrefixOf (BS.pack "component") (BS.pack path)
toPred Reducer path   = BS.isPrefixOf (BS.pack "reducer") (BS.pack path)

toTemplate :: String -> (String, String) -> Template
toTemplate name (path, content) =
  Template (joinWith "." [name, suffix path]) content (ext path)

suffix :: String -> String
suffix path = BS.unpack $ findSuffix $ BS.split '.' (BS.pack path)
  where
    findSuffix xs =
      if length xs > 2
        then xs !! 1
        else BS.pack ""

ext :: String -> String
ext path = BS.unpack $ last $ BS.split '.' (BS.pack path)

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
