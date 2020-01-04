module Writer
  ( write
  ) where

import           Command               (GenCommand (GenCommand))
import           Config                (GenConfig, language, outputDirs,
                                        projectDir, separator)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (find)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import           System.Directory      (createDirectory,
                                        createDirectoryIfMissing,
                                        doesDirectoryExist)
import           System.FilePath       ((</>))
import           System.IO             (Handle, IOMode (ReadWriteMode), hClose,
                                        hPutStr, openFile)
import           Template              (Template, content, extension, filename,
                                        sourcePath)
import           Utils                 (joinWith, pathStartsWith)

write :: GenConfig -> Template -> IO (Either String String)
write config template =
  (Right $ "Created " <> filename template) <$ -- TODO: Fix this so we can handle errors (Lefts)
  (getFileHandler out (getNameWithExt (separator config) template) >>= persistWithContent (content template))
  where
    out = mkOutputDir (projectDir config) (outputDirs config) (sourcePath template)

mkOutputDir :: String -> M.Map String String -> String -> FilePath
mkOutputDir baseDir configOutputDirs templateSourcePath = baseDir </> getOutputDir configOutputDirs templateSourcePath
  where
    getOutputDir dirs pathPrefix =
      if null pathPrefix
        then ""
        else fromMaybe
               (getOutputDir dirs (take (length pathPrefix - 1) pathPrefix))
               (find (pathStartsWith pathPrefix) dirKeys >>= (`M.lookup` dirs))
    dirKeys = M.keys configOutputDirs

getNameWithExt :: Char -> Template -> String
getNameWithExt separator' template = joinWith [separator'] [filename template, extension template]

getFileHandler :: FilePath -> FilePath -> IO Handle
getFileHandler dirPath filePath = do
  createDirectoryIfMissing shouldCreateParents dirPath
  openFile (dirPath </> filePath) ReadWriteMode
  where
    shouldCreateParents = True -- TODO: This may need to come from the config instead

persistWithContent :: String -> Handle -> IO ()
persistWithContent content handle = do
  hPutStr handle content
  hClose handle
