module Writer
  ( write
  ) where

import           Command               (GenCommand (GenCommand))
import           Config                (GenConfig, outputDirs, projectDir,
                                        separator)
import qualified Data.ByteString.Char8 as BS
import           Data.List             (find)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import           System.Directory      (createDirectory,
                                        createDirectoryIfMissing,
                                        doesDirectoryExist)
import           System.IO             (Handle, IOMode (ReadWriteMode), hClose,
                                        hPutStr)
import           System.Path           (AbsDir, RelDir, RelFile, absDir, relDir,
                                        relFile, toString, (</>))
import           System.Path.IO        (openFile)
import           Template              (Template, content, extension, filename,
                                        sourcePath)
import           Utils                 (joinWith, pathStartsWith)

write :: GenConfig -> Template -> IO (Either String String)
write config template =
  (Right $ "Created " <> filename template) <$ -- TODO: Fix this so we can handle errors (Lefts)
  (getFileHandler out (getNameWithExt (separator config) template) >>= persistWithContent (content template))
  where
    out = mkOutputDir (projectDir config) (outputDirs config) (sourcePath template)

mkOutputDir :: AbsDir -> M.Map String RelDir -> String -> AbsDir
mkOutputDir baseDir configOutputDirs templateSourcePath = baseDir </> getOutputDir configOutputDirs templateSourcePath
  where
    getOutputDir dirs pathPrefix =
      if null pathPrefix
        then relDir ""
        else fromMaybe
               (getOutputDir dirs (take (length pathPrefix - 1) pathPrefix))
               (find (pathStartsWith pathPrefix) dirKeys >>= (`M.lookup` dirs))
    dirKeys = M.keys configOutputDirs

getNameWithExt :: Char -> Template -> RelFile
getNameWithExt separator' template = relFile $ joinWith [separator'] [filename template, extension template]

getFileHandler :: AbsDir -> RelFile -> IO Handle
getFileHandler dirPath filePath = do
  createDirectoryIfMissing shouldCreateParents (toString dirPath)
  openFile (dirPath </> filePath) ReadWriteMode
  where
    shouldCreateParents = True -- TODO: This may need to come from the config instead

persistWithContent :: String -> Handle -> IO ()
persistWithContent content handle = do
  hPutStr handle content
  hClose handle
