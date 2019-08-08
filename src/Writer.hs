module Writer
  ( write
  ) where

import           Command          (GenCommand (GenCommand), What (Component))
import           Config           (GenConfig, language, projectDir)
import           System.Directory (createDirectory, createDirectoryIfMissing,
                                   doesDirectoryExist)
import           System.IO        (Handle, IOMode (ReadWriteMode), hClose,
                                   hPutStr, openFile)
import           Template         (Template, content, extension, filename)

write :: GenConfig -> Template -> IO (Either String String)
write config template =
  (\_ -> Right "All done") <$> -- TODO find a way to remove the lambda
  (getFileHandler (projectDir config) (filename template <> extension template) >>=
   persistWithContent (content template))

getFileHandler :: FilePath -> FilePath -> IO Handle
getFileHandler dirPath filePath = do
  createDirectoryIfMissing shouldCreateParents dirPath
  openFile (dirPath <> filePath) ReadWriteMode
  where
    shouldCreateParents = True

persistWithContent :: String -> Handle -> IO ()
persistWithContent content handle = do
  hPutStr handle content
  hClose handle
