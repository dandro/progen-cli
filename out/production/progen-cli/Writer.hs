module Writer
  ( write
  ) where

import           Command               (GenCommand (GenCommand))
import           Config                (GenConfig, language, projectDir)
import qualified Data.ByteString.Char8 as BS
import           System.Directory      (createDirectory,
                                        createDirectoryIfMissing,
                                        doesDirectoryExist)
import           System.FilePath       ((</>))
import           System.IO             (Handle, IOMode (ReadWriteMode), hClose,
                                        hPutStr, openFile)
import           Template              (Template, content, extension, filename)
import           Utils                 (joinWith)

write :: GenConfig -> Template -> IO (Either String String)
write config template =
  (Right $ "Created " <> filename template) <$
  (getFileHandler (projectDir config) (getNameWithExt template) >>=
   persistWithContent (content template))

getNameWithExt :: Template -> String
getNameWithExt template = joinWith "." [filename template, extension template]

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
