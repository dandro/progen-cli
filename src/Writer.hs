{-|
Module: Writer
Description: Write files

Persist template.
@Note: If there are many templates the operation is not atomic.@
-}
module Writer
  ( write
  ) where

import           Config                (GenConfig, outputDirs, projectDir,
                                        separator)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor          (($>))
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
import qualified Template              as Tpl
import           Utils                 (joinWith, pathStartsWith)

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

getNameWithExt :: Char -> Tpl.Template -> RelFile
getNameWithExt separator' template =
  relFile $ joinWith [separator'] [Tpl.name template, Tpl.suffix template, Tpl.extension template]

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

combineWhenModule :: Bool -> Tpl.Template -> AbsDir -> AbsDir
combineWhenModule asModule template out =
  out </> -- TODO: Refactor this to use something like mappend to combine with name if is module
  relDir
    (if asModule
       then Tpl.name template
       else "")

-- | Write template file to output directory.
write ::
     Bool -- ^ Whether to write the file as a module. If true, it will create a directory and save the templates in it.
  -> GenConfig -- ^ Config
  -> Tpl.Template -- ^ Template to write to the output directory
  -> IO (Either String String)
write asModule config template =
  (getFileHandler (combineWhenModule asModule template out) (getNameWithExt (separator config) template) >>=
   persistWithContent (Tpl.content template)) $>
  (Right $ "Created: " <> show template) -- TODO: Fix this so we can handle errors (Lefts)
  where
    out = mkOutputDir (projectDir config) (outputDirs config) (Tpl.sourcePath template)
