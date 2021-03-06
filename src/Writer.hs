{-|
Module: Writer
Description: Write files

Persist template.
@Note: If there are many templates the operation is not atomic.@
-}
{-# LANGUAGE LambdaCase #-}

module Writer
  ( write
  , WriterError
  ) where

import           Config                (GenConfig, outputDirs, separator)
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

-- | WriterError represents all the possible error states in the Write module
newtype WriterError =
  FailedToWrite String -- ^ The process failed to write for some reason. More information should be found in the string.
  deriving (Show)

mkOutputDir :: AbsDir -> M.Map String RelDir -> String -> AbsDir
mkOutputDir baseDir configOutputDirs templateSourcePath =
  baseDir </> getOutputDir configOutputDirs templateSourcePath
  where
    getOutputDir dirs pathPrefix =
      if null pathPrefix
        then relDir ""
        else fromMaybe
               (getOutputDir dirs (take (length pathPrefix - 1) pathPrefix))
               (find (pathStartsWith pathPrefix) dirKeys >>= (`M.lookup` dirs))
    dirKeys = M.keys configOutputDirs

getNameWithExt :: Bool -> Maybe Char -> Tpl.Template -> RelFile
getNameWithExt asModule separator' template =
  relFile $ joinWith [withSeparator separator'] nameParts
  where
    suffix' = Tpl.suffix template
    nameParts =
      if asModule
        then [ if null suffix'
                 then Tpl.name template
                 else suffix'
             , Tpl.extension template
             ]
        else [Tpl.name template, suffix', Tpl.extension template]
    withSeparator =
      \case
        Nothing -> '.'
        Just sep -> sep

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
     AbsDir -- ^ Current working directory
  -> Bool -- ^ Whether to write the file as a module. If true, it will create a directory and save the templates in it.
  -> GenConfig -- ^ Config
  -> Tpl.Template -- ^ Template to write to the output directory
  -> IO (Either WriterError String)
write root asModule config template =
  (getFileHandler outDir filename >>= persistWithContent (Tpl.content template)) $>
  (Right $ "Created: " <> Tpl.name template <> " at " <> toString (outDir </> filename)) -- TODO: Fix this so we can handle errors (Lefts)
  where
    out = mkOutputDir root (outputDirs config) (Tpl.sourcePath template)
    outDir = combineWhenModule asModule template out
    filename = getNameWithExt asModule (separator config) template
