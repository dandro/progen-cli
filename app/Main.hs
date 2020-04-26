{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (GenCommand, asModule, output,
                                      parserOptions, what)
import           Config              (ConfigError, Dotfile, GenConfig,
                                      dotfileName, handleConfigResult, mkConfig,
                                      mkDotfile)
import           Data.Functor        ((<&>))
import           Data.List           (intersperse)
import           Data.Map.Strict     (fromList)
import           Data.Monoid         (Last (Last))
import           Options.Applicative (execParser)
import           System.Directory    (findFile, getCurrentDirectory)
import           System.Path         (AbsDir, absDir)
import           Template            (Template, getTemplateFiles)
import           Transformations     (transformContent)
import           Writer              (WriterError, write)

getConfig :: FilePath -> Dotfile -> IO (Either ConfigError GenConfig)
getConfig pwd dotfile =
  findFile [pwd] dotfileName >>= traverse readFile <&> (>>= mkConfig dotfile) <&> handleConfigResult -- TODO: May want to handle findFile Nothing separate to mkConfig Nothing

execWrite :: AbsDir -> GenCommand -> GenConfig -> [Template] -> IO [Either WriterError String]
execWrite root command conf templates =
  traverse (write root (asModule command) conf) (transformContent command <$> templates)

main :: IO ()
main = do
  pwd <- getCurrentDirectory -- TODO: Should handle possible errors (This can throw some errors)
  command <- execParser parserOptions
  let dotfile = mkDotfile (Last Nothing) (Last $(\o -> fromList [(what command, o)]) <$> output command) (Last Nothing)
  config' <- getConfig pwd dotfile
  (\case
     Left err -> print err
     Right conf ->
       getTemplateFiles (absDir pwd) conf command >>=
       (\case
          Left err -> print err
          Right templates ->
            execWrite (absDir pwd) command conf templates >>=
            ((\case
                Left err -> print err
                Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) .
             sequence)))
    config'
