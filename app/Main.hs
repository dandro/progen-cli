{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (GenCommand, asModule, output,
                                      parserOptions, what)
import           Config              (Dotfile, GenConfig, dotfileName, mkConfig,
                                      mkDotfile)
import           Data.Functor        ((<&>))
import           Data.List           (intersperse)
import           Data.Map.Strict     (fromList)
import           Data.Monoid         (Last (Last))
import           Options.Applicative (execParser)
import           System.Directory    (findFile, getCurrentDirectory)
import           Template            (Template, getTemplateFiles)
import           Transformations     (transformContent)
import           Writer              (write)

getConfig :: Dotfile -> IO (Either String GenConfig)
getConfig dotfile = do
  pwd <- getCurrentDirectory -- TODO: Should handle possible errors (This can throw some errors)
  result <- findFile [pwd] dotfileName >>= traverse readFile <&> (>>= mkConfig dotfile) -- TODO: test for findFile Nothing and mkConfig Nothing
  pure $
    case result of
      Just config -> Right config
      Nothing     -> Left "ERROR: Coud not make a valid configuration."

execWrite :: GenCommand -> GenConfig -> [Template] -> IO [Either String String]
execWrite command conf templates = traverse (write (asModule command) conf) (transformContent command <$> templates)

main :: IO ()
main = do
  command <- execParser parserOptions
  let dotfile =
        mkDotfile
          (Last Nothing)
          (Last Nothing)
          (Last $(\o -> fromList [(what command, o)]) <$> output command)
          (Last Nothing)
  config' <- getConfig dotfile
  (\case
     Left err -> putStrLn err
     Right conf ->
       getTemplateFiles conf command >>=
       (\case
          Left err -> putStrLn err
          Right templates ->
            execWrite command conf templates >>=
            ((\case
                Left err -> putStrLn err
                Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) .
             sequence)))
    config'
