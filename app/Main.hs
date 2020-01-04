{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (parserOptions)
import           Config              (GenConfig, dotfileName, mkConfig)
import           Data.Functor        ((<&>))
import           Data.List           (intersperse)
import           Options.Applicative (execParser)
import           System.Directory    (findFile, getCurrentDirectory)
import           Template            (getTemplateFiles)
import           Transformations     (transformContent)
import           Writer              (write)

getConfig :: IO (Either String GenConfig)
getConfig = do
  pwd <- getCurrentDirectory
  result <- findFile [pwd] dotfileName >>= traverse readFile <&> (>>= mkConfig)
  pure $
    case result of
      Just config -> Right config
      Nothing -> Left "ERROR: Coud not make a valid configuration."

main :: IO ()
main = do
  command <- execParser parserOptions
  getConfig >>=
    (\case
       Left err -> putStrLn err
       Right conf -> do
         templates <- getTemplateFiles conf command
         res <- traverse (write conf . transformContent command) templates
         (\case
            Left err -> putStrLn err
            Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) $
           sequence res)
