{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (parserOptions)
import           Config              (getConfig)
import           Data.List           (intersperse)
import           Options.Applicative (execParser)
import           Template            (getTemplateFiles)
import           Transformations     (transformContent)
import           Writer              (write)

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
