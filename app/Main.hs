{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (makeGenCommand, parserOptions)
import           Config              (getConfig)
import           Data.List           (intersperse)
import           Options.Applicative (execParser)
import           Template            (getTemplateFiles)
import           Writer              (write)

main :: IO ()
main = do
  command <- execParser parserOptions
  getConfig >>=
    (\case
       Left err -> putStrLn err
       Right conf -> do
         templates <- getTemplateFiles conf command
         res <- traverse (write conf) templates
         (\case
            Left err -> putStrLn err
            Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) $
           sequence res)
