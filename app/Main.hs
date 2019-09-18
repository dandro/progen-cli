{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (makeGenCommand, parserOptions)
import           Config              (getConfig)
import           Data.List           (intersperse)
import           Options.Applicative (execParser)
import           Template            (resolveTemplate)
import           Writer              (write)

main :: IO ()
main = do
  command <- execParser parserOptions
  getConfig >>=
    (\case
       Nothing -> putStrLn "ERROR: There is no configuration."
       Just c -> do
         let templates = resolveTemplate c command
         res <- traverse (write c) templates
         (\case
            Left err -> putStrLn err
            Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) $
           sequence res)
