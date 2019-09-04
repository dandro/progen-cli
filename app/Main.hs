{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (makeGenCommand, parserOptions)
import           Config              (makeDefaultConfig)
import           Data.List           (intersperse)
import           Options.Applicative (execParser)
import           Template            (resolveTemplate)
import           Writer              (write)

main :: IO ()
main = do
  command <- execParser parserOptions
  let config = makeDefaultConfig
  let templates = resolveTemplate config command
  res <- traverse (write config) templates
  (\case
     Left err -> putStrLn err
     Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) $
    sequence res
