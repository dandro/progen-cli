{-# LANGUAGE LambdaCase #-}

module Main where

import           Command             (makeGenCommand, parserOptions)
import           Config              (makeDefaultConfig)
import           Options.Applicative (execParser)
import           Template            (resolveTemplate)
import           Writer              (write)

main :: IO ()
main = do
  command <- execParser parserOptions
  let config = makeDefaultConfig
  let template = resolveTemplate config command
  res <- write config template
  (\case
     Left err -> putStrLn err
     Right msg -> putStrLn msg)
    res
