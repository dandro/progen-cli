module Main where

import           Command             (makeGenCommand, parserOptions,
                                      printString)
import           Options.Applicative (execParser)

main :: IO ()
main = execParser parserOptions >>= printString
