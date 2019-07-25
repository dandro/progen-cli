{-# LANGUAGE LambdaCase #-}

module Command
  ( makeGenCommand
  , GenCommand(GenCommand)
  , printString
  , parserOptions
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, eitherReader, fullDesc, header,
                                      help, helper, info, long, option,
                                      progDesc, short, (<**>))

data What =
  Component

data GenCommand =
  GenCommand
    { what :: What
    }

makeGenCommand :: Parser GenCommand
makeGenCommand =
  GenCommand <$>
  option
    (eitherReader
       (\case
          "comp" -> Right Component
          invalid -> Left $ invalid ++ " is currently not supported"))
    (long "what" <> short 'w' <> help "What do you want to generate")

parserOptions =
  info
    (makeGenCommand <**> helper)
    (fullDesc <> progDesc "Generate code for free" <>
     header "Welcome to Progen Cli - Generate whatever you want for free")

printString :: GenCommand -> IO ()
printString (GenCommand Component) = putStrLn "Got my Component command"
