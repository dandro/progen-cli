{-# LANGUAGE LambdaCase #-}

module Command
  ( makeGenCommand
  , GenCommand(GenCommand, what, name)
  , parserOptions
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, eitherReader, fullDesc, header,
                                      help, helper, info, long, option,
                                      progDesc, short, strOption, (<**>))
data GenCommand =
  GenCommand
    { what :: String
    , name :: String
    }

makeGenCommand :: Parser GenCommand
makeGenCommand =
  GenCommand <$>
  strOption (long "what" <> short 'w' <> help "What do you want to generate") <*>
  strOption (long "name" <> short 'n' <> help "Name of file you're generating")

parserOptions =
  info
    (makeGenCommand <**> helper)
    (fullDesc <> progDesc "Let's make your life easier" <>
     header "Welcome to Progen Cli - Generate whatever you want for free")
