{-# LANGUAGE LambdaCase #-}

module Command
  ( makeGenCommand
  , GenCommand(GenCommand, what, name)
  , parserOptions
  , What(..)
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative (Parser, eitherReader, fullDesc, header,
                                      help, helper, info, long, option,
                                      progDesc, short, strOption, (<**>))

data What
  = Component
  | Reducer

data GenCommand =
  GenCommand
    { what :: What
    , name :: String
    }

makeGenCommand :: Parser GenCommand
makeGenCommand =
  GenCommand <$>
  option
    (eitherReader
       (\case
          "comp" -> Right Component
          "reducer" -> Right Reducer
          invalid -> Left $ invalid ++ " is currently not supported"))
    (long "what" <> short 'w' <> help "What do you want to generate") <*>
  strOption (long "name" <> short 'n' <> help "Name of file you're generating")

parserOptions =
  info
    (makeGenCommand <**> helper)
    (fullDesc <> progDesc "Let's make your life easier" <>
     header "Welcome to Progen Cli - Generate whatever you want for free")
