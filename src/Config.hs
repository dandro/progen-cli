module Config
  ( GenConfig(projectDir, language)
  , Language(..)
  , makeDefaultConfig
  ) where

data Language
  = JavaScript
  | Flow
  | TypeScript

data GenConfig =
  GenConfig
    { projectDir :: String
    , language   :: Language
    }

makeDefaultConfig :: GenConfig
makeDefaultConfig = GenConfig "/Users/daniel.martinez/Documents/js/dummy-project/" Flow
