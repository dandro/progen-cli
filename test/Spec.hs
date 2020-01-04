module Main where

import           ConfigSpec          (configSuite)
import           Test.Hspec          (hspec)
import           TransformationsSpec (transformationsSuite)
import           UtilsSpec           (utilsSuite)

main :: IO ()
main =
  hspec $ do
    utilsSuite
    transformationsSuite
    configSuite
