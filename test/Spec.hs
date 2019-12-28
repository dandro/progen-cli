module Main where

import           Test.Hspec (hspec)
import           UtilsSpec  (utilsSuite)
import TransformationsSpec (transformationsSuite)

main :: IO ()
main = hspec $ do 
  utilsSuite
  transformationsSuite
