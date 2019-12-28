module UtilsSpec
  ( utilsSuite
  ) where

import           Test.Hspec (Spec, describe, it, shouldBe)
import           Utils

utilsSuite :: Spec
utilsSuite = do
  describe "Utils/joinWith" $ do
    it "should join a list with a given stirng" $ joinWith "-" ["Some", "thi", "ng"] `shouldBe` "Some-thi-ng"
    it "should join a list with one value in it" $ joinWith "-" ["One"] `shouldBe` "One"
    it "should handle empty list" $ joinWith "-" [] `shouldBe` ""
    it "should handle empty separator" $ joinWith "" ["Some", "thi", "ng"] `shouldBe` "Something"
  describe "Utils/trim" $ do
    it "should remove whitespace at the end of a string" $ trim "something " `shouldBe` "something"
    it "should remove whitespace at the beginning of teh string" $ trim " something" `shouldBe` "something"
    it "should not change an empty string" $ trim "" `shouldBe` ""
    it "should remove multiple whitespaces" $ trim "    something   " `shouldBe` "something"
    it "should not remove whitespace from the middle of the string" $ trim " some thing " `shouldBe` "some thing"
  describe "Utils/upperCase" $ do
    it "should transform all letters to uppercase" $ upperCase "something" `shouldBe` "SOMETHING"
    it "should not change a string which is already uppercase" $ upperCase "SOMETHING" `shouldBe` "SOMETHING"
    it "should not change an empty stirng" $ upperCase "" `shouldBe` ""
    it "should handle words with numbers" $ upperCase "something6" `shouldBe` "SOMETHING6"
    it "should handle words with special characters" $
      upperCase "something !@#$%^&*()" `shouldBe` "SOMETHING !@#$%^&*()"
  describe "Utils/pathStartsWith" $ do
    it "should check if starts with a suffix: true" $ pathStartsWith "super" "super-file.js" `shouldBe` True
    it "should check if starts with a suffix: false" $ pathStartsWith "bad" "super-file.js" `shouldBe` False
    it "should handle empty suffix" $ pathStartsWith "" "super-file.js" `shouldBe` True
    it "should handle empty filename" $ pathStartsWith "" "" `shouldBe` True
