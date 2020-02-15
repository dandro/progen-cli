module ConfigSpec
  ( configSuite
  ) where

import           Config          (mkConfig, outputDirs, projectDir, separator,
                                  templatesDir)
import           Data.Map.Strict (fromList)
import           System.Path     (AbsDir, absDir, relDir, rootDir, toString,
                                  (</>))
import           Test.Hspec      (Spec, describe, it, shouldBe)

stubRootDir :: AbsDir
stubRootDir = rootDir </> relDir "project"

stubTemplatesDir :: AbsDir
stubTemplatesDir = stubRootDir </> relDir "templates"

configSuite :: Spec
configSuite = do
  describe "Config/mkConfig - Error" $ do
    it "should handle an empty json string" $ mkConfig "" `shouldBe` Nothing
    it "should handle an invalid json string" $ mkConfig "What's cooking, good looking?" `shouldBe` Nothing
    it "should handle an valid json string without the correct values" $
      mkConfig "{\"name\": \"daniel\"}" `shouldBe` Nothing
    it "should handle empty filenameSeparator value" $
      mkConfig
        ("{ \"root\": \"" ++
         toString stubRootDir ++
         "\", \"templates\": \"" ++
         toString stubTemplatesDir ++ "\", \"filenameSeparator\": \"\", \"output\": { \"comp\": \"components\" } }") `shouldBe`
      Nothing
  describe "Config/mkConfig - Success" $ do
    let actual =
          mkConfig
            ("{ \"root\": \"" ++
             toString stubRootDir ++
             "\", \"templates\": \"" ++
             toString stubTemplatesDir ++ "\", \"filenameSeparator\": \".\", \"output\": { \"comp\": \"components\" } }")
    it "should have project dir" $ (projectDir <$> actual) `shouldBe` Just stubRootDir
    it "should have templates dir" $ (templatesDir <$> actual) `shouldBe` Just stubTemplatesDir
    it "should have output mapping" $ (outputDirs <$> actual) `shouldBe` Just (fromList [("comp", relDir "components")])
    it "should have a filename separator" $ (separator <$> actual) `shouldBe` Just '.'
