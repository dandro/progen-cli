module TransformationsSpec
  ( transformationsSuite
  ) where

import           Command         (mkGenCommand)
import qualified Data.Map.Strict as M
import           Template        (Template, mkTemplate)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Transformations

mockTemplate :: Template
mockTemplate = mkTemplate "" "" "export default const $NAME$ = <$span$>MyComponent</$span$>;" "" ""

mkExpectedTemplate :: String -> Template
mkExpectedTemplate subs = mkTemplate "" "" subs "" ""

transformationsSuite :: Spec
transformationsSuite =
  describe "Transformations/transformContent" $ do
    it "should not change the template if substitutions are empty" $
      transformContent (mkGenCommand "" "" M.empty False Nothing) mockTemplate `shouldBe` mockTemplate
    it "should replace a string in the template content with the substitution" $
      transformContent (mkGenCommand "" "" (M.fromList [("NAME", "MeinButton")]) False Nothing) mockTemplate `shouldBe`
      mkExpectedTemplate "export default const MeinButton = <$span$>MyComponent</$span$>;"
    it "should replace multiple occurrences of the same substitution" $
      transformContent (mkGenCommand "" "" (M.fromList [("span", "p")]) False Nothing) mockTemplate `shouldBe`
      mkExpectedTemplate "export default const $NAME$ = <p>MyComponent</p>;"
    it "should not change the template if the substitutions are not found in the content" $
      transformContent (mkGenCommand "" "" (M.fromList [("$SOURCE$", "source")]) False Nothing) mockTemplate `shouldBe`
      mockTemplate
    it "should handle multiple substitutions in the same content" $
      transformContent
        (mkGenCommand "" "" (M.fromList [("span", "p"), ("NAME", "MeinButton")]) False Nothing)
        mockTemplate `shouldBe`
      mkExpectedTemplate "export default const MeinButton = <p>MyComponent</p>;"
    it "should not change the template if the content is empty" $
      transformContent (mkGenCommand "" "" (M.fromList [("NAME", "MeinButton")]) False Nothing) (mkExpectedTemplate "") `shouldBe`
      mkExpectedTemplate ""
    it "should only substitute a value if it is within $s" $
      transformContent (mkGenCommand "" "" (M.fromList [("const", "let")]) False Nothing) mockTemplate `shouldBe`
      mockTemplate
