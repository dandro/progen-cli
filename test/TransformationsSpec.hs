module TransformationsSpec
  ( transformationsSuite
  ) where

import           Command         (GenCommand (GenCommand))
import qualified Data.Map.Strict as M
import           Template        (mkTemplate, Template)
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Transformations

mockTemplate :: Template
mockTemplate = mkTemplate "" "export default const $NAME$ = <span>MyComponent</span>;" "" ""

transformationsSuite :: Spec
transformationsSuite =
  describe "Transformations/transformContent" $ do
    it "should not change the template if substitutions are empty" $
      transformContent (GenCommand "" "" M.empty) mockTemplate `shouldBe` mockTemplate
    it "should replace a string in the template content with the substitution" $
      transformContent (GenCommand "" "" (M.fromList [("$NAME$", "MeinButton")])) mockTemplate `shouldBe`
      mkTemplate "" "export default const MeinButton = <span>MyComponent</span>;" "" ""
    it "should replace multiple occurrences of the same substitution" $
      transformContent (GenCommand "" "" (M.fromList [("span", "p")])) mockTemplate `shouldBe`
      mkTemplate "" "export default const $NAME$ = <p>MyComponent</p>;" "" ""
    it "should not change the template if the substitutions are not found in the content" $
      transformContent (GenCommand "" "" (M.fromList [("$SOURCE$", "source")])) mockTemplate `shouldBe` mockTemplate
    it "should handle multiple substitutions in the same content" $
      transformContent (GenCommand "" "" (M.fromList [("span", "p"), ("$NAME$", "MeinButton")])) mockTemplate `shouldBe`
      mkTemplate "" "export default const MeinButton = <p>MyComponent</p>;" "" ""
    it "should not change the template if the content is empty" $
      transformContent (GenCommand "" "" (M.fromList [("$NAME$", "MeinButton")])) (mkTemplate "" "" "" "") `shouldBe`
      mkTemplate "" "" "" ""
