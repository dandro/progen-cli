module Transformations
  ( transformContent
  ) where

import           Command   (GenCommand, sub)
import qualified Data.Map  as M
import qualified Data.Text as T
import           Template  (Template, content, extension, name, suffix, mkTemplate,
                            sourcePath)

transformContent :: GenCommand -> Template -> Template
transformContent command template =
  mkTemplate
    (name template)
    (suffix template)
    (foldr replace' (content template) (M.keys substitutions))
    (extension template)
    (sourcePath template)
  where
    substitutions = sub command
    replace' key content' = T.unpack $ T.replace (T.pack key) (T.pack (substitutions M.! key)) $ T.pack content'
