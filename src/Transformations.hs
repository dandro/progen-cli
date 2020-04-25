{-|
Module: Transformations
Description: Modifications to template's content

Transformations are applied to the content of a template where each instance of a
key found in the Commands substitutions Map is searched in the template content
and replaced with the substitution value.
-}
module Transformations
  ( transformContent
  ) where

import           Command   (GenCommand, sub)
import qualified Data.Map  as M
import qualified Data.Text as T
import           Template  (Template, content, extension, mkTemplate, name,
                            sourcePath, suffix)

{-|
  Apply substitutions to templates content
-}
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
    replace' key content' =
      T.unpack $ T.replace (T.pack ("$" <> key <> "$")) (T.pack (substitutions M.! key)) $ T.pack content'
