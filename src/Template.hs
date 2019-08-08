module Template
  ( Template(filename, content, extension)
  , resolveTemplate
  ) where

import           Command (GenCommand (GenCommand), What (Component))
import           Config  (GenConfig, Language (Flow, JavaScript, TypeScript),
                          language)

reactStatelessComponentTemplate =
  "import React from 'react';\n\nexport default function myName(props) {\n\treturn <p>My dummy component</p>\n}"

data Template =
  Template
    { filename  :: String
    , content   :: String
    , extension :: String
    }

getFileExtension :: Language -> String
getFileExtension JavaScript = ".js"
getFileExtension Flow       = ".js"
getFileExtension TypeScript = ".ts"

resolveTemplate :: GenConfig -> GenCommand -> Template
resolveTemplate config (GenCommand Component filename) =
  Template filename reactStatelessComponentTemplate (getFileExtension (language config))
