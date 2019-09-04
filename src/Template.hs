module Template
  ( Template(filename, content, extension)
  , resolveTemplate
  ) where

import           Command (GenCommand (GenCommand), What (Component, Reducer))
import           Config  (GenConfig, Language (Flow, JavaScript, TypeScript),
                          language)

reactStatelessComponentTemplate =
  "import React from 'react';\n\nexport default function myName(props) {\n\treturn <p>My dummy component</p>\n}"
reactStatelessComponentFlowTemplate =
  "import React from 'react';\nimport type { Props } from './types';\n\nexport default function myName(props: Props) {\n\treturn <p>My dummy component</p>\n}"
reactStatelessComponentTypesFlowTemplate = "// @flow\n\nexport type Props = {};"
reducerTemplate = "function reducer(state, action) {\n\t switch(action.type) {\n\t\tdefault:\n\t\t\treturn state;\n\t}\n}"
reducerFlowTemplate = "// @flow\n\nexport type State = {};\nexport type Action = {};"

data Template =
  Template
    { filename  :: String
    , content   :: String
    , extension :: String
    }

data ModeTemplateConfig =
  ModeTemplateConfig
    { template :: String
    , suffix   :: String
    , ext :: String
    }

getComponentTemplatesForMode :: Language -> [ModeTemplateConfig]
getComponentTemplatesForMode JavaScript = [ModeTemplateConfig reactStatelessComponentTemplate "" ".js"]
getComponentTemplatesForMode Flow =
  [ ModeTemplateConfig reactStatelessComponentFlowTemplate ".view" ".js"
  , ModeTemplateConfig reactStatelessComponentTypesFlowTemplate ".types" ".js"
  ]
getComponentTemplatesForMode TypeScript = [ModeTemplateConfig reactStatelessComponentTemplate "" ".ts"]

getReducerTemplatesForMode :: Language -> [ModeTemplateConfig]
getReducerTemplatesForMode JavaScript = [ModeTemplateConfig reducerTemplate "" ".js"]
getReducerTemplatesForMode Flow =
  [ ModeTemplateConfig reducerTemplate "" ".js"
  , ModeTemplateConfig reducerFlowTemplate ".types" ".js"
  ]
getReducerTemplatesForMode TypeScript = [ModeTemplateConfig reducerTemplate "" ".ts"]

resolveTemplate :: GenConfig -> GenCommand -> [Template]
resolveTemplate config (GenCommand Component filename) =
  (\(ModeTemplateConfig template suffix ext) -> Template (filename <> suffix) template ext) <$>
  getComponentTemplatesForMode (language config)

resolveTemplate config (GenCommand Reducer filename) =
  (\(ModeTemplateConfig template suffix ext) -> Template (filename <> suffix) template ext) <$>
  getReducerTemplatesForMode (language config)