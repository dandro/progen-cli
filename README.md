# progen-cli
Cli tool for generating code for your project.

The tool copies templates and performs substitutions on its contents based on values passed it from the command line. 
Progen does not contain any templates itself, you must create them all. 

## Installation
At the moment you need Stack to install the tool. Execute the `stack install` command which should install the binary in your path. 

## Configuration

| Property          | Description           | Example           |
| ----------------- |---------------------- |------------------ |
| root              |  Project Root. Must be an absolute path. | /Users/daniel/git/my-project |
| templates         |  Path to templates. Must be an absolute path. | /Users/daniel/git/my-project/.progen/templates |
| filenameSeparator |  Character used to separate parts of the filename. This way we can identify suffix to the name. | in "page.view.js" the separator is '.' |
| output            |  Object mapping of template names and where the template should be saved.  | `{ "component.view": "./views", "component.type": "./types" }` |


You can use this example configuration to get started:

```json
{
  "root": "DIR_PATH",
  "templates": "TEMPLATES_PATH",
  "filenameSeparator": "FILENAME_SEPARATOR_CHAR",
  "output": {
    "FILENAME_KEY": "RELATIVE_PATH"
  }
}
```

## Development
The project runs on Haskell GHC and has been setup with Stack version lts-13.29.

## Generate Haddock Documentation
run the `gen-docs.sh` shell script to create the code documentation. This is not to know how to 
use the application, but to understand how it is built. It is meant for developers of the application. 
