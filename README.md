# progen-cli
Cli tool for generating code for your project.

The tool copies templates and performs substitutions on its contents based on values passed it from the command line. 
Progen does not contain any templates itself, you must create them all. 

## Installation
At the moment you need Stack to install the tool. Execute the `stack install` command which should install the binary in your path. 

## CLI Options
```
Welcome to Progen Cli - Generate whatever you want for free

Usage: <interactive> (-w|--what ARG) (-n|--name ARG) [-s|--substitution ARG]
                     [-m|--as-module]
  Let's make your life easier

Available options:
  -w,--what ARG            What do you want to generate
  -n,--name ARG            Name of file you're generating
  -s,--substitution ARG    Values to substitue in the template. The format is
                           '-s "$KEY_ONE$:value-one,$KEY_TWO$:value-two."'
  -m,--as-module           Treat as module. This will create a directory in the
                           output location
  -h,--help                Show this help text
```

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

## Example

Create a configuration for your project that looks like this:

```json
{
  "root": "/Users/daniel/git/my-project",
  "templates": "/Users/daniel/git/my-project/.progen/templates",
  "filenameSeparator": ".",
  "output": {
    "comp": "./components"
  }
}
```

Create a template, usually boilerplate code, and add them to the templates directly in the configuration. The name of 
the template file should match a configuration output key so the copied file can be put there.
```js
export default function $NAME$() {
    return '';
}
```

Then you can use the cli tool to copy the template:

```bash
progencli -w comp -n ExampleFunction -s "$NAME$:example"
```

## Development
The project runs on Haskell GHC and has been setup with Stack version lts-13.29.
