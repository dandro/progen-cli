# Config

The dotfile configuration is how Progen knows about the project, where it's templates 
are, mapping of templates names and outputs and more.

To get started, create a file called `.progenrc`. Although it does not matter where 
this file is you must execute all Progen cli commands in the directory this file is in 
so for this guide we'll assume it is at the root of your project. It must have the following 
properties:

### templates
This is a relative path from current working directory to the directory where the templates 
are. It can be anywhere in the project but in this documentation we'll assume it 
is `.progen/templates`.

### filenameSeparator (Optional)
This is a character to separate parts of your file's name. For example if your filename is 
`page.view.js` then your `filenameSeparator` is '.'. This is useful to group related files 
by their name. This means that if you have a `page` template which is made out of 3 files,

```md | Page
page.view.js
page.types.js
page.spec.js
```

You can call Progen with `-w page -n home` and all 3 files will be copied, and the result will be

```md | Filename Separator
home.view.js
home.types.js
home.spec.js
``` 

Or if you want a module instead pass the `-m` arg, like so `-w page -n home -m` and Progen will 
create a directory with the name passed while keeping the suffixes for files copied.

```md | Filename Separator
home/
    view.js
    types.js
    spec.js
```

### Output
This is a key/value mapping of filename and path. The output of a file is picked by how 
close it matches the template filename. Output keys can overlap to give some files in a 
group a different output path. For example, let's assume want all your type files in a type 
directory. Given the following templates:

```md | Page
page.view.js
page.spec.js
page.types.js
```

You can have the following output configuration:

```md | .progenrc
{
  "output": {
    "page": "./pages",
    "page.type": "./types",
  }
}
```

The result will be:

```md | Result
/pages
    page.view.js
    page.spec.js
/types
    page.types.js
```

## Example Config File

```md | .progenrc
{
  "templates": "TEMPLATES_PATH",
  "filenameSeparator": "FILENAME_SEPARATOR_CHAR", <!--> This is optional -->
  "output": {
    "FILENAME_KEY": "RELATIVE_PATH"
  }
}
``` 

> :ToCPrevNext