# CLI

## Options

### -w | --what
What do you want to generate? This will match templates that start with this 
value. If you have the following templates:

```md | cli what
feature.component.js
feature.reducer.js
```

You can run `progen -w fe` and it will match on both templates.

### -n | --name
What is the name of the generated code? This name will be used as the name of 
the file or directory create.

### -s | --substitutions
Accepts a string of `;` separated `key:value` pairs. If the `key` appears between 
dollar signs within the content of templates matched, it will be replaced with the 
value provided. For example, if your template contains `function $my-function-name$() {}` 
then you can call Progen with `-s "my-function-name:main"` and the resulting file 
will have `function main() {}` in it.

You can also have many substitutions for one command. For example `-s "ONE:first; TWO:second"`.

> ⚠️ The substitution will apply to all matches in the template content. If you 
> have $NAME$ 3 times, all 3 instances will be changed.

### -m | --as-module
Use this option to create a directory with all matching templates inside. If we have
the following templates:

```md | cli templates
feature.component.js
feature.reducer.js
```

Our configuration output mapping is:

```md | .progenrc
{
    "output": {
        "feature": "./modules"
    }
}
```

Now, we run `progen -w feature -n MyFeature` Progen will produce:

```md | cli -m
modules/
    MyFeature/
        component.js
        reducer.js
```

> Note: When we use `as-module` the name is not appended to the resulting filename.

### -o | --output
Use the `-o` arg to override the configuration. This will ignore every 
other output mapping and place all files generated in the path provided.

The value must be a valid relative path to the root of the project 
specified in the config file.

### -h | --help
Display all options with their descriptions. This is only the cli help 
so if you are looking for more help you may want to come back to the 
documentation. 

> :ToCPrevNext
