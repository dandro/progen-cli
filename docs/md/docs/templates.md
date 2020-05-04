# Templates

Create templates for common abstractions and leverage Progen's power of automation
to generate boilerplate code. This will increase your team's productivity, help you 
enforce your standards and help on-board new team members.

## Filenames & Grouping
There is no real requirement for file names, so you can get creative and use the 
tool however it yields more value to you and your team, however, a useful pattern 
to keep in mind could be would be to start the name with the __abstraction layer__
followed by "group names". For example `component.view.js`. This would allow you to 
group many templates under with the `component` prefix but if you only want the view 
you can be more specific and use `component.view`.

### Grouping Examples
> :Tabs
> > :Tab title=Pure Component Group
> >
> > In our projects, to create a React component, we separate the view, types and 
> > stories into separate files and put it all together before exporting it in an 
> > index.js file. We can group our templates like so:
> >
> > ```md | Pure Component Group
> > pure-component.index.js
> > pure-component.view.js
> > pure-component.view.spec.js
> > pure-component.enhancer.js
> > pure-component.types.js
> > pure-component.styles.scss
> > ```
> > > Run `progen -w pure -n ComponentName` to use the template group.
>
> > :Tab title=Redux Reducer Group
> >
> > Redux Reducer must have a reducer file, a type file and an action file. For this we can 
> > group our templates like so:
> >
> > ```md | Redux Component Group
> > reducer.index.js
> > reducer.reducer.js
> > reducer.reducer.spec.js
> > reducer.actions.js
> > reducer.actions.spec.js
> > reducer.types.js
> > ```
> > > Run `progen -w reducer -n ReducerName` to use the template group.
> > 

## Substitutions
Templates can have variables which will be substituted when copying the template. 
Simply put a name between dollar signs. For example `function $NAME$() {` and pass 
in the substitution in the CLI with the `-s or --substitute` command. The value for 
the substitute cli argument is a `key:value` pairs separated by `,`.

``` bash | Substitutions
progen -w component -n MySection -s "NAME:MySection, TITLE:This is my Section"
```

This command will substitute `$NAME$` for MySection and `$TITLE` for "This is my Section".

> :ToCPrevNext