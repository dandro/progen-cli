# Quick Start

This tutorial is design to get you started quickly and does not explain all the options 
in depth. For more information continue reading this documentation.

> :Tabs
> > :Tab title=Step One
> >
> > ## Configuration File 
> >
> > Create a file called .progenrc at the root of you project
> > with the following content
> >
> > ``` md | .progenrc
> > {
> >   "templates": ".progen/templates", <!--> Relative to current working directory. -->
> >   "output": {
> >     "component": "./components"
> >   }
> > }
> > ```
>
> > :Tab title=Step Two
> >
> > ## Create Templates
> >
> > Create a new file and save it to `.progen/templates/component.js`.
> >
> > ``` md | 
> > export default function $NAME$() {
> >     return (
> >         <section>
> >             <h2>My Heading</h2>
> >             <ul>
> >                 <li>One</li>
> >                 <li>Two</li>
> >             </ul>
> >         </section>
> >     );
> > } 
> > ```
>
> > :Tab title=Step Three
> >
> > ## Use the CLI
> > 
> > Open the terminal and call Progen with the following arguments:
> > `progen -w component -n Section -m -s "NAME:Section""`
> 
> > :Tab title=Step Four
> >
> > ## Check the Results
> >
> > Check the components directory and it should now contain a Section directory 
> > with the template we just created.

> :ToCPrevNext