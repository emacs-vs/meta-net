[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CI](https://github.com/emacs-vs/meta-net/workflows/CI/badge.svg)

# meta-net
> Parse C# .NET csproj to usable data

## :floppy_disk: Quickstart

```el
(require 'ht)

(meta-net-read-project)  ; read and build data in .cs file

(let* ((project meta-net-csproj-current)
       (csprojs (meta-net-csproj-files project))   ; List of csproj files under project
       (first-csproj (nth 0 csprojs))              ; Pick the first csproj file
       (xmls (meta-net-csproj-xmls first-csproj))  ; Get all xml files under a csproj
       (first-xml (nth 1 xmls))                    ; Pick the first xml file
       )
    (message "%s" (meta-net-xml-types first-xml))  ; print out all types from assembly xml
    )
```

All data are in `hash-table`, I would recommend you use library [ht](https://github.com/Wilfred/ht.el)
so you can access the data easily.

## :hammer: Basic Usage

##### `meta-net-csproj-current`

Buffer local variable, stores the id to access current csproj data. Please use it
with `meta-net-projects` to access the [solution](https://docs.microsoft.com/en-us/visualstudio/ide/solutions-and-projects-in-visual-studio?view=vs-2019)
data.

#####  `(meta-net-csproj-files &optional PROJECT)`

Get a list of csporj file path by PROJECT path.

#####  `(meta-net-csproj-names &optional PROJECT)`

Get a csporj base names by PROJECT path.

##### `(meta-net-define-constants PATH)`

Get a list of define constants from a csproj file. This is useful when you want
to know what's enable or disable inside preprocessor.

##### `(meta-net-csproj-xmls PATH)`

Get a list of assembly xml files under a csproj PATH file.

##### `(meta-net-xml-assemly-name PATH)`

Return the name of the assembly xml file PATH.

##### `(meta-net-xml-data PATH)`

Access the xml data from assembly xml file PATH.

## :link: References

* [The XML Documentation File](https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/october/csharp-accessing-xml-documentation-via-reflection)

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
