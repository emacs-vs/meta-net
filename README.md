[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CI](https://github.com/emacs-vs/meta-net/workflows/CI/badge.svg)

# meta-net
> Parse C# .NET csproj to usable data

The goal of this project is to easily let user access csproj file data to
perform some features from Visaul Studio IDE (not VSCode). This package only
provides reading data and built it to user accessible level.

## :trophy: Features

* *Access includes file* - source files under current project
* *Access define constants* - define constants (preprocessor)
* *Access assembly xml files* - access xml files and it's xml data

## :floppy_disk: Quickstart

```el
(require 'ht)

(meta-net-read-project)  ; read and build data, call this in a .cs file

(let* ((project meta-net-csproj-current)           ; path to csproj file
       (csprojs (meta-net-csproj-files project))   ; List of csproj files under project
       (first-csproj (nth 0 csprojs))              ; Pick the first csproj file
       (xmls (meta-net-csproj-xmls first-csproj))  ; Get all xml files under a csproj
       (first-xml (nth 1 xmls))                    ; Pick the first xml file
       )
    (message "%s" (meta-net-xml-types first-xml))  ; print out all types from assembly xml
    )
```

*P.S. All data are in `hash-table`, I would recommend you use library [ht](https://github.com/Wilfred/ht.el)
so you can access the data easily.*

## :hammer: Basic Usage

#### `meta-net-csproj-current`

Buffer local variable that stores the csproj path for current `.cs` source file.
Use this variable with hash-table `meta-net-csproj` to access csporj data.

The variable can be `nil` if the source file does not exist in any valid C#
project. Try rebuilds the solution (`.sln`) and C# project files (`.csproj`);
or add the source file a csproj.

You can add C# source file (`.cs`) by using Visual Studio's (not VSCode);
right click on csproj then add item.

#### `(meta-net-define-constants PATH)`

Get a list of define constants from a csproj file. This is useful when you want
to know what's enable or disable inside preprocessor.

#### `(meta-net-includes PATH)`

Get a list of included source files under a csproj PATH file.

#### `(meta-net-csproj-xmls PATH)`

Get a list of assembly xml files under a csproj PATH file.

#### `(meta-net-xml-assemly-name PATH)`

Return the name of the assembly xml file PATH.

#### `(meta-net-xml-data PATH)`

Access the xml data from assembly xml file PATH.

## :link: References

* [The XML Documentation File](https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/october/csharp-accessing-xml-documentation-via-reflection)

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
