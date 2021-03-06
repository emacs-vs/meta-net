[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CELPA](https://celpa.conao3.com/packages/meta-net-badge.svg)](https://celpa.conao3.com/#/meta-net)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/meta-net.svg)](https://jcs-emacs.github.io/jcs-elpa/#/meta-net)

# meta-net
> Parse C# .NET csproj to usable data

![CI](https://github.com/emacs-vs/meta-net/workflows/CI/badge.svg)

The goal of this project is to easily let user access csproj file data to
perform some features from Visaul Studio IDE (not VSCode). This package only
provides reading data and built it to user accessible level. Here are some
packages as examples,

* [hl-preproc](https://github.com/emacs-vs/hl-preproc) - Unhighlight invalid preprocessor region
* [meta-view](https://github.com/emacs-vs/meta-view) - View metadata from .NET assemblies
* [company-meta-net](https://github.com/emacs-vs/company-meta-net) - company-mode backend for VS C# project
* [eldoc-meta-net](https://github.com/emacs-vs/eldoc-meta-net) - Eldoc support for for VS C# project

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

If this variable is constantly being `nil`, it can cause be one of the following
reasons:

* csproj and solution are not built correctly, try rebuild it using Visual
Studio IDE (not VSCode)
* The source file is not added to csproj file but exists under the project
directory, add the source file to csproj from Visual Studio IDE or edit csproj
your self
* Not under a valid Visual Studio IDE C# project
* The new SDK based csproj no longer needs to include all source (.cs) files.

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
* [.NET Core .csproj missing file path of .cs files in vs studio 2019](https://stackoverflow.com/questions/60541348/net-core-csproj-missing-file-path-of-cs-files-in-vs-studio-2019)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
