[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CI](https://github.com/emacs-vs/meta-net/workflows/CI/badge.svg)

# meta-net
> Parse .NET assembly's XML

## :floppy_disk: Quickstart

```el
(meta-net-read-project)  ; read and build data in .cs file

(meta-net-solution-names)  ; Return a list of solutions names

(let* ((first-csproj (nth 0 meta-net-csproj-current))  ; Get the first csproj in solution
       (data (ht-get meta-net-csproj first-csproj))
       xml first-xml)

  (ht-get data 'constants)         ; Constants defined in this C# project

  (setq xml (ht-get data 'xml)     ; List of path to available assembly xml
        first-xml (nth 0 xml))     ; Get the first for example,

  (ht-get meta-net-xml first-xml)  ; ...
  )
```

## Documents

##### `meta-net-csproj-current`

Buffer local variable, stores the id to access current csproj data. Please use it
with `meta-net-projects` to access the [solution](https://docs.microsoft.com/en-us/visualstudio/ide/solutions-and-projects-in-visual-studio?view=vs-2019)
data.

#####  `(meta-net-csproj-files)`

Return a list of csporj files

#####  `(meta-net-csproj-names)`

Return a list of csporj names

##### `(meta-net-define-constants)`

Get a list of define constants from a csproj file.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
