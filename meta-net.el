;;; meta-net.el --- Parse .NET assembly's XML  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 21:17:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Parse .NET assembly's XML
;; Keyword: assembly xml utility
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (ht "2.3") (f "0.20.0"))
;; URL: https://github.com/emacs-vs/meta-net

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Parse .NET assembly's XML.
;;

;;; Code:

(require 'f)
(require 'ht)
(require 'xml)
(require 'subr-x)

(defgroup meta-net nil
  "Parse .NET assembly's XML."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-net"))

(defconst meta-net--tag-property "P:"
  "Tag represent properties declaration.")

(defconst meta-net--tag-method "M:"
  "Tag represent methods declaration.")

(defconst meta-net--tag-type "T:"
  "Tag represent types (enum, class, interface) declaration.")

(defconst meta-net--tag-fields "F:"
  "Tag represent fields.")

(defconst meta-net--tag-events "E:"
  "Tag represent events.")

(defvar meta-net-projects (ht-create)
  "Store all the project points to csporj files.

This prevents reading the same project and waste it's performance.  Notice
project path here aren't source control path.  It's just the parent path of
all .csproj file so this will work without the source control or one repository
with multiple projects' structure.

Data look like (path . (csporj_1, csproj_2)).")

(defvar-local meta-net-csproj-current nil
  "Parent path of all csproj files under current file.

Please use this variable with variable `meta-net-projects' to get the full
list of csproj.")

(defvar meta-net-csproj (ht-create)
  "Mapping of all csproj file entries.

Store data in (path . hash-table); hash-table are data defined in csporj.
See function `meta-net--parse-csproj-xml' to get more information.")

(defvar meta-net-xml (ht-create)
  "Mapping of all assembly xml files to it's data in hash table.

Store data in (path . hash-table); hash-table are data defined in assembly xml.")

(defvar meta-net-show-log t
  "Show the log message from this package.")

(defvar meta-net-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun meta-net-log (fmt &rest args)
  "Log message like function `message' with same argument FMT and ARGS."
  (when meta-net-show-log
    (let (message-log-max) (apply 'message fmt args))))

(defun meta-net-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when meta-net-show-debug (apply 'message fmt args)))

(defun meta-net--project-current ()
  "Return the current project root."
  (cdr (project-current)))

(defun meta-net--walk-path (path fnc &optional start-path)
  "Walk through PATH and execute FNC.

Argument START-PATH should be sub directory from PATH."
  (let* ((lst (f-split path)) (lst-start (when start-path (f-split start-path)))
         (len (length lst)) (len-start (length lst-start))
         (index (if start-path (1- len-start) 0))
         (current (if start-path start-path (nth index lst)))
         break)
    (while (and (not break) (< index len))
      (when (file-directory-p current)
        (setq break (funcall fnc current)))
      (setq index (1+ index))
      (when (< index len)
        (setq current (f-slash (f-join current (nth index lst))))))))

;;
;; (@* "Core" )
;;

(defun meta-net--grab-define-constants (project-node)
  "Return list of string that are define constants.

Argument PROJECT-NODE is the root node from a csproj file."
  (let ((project-groups (xml-get-children project-node 'PropertyGroup))
        constants)
    (dolist (project-group project-groups)
      (setq constants (car (xml-get-children project-group 'DefineConstants)))
      (when constants
        (setq constants (nth 2 constants)
              constants (split-string constants ";"))))
    constants))

(defun meta-net--grab-assembly-xml (project-node)
  "Return a list of path that are assembly xml.

Argument PROJECT-NODE is the root node from a csproj file."
  (let ((item-groups (xml-get-children project-node 'ItemGroup))
        refs hint-path attr-include xml)
    (dolist (item-group item-groups)
      (setq refs (xml-get-children item-group 'Reference))
      (dolist (ref refs)
        (setq attr-include (xml-get-attribute ref 'Include)
              hint-path (nth 2 (car (xml-get-children ref 'HintPath))))
        (unless (file-exists-p hint-path)  ; Convert relative path to absolute path
          (setq hint-path (f-join (meta-net--project-current) hint-path)))
        (setq hint-path (f-swap-ext hint-path "xml"))
        (when (file-exists-p hint-path)  ; file must exists
          (meta-net-create-entry-xml hint-path)
          (push hint-path xml))))
    xml))

(defun meta-net--parse-csproj-xml (path)
  "Parse a csproj xml from PATH and return data in hash table.

Hash table includes these following keys,

   * constants - A list of define constans
   * xml       - A list of assembly xml path

You can access these data through variable `meta-net-csproj'."
  (let* ((result (ht-create)) (parse-tree (xml-parse-file path))
         (project-node (assq 'Project parse-tree))
         constants xml)
    (setq constants (meta-net--grab-define-constants project-node))
    (ht-set result 'constants constants)  ; add `constants' it to data
    (setq xml (meta-net--grab-assembly-xml project-node))
    (ht-set result 'xml xml)  ; add `xml' it to data
    result))

(defun meta-net--grab-xml-members (doc-node)
  "Return members data from assembly xml.

Argument DOC-NODE is the root from assembly xml file."
  (let* ((result (ht-create))
         (members-node (car (xml-get-children doc-node 'members)))
         (members (xml-get-children members-node 'member))
         type-name  ; we use this as a key
         type-data  ; this as a data from type-name
         name summary-node para summary params)
    (dolist (member members)
      (meta-net-debug "\f")
      (meta-net-debug "%s" member)
      (setq name (xml-get-attribute member 'name)
            summary-node (car (xml-get-children member 'summary))
            summary (nth 2 summary-node)
            para (nth 3 summary-node))
      (when para (setq summary (nth 2 para)))
      (when summary (setq summary (string-trim summary)))
      (meta-net-debug "---------")
      (meta-net-debug "name: %s" name)
      (meta-net-debug "summary: `%s`" summary)
      (cond ((string-match-p meta-net--tag-type name)
             (setq type-name (s-replace meta-net--tag-type "" name)
                   type-data (ht-create))
             (ht-set result type-name type-data))
            ((string-match-p meta-net--tag-method name)
             (ht-set type-data 'method )
             )
            )
      )
    result))

(defun meta-net--parse-assembly-xml (path)
  "Parse a assembly (dll) xml from PATH and return data in hash table.

Hash table includes these following keys,

   * assembly  - Name of the assembly xml
   * data      - Hash table that use `type` key

You can access these data through variable `meta-net-xml'."
  (let* ((result (ht-create))
         (parse-tree (xml-parse-file path))
         (doc-node (assq 'doc parse-tree))
         (assembly (car (xml-get-children doc-node 'assembly)))
         data)
    (when assembly (ht-set result 'assembly assembly))
    (setq data (meta-net--grab-xml-members doc-node))
    (ht-set result 'data data)
    result))

;;;###autoload
(defun meta-net-read-project (&optional force)
  "Read .NET csproj from current project.

If argument FORCE is non-nil, refresh cache and rebuild data cleanly.

P.S. Please call the function under a project."
  (when force (setq meta-net-csproj-current nil))
  (if meta-net-csproj-current
      (user-error "Data has been built, pass FORCE with t to rebuild")
    (let ((project (meta-net--project-current)) (path (f-parent (buffer-file-name)))
          csprojs)
      (if (not project) (user-error "Path is not under project root: %s" path)
        (meta-net--walk-path
         path
         (lambda (current)  ; current is the path of walking path
           (setq csprojs (ht-get meta-net-projects current))  ; get csporj files if already exists
           ;; if exists, we don't need to read it again
           (if (and csprojs (not force))  ; if force, we need to refresh it
               (setq meta-net-csproj-current current)  ; records the key (current)
             (setq csprojs (f--files current (equal (f-ext it) "csproj")))
             (when csprojs  ; found csproj files in `current' directory
               (setq meta-net-csproj-current current)  ; record it's key
               (ht-set meta-net-projects current csprojs)
               (meta-net-create-entry-csporj csprojs))))
         project)))
    (meta-net-build-data force)))

(defun meta-net-create-entry-csporj (csprojs)
  "Create new csproj entry from current buffer.

Argument CSPROJS is a list of csporj files for use to create.

P.S. Use this carefully, this will overwrite the existing key with null."
  (dolist (entry csprojs)
    (meta-net-log "Create csporj entry: `%s`" entry)
    (ht-set meta-net-csproj entry nil)))

(defun meta-net-create-entry-xml (path)
  "Create new xml entry (PATH) from current buffer.

P.S. Use this carefully, this will overwrite the existing key with null."
  (meta-net-log "Create xml entry: `%s`" path)
  (ht-set meta-net-xml path nil))

(defun meta-net-build-data (&optional force)
  "Read all csproj files and read all assembly xml files to usable data."
  (let ((built t))
    ;; Access csporj to get assembly information including the xml path
    (let ((keys-csproj (ht-keys meta-net-csproj)) result)
      (dolist (key keys-csproj)                              ; key, is csporj path
        (when (or force (not (ht-get meta-net-csproj key)))  ; if it hasn't build, build it
          (setq result (meta-net--parse-csproj-xml key))     ; start building data
          (ht-set meta-net-csproj key result)
          (setq built nil))))
    ;; Build assembly xml data to cache
    (let ((keys-xml (ht-keys meta-net-xml)) result)
      (dolist (key keys-xml)                                ; key, is xml path
        (when (or force (not (ht-get meta-net-xml key)))    ; if it hasn't build, build it
          (setq result (meta-net--parse-assembly-xml key))  ; start building data
          (ht-set meta-net-xml key result)
          (setq built nil))))
    (if built (message "Everything up to date, no need to rebuild")
      (message "Done rebuild solution for project: `%s`" (meta-net--project-current)))))

;;
;; (@* "CsProj" )
;;

(defun meta-net-csporj-files (&optional project)
  "Return a list of csporj files.

See variable `meta-net-projects' description for argument PROJECT."
  (ht-get meta-net-projects (or project meta-net-csproj-current)))

(defun meta-net-csproj-names (&optional project)
  "Return a list of csproj names.

See variable `meta-net-projects' description for argument PROJECT."
  (let (solutions)
    (dolist (path (meta-net-csporj-files project))
      (push (f-base path) solutions))
    (reverse solutions)))

(defun meta-net--get-csproj (path key)
  "Return csproj data by it's PATH with KEY."
  (if-let ((data (ht-get meta-net-csproj path)))
      (ht-get data key)
    (user-error "CsProj data not found, %s => `%s`" key path)))

(defun meta-net-define-constants (path)
  "Return define constants from a CSPROJ file."
  (meta-net--get-csproj path 'constants))

(defun meta-net-csproj-xmls (path)
  "Return list of assembly xml files.

Argument PATH is the csproj path that points to it file."
  (meta-net--get-csproj path 'xml))

;;
;; (@* "Xmls" )
;;

(defun meta-net--get-xml (path key)
  "Return xml data by it's PATH with KEY."
  (if-let ((data (ht-get meta-net-xml path)))
      (ht-get data key)
    (user-error "Xml data not found, %s => `%s`" key path)))

(defun meta-net-xml-assemly-name (path)
  "Return the name of the assembly.

Argument PATH is the path points to assembly xml file."
  (meta-net--get-xml path 'assembly))

(defun meta-net-xml-data (path)
  "Return the data of the assembly.

Argument PATH is the path points to assembly xml file."
  (meta-net--get-xml path 'data))

(provide 'meta-net)
;;; meta-net.el ends here
