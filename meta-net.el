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

(require 'meta-net-util)

(defgroup meta-net nil
  "Parse .NET assembly's XML."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-net"))

(defconst meta-net--tag-property "P:"
  "Tag represent property declaration.")

(defconst meta-net--tag-method "M:"
  "Tag represent method/function declaration.")

(defconst meta-net--tag-type "T:"
  "Tag represent type (enum, class, interface) declaration.")

(defconst meta-net--tag-enum "F:"
  "Tag represent enum item.")

(defvar meta-net-projects (ht-create)
  "Store all the project points to csporj files.

This prevents reading the same project and waste it's performance.  Notice
project path here aren't source control path.  It's just the parent path of
all .csproj file so this will work without the source control or one repository
with multiple projects' structure.

Data look like (path . (csporj_1, csproj_2)).")

(defvar meta-net-csproj (ht-create)
  "Store all csproj file entries.

Store data in (path . hash-table); hash-table are data defined in csporj.
See function `meta-net--parse-csproj-xml' to get more information.")

(defvar-local meta-net-csproj-current nil
  "Store csproj files for each existing buffer.

Local variable stores a list of csproj path, please use the path as id to
variable `meta-net-csproj'.")

(defvar meta-net-xml (ht-create)
  "Store all assembly xml files to it's data in hash table.

Store data in (path . hash-table); hash-table are data defined in assembly xml.")

(defvar meta-net-show-log t
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun meta-net-log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when meta-net-show-log
    (let (message-log-max) (apply 'message fmt args))))

;;
;; (@* "Core" )
;;

(defun meta-net--parse-csproj-xml (path)
  "Parse a csproj xml from PATH and return data in hash table.

Data hash table includes these keys,

   * constants - return a list of define constans
   * xml       - return a list of assembly xml path.

You can access these data through variable `meta-net-csproj'."
  (let* ((result (ht-create)) constants xml
         (parse-tree (xml-parse-file path))
         (project-node (assq 'Project parse-tree))
         (item-groups (xml-get-children project-node 'ItemGroup))
         refs hint-path attr-include)
    ;; TODO: Grab define constans information..
    (ht-set result 'constants constants)  ; add `constants' it to data
    (dolist (item-group item-groups)
      (setq refs (xml-get-children item-group 'Reference))
      (dolist (ref refs)
        (setq attr-include (xml-get-attribute ref 'Include)
              hint-path (nth 2 (car (xml-get-children ref 'HintPath))))
        (unless (file-exists-p hint-path)  ; Convert relative path to absolute path
          (setq hint-path (f-join (meta-net-util-project-current) hint-path)))
        (setq hint-path (f-swap-ext hint-path "xml"))
        (when (file-exists-p path)
          (meta-net-create-entry-xml hint-path)
          (push hint-path xml))))
    (ht-set result 'xml xml)  ; add `xml' it to data
    result))

(defun meta-net--parse-assembly-xml (path)
  "Parse a assembly (dll) xml from PATH and return data in hash table."
  (let* ((result (ht-create))
         (parse-tree (xml-parse-file path))
         (doc-node (assq 'doc parse-tree))
         (assembly (car (xml-get-children doc-node 'assembly)))
         (members (xml-get-children doc-node 'members)))
    (jcs-print assembly)
    (when assembly)
    ;;(jcs-print members)
    result))

;;;###autoload
(defun meta-net-read-project (&optional force)
  "Read .NET csproj from current project.

If argument FORCE is non-nil, refresh cache and rebuild data cleanly.

P.S. Please call the function under a project."
  (when force (setq meta-net-csproj-current nil))
  (if meta-net-csproj-current
      (user-error "Data has been built, pass FORCE with t to rebuild")
    (let ((project (meta-net-util-project-current)) (path (f-parent (buffer-file-name)))
          csprojs)
      (if (not project) (user-error "Path is not under project root: %s" path)
        (meta-net-util-walk-path
         path
         (lambda (current)
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
    (meta-net-build-data)))

(defun meta-net-create-entry-csporj (csprojs)
  "Create new csproj entry from current buffer.

Argument CSPROJS is a list of csporj files for use to create.

P.S. Use this carefully since this will overwrite the existing key with null."
  (dolist (entry csprojs)
    (meta-net-log "Create csporj entry: `%s`" entry)
    (ht-set meta-net-csproj entry nil)))

(defun meta-net-create-entry-xml (path)
  "Create new xml entry (PATH) from current buffer.

P.S. Use this carefully since this will overwrite the existing key with null."
  (meta-net-log "Create xml entry: `%s`" path)
  (ht-set meta-net-xml path nil))

(defun meta-net-build-data ()
  "Read all csproj files and read all assembly xml files to usable data."
  (let ((built t))
    ;; Access csporj to get assembly information including the xml path
    (let ((keys-csproj (ht-keys meta-net-csproj)) result)
      (dolist (key keys-csproj)                           ; key, is csporj path
        (unless (ht-get meta-net-csproj key)              ; if it hasn't build, build it
          (setq result (meta-net--parse-csproj-xml key))  ; start building data
          (ht-set meta-net-csproj key result)
          (setq built nil))))
    ;; Build assembly xml data to cache
    (let ((keys-xml (ht-keys meta-net-xml)) result)
      (dolist (key keys-xml)                                ; key, is xml path
        (unless (ht-get meta-net-xml key)                   ; if it hasn't build, build it
          (setq result (meta-net--parse-assembly-xml key))  ; start building data
          (ht-set meta-net-xml key result)
          (setq built nil))))
    (if built (message "Everything up to date, no need to rebuild")
      (message "Done rebuild solution for project: `%s`" (meta-net-util-project-current)))))

(defun meta-net-solution-names ()
  "Return a list of solutions names."
  (let (solutions)
    (dolist (path meta-net-csproj-current) (push (f-base path) solutions))
    (reverse solutions)))

(provide 'meta-net)
;;; meta-net.el ends here
