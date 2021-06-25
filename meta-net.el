;;; meta-net.el --- Parse .NET assembly's XML  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 21:17:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Parse .NET assembly's XML
;; Keyword: assembly xml utility
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (ht "2.3"))
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

(defvar meta-net-csproj (ht-create)
  "Store all csproj entry to it's data in hash table.

Data is in (path . data), data is csproj and it's in xml.")

(defvar-local meta-net-csproj-current nil
  "Store csproj files for each existing buffer.

Local variable stores a list of csproj path, please use the path as id to
variable `meta-net-csproj'.")

(defvar meta-net-xml (ht-create)
  "Store all assembly xml files to it's data in hash table.

Data is in (path . data), data is xml that records assembly's information.")

(defun meta-net--parse-csproj-xml (path)
  "Parse a csproj xml from PATH."
  (let* ((parse-tree (xml-parse-file path))
         (project-node (assq 'Project parse-tree))
         (item-groups (xml-get-children project-node 'ItemGroup))
         reference)
    (dolist (item-group item-groups)
      (setq reference (xml-get-children item-group 'Reference))
      (when reference
        (jcs-print reference))
      )))

(defun meta-net--parse-assembly-xml (path)
  "Parse a assembly (dll) xml from PATH."
  (let* ((parse-tree (xml-parse-file path))
         (doc-node (assq 'doc parse-tree))
         (assembly (car (xml-get-children doc-node 'assembly)))
         (members (xml-get-children doc-node 'members)))
    (jcs-print assembly)
    (jcs-print members)
    ))

;;;###autoload
(defun meta-net-read-project ()
  "Read .NET csproj from current project."
  (let ((project (meta-net-util-project-current)) (path (buffer-file-name)) csprojs)
    (if (not project) (user-error "Path is not under project root: %s" path)
      (meta-net-util-walk-path
       path
       (lambda (current)
         (setq csprojs (f--files current (equal (f-ext it) "csproj")))
         (when csprojs
           (setq meta-net-csproj-current csprojs)
           (meta-net-create-entry)))
       project)))
  (meta-net-build-data))

(defun meta-net-create-entry ()
  "Create new entry from current buffer."
  (dolist (entry meta-net-csproj-current) (ht-set meta-net-csproj entry nil)))

(defun meta-net-build-data ()
  "Read "
  (let ((keys (ht-keys meta-net-csproj)))
    (dolist (key keys)
      (unless (ht-get meta-net-csproj key)
        (ht-set meta-net-csproj key (meta-net--parse-csproj-xml key))))))

(provide 'meta-net)
;;; meta-net.el ends here
