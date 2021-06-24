;;; meta-net.el --- Parse .NET assembly's XML  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-24 21:17:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Parse .NET assembly's XML
;; Keyword: assembly xml utility
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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

(require 'xml)

(defgroup meta-net nil
  "Parse .NET assembly's XML."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-net"))

(defconst meta-view--tag-property "P:"
  "Tag represent property declaration.")

(defconst meta-view--tag-method "M:"
  "Tag represent method/function declaration.")

(defconst meta-view--tag-type "T:"
  "Tag represent type (enum, class, interface) declaration.")

(defconst meta-view--tag-enum "F:"
  "Tag represent enum item.")

(defun meta-view--parse-xml (path)
  ""
  (let* ((parse-tree (xml-parse-file path))
         (doc-mode (assq 'doc parse-tree))
         (assembly-node (car (xml-get-children doc-node 'assembly)))
         (member-nodes (xml-get-children doc-node 'member)))
    ))

(provide 'meta-net)
;;; meta-net.el ends here
