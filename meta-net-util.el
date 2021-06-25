;;; meta-net-util.el --- Utility module for meta-net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-25 13:15:07

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
;; Utility module for meta-net.
;;

;;; Code:

(require 'f)

(defun meta-net-util-project-current ()
  "Return the current project root."
  (cdr (project-current)))

(defun meta-net-util-walk-path (path fnc &optional start-path)
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

(provide 'meta-net-util)
;;; meta-net-util.el ends here
