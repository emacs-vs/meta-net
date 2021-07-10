;;; meta-net-test.el --- logms tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jen-Chieh Shen

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords:

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

;; Tests for meta-net

;;; Code:

(require 'meta-net)
(require 'ert)
(require 'debug)

;;
;; (@* "CsProj" )
;;

(ert-deftest meta-net-test--parse-csproj--unity-csharp ()
  (should (meta-net--parse-csproj-xml "./csproj/Unity/Assembly-CSharp.csproj")))

(ert-deftest meta-net-test--parse-csproj--unity-csharp-editor ()
  (should (meta-net--parse-csproj-xml "./csproj/Unity/Assembly-CSharp-Editor.csproj")))

;;
;; (@* "Xml" )
;;

(ert-deftest meta-net-test--parse-xml--json-dotnet ()
  (should (meta-net--parse-assembly-xml "./xml/Newtonsoft.Json.xml")))

(ert-deftest meta-net-test--parse-xml--unity-editor ()
  (should (meta-net--parse-assembly-xml "./xml/UnityEngine.xml")))

(ert-deftest meta-net-test--parse-xml--unity-engine ()
  (should (meta-net--parse-assembly-xml "./xml/UnityEngine.xml")))

(ert-deftest meta-net-test--parse-xml--yaml-dotnet ()
  (should (meta-net--parse-assembly-xml "./xml/YamlDotNet.xml")))

(provide 'meta-net-test)
;;; meta-net-test.el ends here
