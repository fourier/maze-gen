#|
  This file is a part of maze-gen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2019
|#
(in-package :cl-user)
(defpackage maze-gen-asd
  (:use :cl :asdf))
(in-package :maze-gen-asd)

(defsystem maze-gen
  :version "0.1"
  :author ""
  :license ""
  :depends-on (alexandria split-sequence 3d-vectors 3d-matrices ppath)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "cell")
                 (:file "grid")
                 (:file "dungeon")
                 (:file "binary-tree")
                 (:file "sidewinder")
                 (:file "trenchbroom")
                 #+lispworks(:file "ui"))))
  :description "Procedural maze generation")
