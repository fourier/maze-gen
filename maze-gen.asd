#|
  This file is a part of maze-gen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2019
|#
(in-package :cl-user)
(defpackage maze-gen-asd
  (:use :cl :asdf))
(in-package :maze-gen-asd)

(defsystem maze-gen
  :version "0.4"
  :author "Alexey Veretennikov"
  :license ""
  :depends-on (alexandria #-lispworks unix-opts)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "cell")
                 (:file "grid")
                 (:file "dungeon")
                 (:file "random-walk")
                 (:file "binary-tree")
                 (:file "sidewinder")
                 (:file "aldous-broder")
                 (:file "wilson")
                 (:file "dijkstra")
                 (:file "hunt-and-kill")
                 (:file "recursive-backtracker")
                 (:file "print")
                 (:file "trenchbroom")
                 #+lispworks(:file "ui")
                 #-lispworks(:file "cmdline"))))
  :description "Procedural maze generation")

