#|
  This file is a part of maze-gen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2019

  We reserve all rights in this file and in the information
  contained therein. Reproduction, use or disclosure to third
  parties without express authority is strictly forbidden.
|#

(in-package "CL-USER")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #+:MSWINDOWS "C:/apps/quicklisp/setup.lisp"
					  #-:MSWINDOWS (merge-pathnames ".quicklisp/setup.lisp"
                                                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
(push (pathname #+:MSWINDOWS "C:/Sources/lisp" #-:MSWINDOWS "~/Sources/lisp") ql:*local-project-directories*)
(ql:register-local-projects)


;;; Load the application:

(ql:quickload :maze-gen)

#-lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (sb-ext:save-lisp-and-die "maze-gen"
                            :toplevel #'maze-gen::main-cmdline
                            :executable t
                            :save-runtime-options t)
  (quit))


