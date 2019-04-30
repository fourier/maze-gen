#|
  This file is a part of maze-gen project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2019
|#
(in-package :maze-gen)

(defparameter *algorithms*
  '(("binary" . binary-tree)
    ("sidewinder" . sidewinder)
    ("aldous-broder" . aldous-broder)
    ("wilson" . wilson)
    ("hunt-and-kill" . hunt-and-kill)))
  

(defparameter *long-description*
  (format nil 
          "The program prints the generated random maze to the output
or the file provided. The supported algorithms (specified by --algo option) are:~%~{~a~%~}"
          (mapcar #'car *algorithms*)))


(defun parse-algo (algo)
  "Parse the algorithm string and return the corresponding function"
  (if-let (fun (assoc algo *algorithms* :test #'string=))
    (cdr fun)
    (error "Unknown algorithm")))


(opts:define-opts
  (:name :help
   :description "print help"
   :short #\h
   :long "help")
  (:name :path
   :description "show longest path"
   :short #\p
   :long "path")
  (:name :ends
   :description "show start and end of the longest path"
   :short #\e
   :long "ends")
  (:name :rows
   :description "number of ROWS in the maze, 15 by default"
   :short #\r
   :long "rows"
   :arg-parser #'parse-integer
   :meta-var "ROWS")
  (:name :cols
   :description "number of COLUMNS in the maze, 15 by default"
   :short #\c
   :long "columns"
   :arg-parser #'parse-integer
   :meta-var "COLS")  
  (:name :algo
   :description "use algorithm ALGO, by default hunt-and-kill, see below"
   :short #\a
   :long "algo"
   :arg-parser #'parse-algo
   :meta-var "ALGO")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))


(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))


;;;;;;;;;;;; Application entry point ;;;;;;;;;;;;




(defun main-cmdline ()
  (let ((nrows 15)
        (ncols 15)
        (algo #'hunt-and-kill)
        (fname nil)
        (show-start-end nil)
        (show-path nil))
    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s~%"
                    (opts:raw-arg condition)
                    (opts:option condition)))
          (opts:missing-required-option (con)
            (format t "fatal: ~a~%" con)
            (opts:exit 1)))
      (declare (ignore free-args))
      ;; Process command line options
      (when-option (options :help)
        (opts:describe
         :prefix "Procedural maze generator."
         :suffix *long-description*
         :usage-of "maze-gen"
         :args     "[FREE-ARGS]"))
      (when-option (options :path)
        (setf show-path t))
      (when-option (options :ends)
        (setf show-start-end t))
      (when-option (options :rows)
        (setf nrows it)
        (when (< it 1)
          (format t "fatal: Number of rows could not be less than 1, given: ~a~%" it)
          (opts:exit 1)))
      (when-option (options :cols)
        (setf ncols it)
        (when (< it 1)
          (format t "fatal: Number of columns could not be less than 1, given: ~a~%" it)
          (opts:exit 1)))
      (when-option (options :output)
        (setf fname (getf options :output)))
      (when-option (options :algo)
        (setf algo (getf options :algo))))
    ;; seed the random state
    (setf *random-state* (make-random-state t))
    ;; ok time to print the maze!
    (grid-print (funcall algo (make-instance 'grid
                                             :rows nrows
                                             :cols ncols))
                :print-shortest-path show-path
                :print-start-end show-start-end)))
