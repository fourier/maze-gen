(in-package :maze-gen)

(defun random-walk (from finish-if &key (allow-loops nil)
                         (constraint-if
                          (lambda (c)
                            (declare (ignore c)) nil)))
  "Create random path on a grid from the element FROM
until the predicte FINISH-IF is t on next element of
the generated random path. The cell on which the
FINISH-IF predicate is true is also added to the end
of the result list
CONSTRAINT-IF is a predicate specifying if the cell
shall be removed from the list of candidates to walk to"
  (flet ((walk (from)
           (let ((cells (remove-if (lambda (c)
                                     (or (null c)
                                         (funcall constraint-if c)))
                                   (cell-neighbours from))))
                 (random-elt cells))))
    (loop with result = (list from)
          for current = (car result)
          for next = (walk current)
          until (or (null next) (funcall finish-if next))
          do
          ;; loop avoidance
          (let ((mem (member next result)))
            (if (and mem (not allow-loops))
                (setf result mem)
                (push next result)))
          finally
             (progn
               (when next (push next result))
               (return (nreverse result))))))
