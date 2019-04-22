(in-package :maze-gen)

(defun random-walk (from finish-if &key (allow-loops nil))
  "Create random path on a grid from the element FROM
until the predicte FINISH-IF is t on next element of
the generated random path. The cell on which the
FINISH-IF predicate is true is also added to the end
of the result list"
  (flet ((walk (from)
           (random-elt (remove-if #'null (cell-neighbours from)))))
    (loop with result = (list from)
          for current = (car result)
          for next = (walk current)
          until (funcall finish-if next)
          do
          ;; loop avoidance
          (let ((mem (member next result)))
            (if (and mem (not allow-loops))
                (setf result mem)
                (push next result)))
          finally (return (nreverse (push next result))))))
