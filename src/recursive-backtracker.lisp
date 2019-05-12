(in-package :maze-gen)

(defun recursive-backtracker (grid)
  "Recursive backtracker maze generation algorithm. This is
basically the random walk algorithm with constraints.
Returns the copy of the grid with boundaries
removed to form a maze"
  (let ((visited (make-hash-table))
        (stack nil))
    (labels ((visit (c) (setf (gethash c visited) t))
             (visited (c) (gethash c visited))
             (not-visited-neighbours (c)
               (remove-if #'visited (cell-neighbours c)))
             (backtrack ()
               "Pop from the stack until we find not visited neighbour"
               (loop while stack
                     for top = (pop stack)
                     when (not (null (not-visited-neighbours top)))
                       do (return top)
                     finally (return nil))))
      (loop with start-cell = (grid-random-cell grid)
            for cell = start-cell then (backtrack)
            while cell
            for path = (random-walk cell
                                    (lambda (c)
                                      (null
                                       (not-visited-neighbours c)))
                                    :visitor
                                    (lambda (c)
                                      (visit c)
                                      (push c stack))
                                    :constraint-if
                                    #'visited)
            do
               (reduce
                (lambda (c1 c2) (cell-link c1 c2) c2)
                path)
            finally (return grid)))))
        
