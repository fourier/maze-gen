(in-package :maze-gen)

(defun hunt-and-kill (grid)
  "Hunt-and-Kill maze generation algorithm. This is
basically the random walk algorithm with constraints.
Returns the copy of the grid with boundaries
removed to form a maze"
  (let ((visited (make-hash-table)))
    (labels ((not-visited-neighbours (c)
               (remove-if
                (lambda (c)
                  (or (null c)
                      (gethash c visited)))
                (cell-neighbours c)))
             (visit (c) (setf (gethash c visited) t))
             (visited (c) (gethash c visited))
             (hunt ()
               (loop named hunt-loop
                     for i below (grid-nrows grid)
                     do
                        (loop for j below (grid-ncols grid)
                              for c = (grid-cell grid i j)
                              for neighbours = (not-visited-neighbours c)
                              when (and neighbours (visited c))
                                do
                                   (cell-link c
                                              (random-elt neighbours))
                                   (return-from hunt-loop c))
                     finally (return-from hunt-loop nil))))
      (loop with total-count = (* (grid-nrows grid)
                                  (grid-ncols grid))
            with start-cell = (grid-random-cell grid)
            for visited-count = (hash-table-count visited)
            while (< visited-count total-count)
            for cell = start-cell then (hunt)
            for path = 
                     (progn
                       (visit cell)
                       (random-walk cell
                                    (lambda (c)
                                      (visit c)
                                      (null
                                       (not-visited-neighbours c)))
                                    :constraint-if
                                    (lambda (c)
                                      (gethash c visited))))
            do
               (reduce
                (lambda (c1 c2) (cell-link c1 c2) c2)
                path)
               (dolist (c path) (setf (gethash c visited) t))
            finally (return grid)))))
        
