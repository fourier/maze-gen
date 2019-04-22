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
                      (gethash c visited)))))
             (walk (from)
                 (random-elt (not-visited-neighbours from)))
             (hunt ()
               (loop named hunt-loop
                     for i below (grid-nrows grid)
                     do
                        (loop for j below (grid-ncols grid)
                              for c = (grid-cell grid i j)
                              when (not-visited-neighbours c)
                                do
                                   (return-from hunt-loop c))
                     finally (return-from hunt-loop nil))))
    (loop with total-count = (* (grid-nrows grid)
                                (grid-ncols grid))
          with start-cell = (grid-random-cell grid)
          for visited-count = (hash-table-count visited)
          while (< visited-count total-count)
          for prev-cell = start-cell then cell
          for cell = (walk prev-cell)
          do (setf (gethash prev-cell visited) t)
          when (not (gethash cell visited))
          do (cell-link cell prev-cell)
          finally (return grid))))
        
