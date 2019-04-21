(in-package :maze-gen)

(defun aldous-broder (grid)
  "Aldous-Broder maze generation algorithm. This is
basically the random walk algorithm.
Returns the copy of the grid with boundaries
removed to form a maze"
  (loop with visited = (make-hash-table)
        with total-count = (* (grid-nrows grid)
                              (grid-ncols grid))
        with start-cell = (grid-random-cell grid)
        for visited-count = (hash-table-count visited)
        while (< visited-count total-count)
        for prev-cell = start-cell then cell
        for neighbours = (remove-if #'null (cell-neighbours prev-cell))
        for cell = (elt neighbours (random (length neighbours)))
        do (setf (gethash prev-cell visited) t)
        when (not (gethash cell visited))
        do (cell-link cell prev-cell)
        finally (return grid)))
        
