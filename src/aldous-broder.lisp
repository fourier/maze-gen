(in-package :maze-gen)

(defun aldous-broder (grid)
  "Aldous-Broder maze generation algorithm. This is
basically the random walk algorithm.
Returns the copy of the grid with boundaries
removed to form a maze"
  (flet ((walk (from)
           (random-elt (cell-neighbours from))))
    (loop with visited = (make-hash-table)
          with total-count = (grid-size grid)
          with start-cell = (grid-random-cell grid)
          for visited-count = (hash-table-count visited)
          while (< visited-count total-count)
          for prev-cell = start-cell then cell
          for cell = (walk prev-cell)
          do (setf (gethash prev-cell visited) t)
          when (not (gethash cell visited))
          do (cell-link cell prev-cell)
          finally (return grid))))
        
