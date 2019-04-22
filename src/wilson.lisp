(in-package :maze-gen)

     
(defun wilson (grid)
  "Wilson's maze generation algorithm. This is
a random walk algorithm.
Returns the copy of the grid with boundaries
removed to form a maze"
  (let ((unvisited nil))
    (grid-map grid (lambda (c) (push c unvisited)))
    (pop unvisited)
    (loop until (emptyp unvisited)
          for cell = (random-elt unvisited)
          for path = (random-walk cell (lambda (c) (not (find c unvisited))))
          do
          (reduce (lambda (c1 c2) (cell-link c1 c2) c2) path)
          (setf unvisited (remove-if (lambda (c) (member c path)) unvisited))
          finally (return grid))))
        
