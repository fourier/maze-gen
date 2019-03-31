(in-package :maze-gen)

(defun binary-tree (grid)
  "Binary tree maze generation algorithm.
Returns the copy of the grid with boundaries
removed to form a maze"
  (flet ((bin-tree-cell (cell)
           ;; get neighbors - northern and eastern
           (let ((n (cell-get-neighbour cell 'north))
                 (e (cell-get-neighbour cell 'east))
                 ;; set the direction to "dig",
                 ;; 0 - north, 1 - east
                 (dir (random 2)))
             ;; perform digging according to the algorithm
             ;; accounting border elements
             (cond ;; can't go north, go east
                   ((null n) (cell-link cell e))
                   ;; can't go east, go north 
                   ((null e) (cell-link cell n))
                   ;; rest - cointoss
                   (t (cell-link cell (if (= dir 0) n e)))))))
    (grid-map grid #'bin-tree-cell)))
