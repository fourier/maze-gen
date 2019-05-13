(in-package :maze-gen)

(defun braid (grid percents)
  "Braiding the GRID, leaving only PERCENTS of the original
dead ends.
Example: given the amount of dead ends in the
grid to be 100, the call
MAZE-GEN>  (braid maze 70) will reduce amount of dead ends to about 70. Brad by 100% should not change the amount of dead ends."
  (flet ((braid-cell (cell)
           ;; pickup those who are not linked to our cell,
           ;; so they have a wall between us
           (let* ((boundaries
                    (remove-if (curry #'cell-linked-p cell)
                               (cell-neighbours cell)))
                  ;; out of those select ones with only one link
                  (best (or boundaries
                    (remove-if
                         (lambda (c)
                           (> (length (cell-links c)) 1))
                         boundaries))))
             (when boundaries
               (cell-link cell (random-elt (or best boundaries)))))))
    (loop for deadend in (shuffle (grid-get-deadends grid))
          for rand  = (1+ (random 100))
          when (> rand percents)
            do (braid-cell deadend)
          finally (return grid))))
