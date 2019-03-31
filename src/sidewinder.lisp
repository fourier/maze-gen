(in-package :maze-gen)

(defun sidewinder (grid)
  "Sidewinder maze generation algorithm.
Returns the copy of the grid with boundaries
removed to form a maze"
  (labels ((sample (run)
             (nth (random (length run)) run))
           (close-run (run)
             (let* ((c (sample run))
                    (n (cell-get-neighbour c 'north)))
               (cell-link c n)))
           (sidewinder-row (row)
             ;; initialize current run
             (let (run)
               (dolist (c row)
                 (let ((dir (random 2))
                       (n (cell-get-neighbour c 'north))
                       (e (cell-get-neighbour c 'east)))
                   ;; add current cell to run
                   (push c run)
                   ;; check different scenarios
                   ;; first can't go north, make a way eastwards
                   (cond ((null n) (cell-link c e))
                         ;; can't go east, close the run                         
                         ((null e) (close-run run) (setf run nil))
                         ;; coin shows go east
                         ((= dir 1) (cell-link c e))
                         ;; coin shows go north (close the run)
                         ((= dir 0) (close-run run) (setf run nil))))))))
    (grid-map-row grid #'sidewinder-row)))
