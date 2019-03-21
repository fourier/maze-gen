(in-package :maze-gen)

(defun sidewinder (grid)
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
                   (cond ((or (null n) (= dir 1)) (cell-link c e))
                         ;; can't go east, close run
                         ((or (null e) (= dir 0))
                              (close-run run)
                              (setf run nil))))))))
    (grid-map-row grid #'sidewinder-row)))
