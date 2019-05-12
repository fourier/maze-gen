(in-package :maze-gen)


(defmethod grid-print-generic ((self grid) &optional
                                             (cell-draw (lambda (c s) (declare (ignore c)) (format s "   ")))
                                             (stream *standard-output*)) 
  "Print the grid as ascii-graphics to the STREAM"
  (flet ((draw-row (row)
           ;; draw the grid line itself, moving to east and
           ;; adding a wall there the cells aren't connected
           (format stream "|")
           (dolist (c row)
             (funcall cell-draw c stream)
             (format stream
             (if (cell-linked-p c (cell-get-neighbour c 'east))
                 " "
                 "|")))
           (format stream "~%")
           ;; now write the southern separator line, again
           ;; verifying if southern cells are connected
           (format stream "+")
           (dolist (c row)
             (format stream
                     (if (cell-linked-p c (cell-get-neighbour c 'south))
                         "   +"
                         "---+")))
           (format stream "~%")))
    ;; output first line - upper wall
    (format stream "+")
    (loop for i below (grid-ncols self)
          do (format stream "---+")
          finally (format stream "~%"))

    
    
    (grid-map-row self #'draw-row)))

(defmethod grid-draw-numbers ((self grid) weights &optional (stream *standard-output*))
  "weidghts - a hash table with key is a cell and value is an integer"
  (let* ((max-width (floor (1+ (log (grid-size self) 10))))
         (fmt-str (format nil "~~{~~~dd ~~}~~%" max-width)))
    (flet ((draw-row (row)
             (format stream fmt-str
                     (mapcar (lambda (c) (gethash c weights)) row))))
      (grid-map-row self #'draw-row))))


(defmethod grid-print ((self grid) &key (print-start-end nil) (print-shortest-path nil))
  (let ((print-func
          (cond ((and print-start-end print-shortest-path)
                 (multiple-value-bind (start-end distances) (dijkstra-longest-path self)
                   (let ((shortest-path (butlast (cdr
                                                  (dijkstra-shortest-path self
                                                                          (car start-end)
                                                                          (cdr start-end)
                                                                distances)))))
                     (lambda (c s)
                       (format s 
                               (cond ((eql c (car start-end)) " S ")
                                     ((eql c (cdr start-end)) " E ")
                                     ((member c shortest-path) " * ")
                                     (t "   ")))))))
                (print-start-end 
                 (multiple-value-bind (start-end distances) (dijkstra-longest-path self)
                   (declare (ignore distances))
                   (lambda (c s)
                     (format s 
                             (cond ((eql c (car start-end)) " S ")
                                   ((eql c (cdr start-end)) " E ")
                                   (t "   "))))))
                (print-shortest-path
                 (multiple-value-bind (start-end distances) (dijkstra-longest-path self)
                   (let ((shortest-path (dijkstra-shortest-path self
                                                                (car start-end)
                                                                (cdr start-end)
                                                                distances)))
                     (lambda (c s)
                       (format s 
                               (if (member c shortest-path) " * " "   "))))))
                (t 
                 (lambda (c s) (declare (ignore c)) (format s "   "))))))
    (grid-print-generic self print-func)))




;; end
