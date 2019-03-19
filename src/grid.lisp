(in-package :maze-gen)

(defclass grid ()
  ((rows :initarg :rows :initform *max-rows* :type fixnum :reader grid-nrows)
   (cols :initarg :cols :initform *max-cols* :type fixnum :reader grid-ncols)
   (grid :initform nil)))

(defmethod initialize-instance :after ((self grid) &key &allow-other-keys)
  "Constructor for the pack-file class"
  (with-slots (rows cols grid) self
    (setf grid (make-array (list rows cols) :element-type 'cell))
    (loop for i below rows do
          (loop for j below cols do
                (setf (aref grid i j) (make-instance 'cell :row i :col j))))
     (loop for i below rows do
          (loop for j below cols
                for c = (aref grid i j)
                do
                (cell-set-neighbour c 'north (grid-cell self (1- i) j))
                (cell-set-neighbour c 'south (grid-cell self (1+ i) j))
                (cell-set-neighbour c 'west (grid-cell self i (1- j)))
                (cell-set-neighbour c 'east (grid-cell self i (1+ j)))))))
          

(defmethod grid-cell ((self grid) row col)
  (handler-case (aref (slot-value self 'grid) row col) (error nil)))


(defmethod grid-size ((self grid))
  (* (slot-value self 'rows) (slot-value self 'cols)))


(defmethod grid-random-cell ((self grid))
  (let ((r (random (slot-value self 'rows)))
        (c (random (slot-value self 'cols))))
    (grid-cell self r c)))


(defmethod grid-map-row ((self grid) fn)
  (with-slots (rows cols grid) self
    (loop for i below rows do
          (funcall fn 
                   (loop for j below cols
                         collect (aref grid i j))))))


(defmethod grid-map ((self grid) fn)
  (with-slots (rows cols grid) self
    (loop for i below rows do
          (loop for j below cols do
                (funcall fn (aref grid i j))))))



(defmethod grid-draw ((self grid) &optional (stream *standard-output*))
  (flet ((draw-row (row)
           ;; draw the grid line itself, moving to east and
           ;; adding a wall there the cells aren't connected
           (format stream "|")
           (dolist (c row)
             (format stream " ")
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
                         " +"
                         "-+")))
           (format stream "~%")))
    (format stream "+")
    (loop for i below (grid-ncols self)
          do (format stream "-+")
          finally (format stream "~%"))
    (grid-map-row self #'draw-row)))
         

;; end
