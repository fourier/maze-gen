(in-package :maze-gen)

(defclass grid ()
  ((rows :initarg :rows :initform *max-rows* :type fixnum :reader grid-nrows)
   (cols :initarg :cols :initform *max-cols* :type fixnum :reader grid-ncols)
   (grid :initform nil))
  (:documentation "The GRID of the maze - the 2d array of cells"))

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
  "Return the cell from the grid by row and column.
If out of bounds return nil"
  (handler-case (aref (slot-value self 'grid) row col) (error nil)))


(defmethod grid-size ((self grid))
  "Return the number of cells in the grid"
  (* (slot-value self 'rows) (slot-value self 'cols)))


(defmethod grid-random-cell ((self grid))
  "Return the random cell of the grid"
  (let ((r (random (slot-value self 'rows)))
        (c (random (slot-value self 'cols))))
    (grid-cell self r c)))


(defmethod grid-map-row ((self grid) fn)
  "For each row of the grid call the function FN.
The row is provided as a list of cells"
  (with-slots (rows cols grid) self
    (loop for i below rows do
          (funcall fn 
                   (loop for j below cols
                         collect (aref grid i j)))))
  self)


(defmethod grid-map ((self grid) fn)
  "For each cell of the grid call the function FN"
  (with-slots (rows cols grid) self
    (loop for i below rows do
          (loop for j below cols do
                (funcall fn (aref grid i j)))))
  self)



(defmethod grid-print ((self grid) &optional
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


(defmethod grid-make-walls ((self grid))
  "Create the list of walls. A wall will be the
combination of coordinates (x1 y1 x2 y2), where
the coordinates basically indexes, i.e.
0 0 0 1"
  (let* ((nrows (grid-nrows self))
         (ncols (grid-ncols self))
         ;; first we create the northen big wall and
         ;; western big wall and push them to result
         (result (list (list 0 0 ncols 0) (list 0 0 0 nrows))))
    (dotimes (r nrows)
      (dotimes (c ncols)
        (let* ((cell (grid-cell self r c))
               (s (cell-get-neighbour cell 'south))
               (e (cell-get-neighbour cell 'east)))
          ;; Example:
          ;; +---+---+---+
          ;; |           |
          ;; +---+   +---+
          ;; |           |
          ;; +   +---+   +
          ;; | X |       |
          ;; +---+---+---+
          ;; cell X: c = 0 r = 2 
          ;; walls: south x1 = 0 y1 = 3, x2 = 1 y2 = 3
          ;;        east: x1 = 1 y1 = 2, x2 = 1 y2 = 3
          ;; eastern boundary - if no eastern cell linked               
          (unless (cell-linked-p cell e)
            (push (list (1+ c) r (1+ c) (1+ r)) result))
          ;; southern boundary - if no southern cell linked
          (unless (cell-linked-p cell s)
            (push (list c (1+ r) (1+ c) (1+ r)) result)))))
    result))


(defun walls-optimize (walls-list)
  "Try to optimize walls list by connecting close walls,
i.e. then the end of one wall is the beginning of another.
Will return the new list optimized 1 pass"
  (warn "Not implemented")
  (let ((new-walls-list))
    walls-list))


;; end
