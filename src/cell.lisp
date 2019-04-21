(in-package :maze-gen)

(defclass cell ()
  ((row :initarg :row :initform 0 :reader cell-row :type fixnum)
   (col :initarg :col :initform 0 :reader cell-col :type fixnum)
   (neighbors :initform (make-hash-table))
   (links :initform (make-hash-table)))
  (:documentation "Cell of the maze. Cell has attributes such as row, column, neighbors etc"))


(defmethod cell-link ((self cell) other &optional (bidi t))
  "Establish a link between 2 cells. Optionally the link is bidirectional (true by default).
Link means no borders between cells"
  (when other
    (setf (gethash other (slot-value self 'links)) t)
    (when bidi (cell-link other self nil)))
  (values))


(defmethod cell-unlink ((self cell) other &optional (bidi t))
  "Remove link between 2 cells. Optionally (true by default) removes in both directions"
  (when other
    (remhash other (slot-value self 'links))
    (when bidi (cell-unlink other self nil)))
  (values))


(defmethod cell-linked-p ((self cell) other)
  "Check if the cell linked to another"
  (gethash other (slot-value self 'links)))


(defmethod cell-links ((self cell))
  "Get a list of all cells one can go from the given cell"
  (loop for key being the hash-keys of (slot-value self 'links)
        collect key))


(defmethod cell-neighbours ((self cell))
  "Get all cell neighbours (east,west,north,south)"
  (loop for val being the hash-values of (slot-value self 'neighbors)
        collect val))


(defmethod cell-get-neighbour ((self cell) direction)
  "Get a cell's neigbour, direction is one of symbols
NORTH, SOUTH, EAST, WEST"
  (gethash direction (slot-value self 'neighbors)))


(defmethod cell-set-neighbour ((self cell) direction other)
    "Set a cell's neigbour, direction is one of symbols
NORTH, SOUTH, EAST, WEST"
  (setf (gethash direction (slot-value self 'neighbors)) other))


;; end
