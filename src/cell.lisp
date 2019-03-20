(in-package :maze-gen)

(defclass cell ()
  ((row :initarg :row :initform 0 :reader cell-row :type fixnum)
   (col :initarg :col :initform 0 :reader cell-col :type fixnum)
   (east :initarg :north :initform nil :accessor cell-east :type 'cell)
   (neighbors :initform (make-hash-table))
   (links :initform (make-hash-table))))


(defmethod cell-link ((self cell) other &optional (bidi t))
  (when other
    (setf (gethash other (slot-value self 'links)) t)
    (when bidi (cell-link other self nil)))
  (values))


(defmethod cell-unlink ((self cell) other &optional (bidi t))
  (when other
    (remhash other (slot-value self 'links))
    (when bidi (cell-unlink other self nil)))
  (values))


(defmethod cell-linked-p ((self cell) other)
  (gethash other (slot-value self 'links)))


(defmethod cell-links ((self cell))
  (loop for key being the hash-keys of (slot-value self 'links)
        collect key))


(defmethod cell-neigbours ((self cell))
  (loop for val being the hash-values of (slot-value self 'neigbours)
        collect val))


(defmethod cell-get-neighbour ((self cell) direction)
  (gethash direction (slot-value self 'neighbors)))


(defmethod cell-set-neighbour ((self cell) direction other)
  (setf (gethash direction (slot-value self 'neighbors)) other))


;; end
