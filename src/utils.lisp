(in-package :maze-gen)

;; The fast array-based fixed size queue, basically ring buffer
;; with 2 indexes - reader and writer
(defclass queue ()
  ((size :initarg :size :initform 0 :type fixnum :reader queue-size
         :documentation "Current size of the queue - a number of enqueued elements")
   (element-type :initarg :element-type :initform t
                 :documentation "Type of the elements in the queue, could be provided for performance reasons")
   (contents :initform nil
             :documentation "A pre-allocated vector with the actual contents")
   (reader-pos :initform 0
               :documentation "Current position in 'contents' to be used then performing 'pop' operation")
   (writer-pos :initform 0
               :documentation "Current position in 'contents' to be used then performing 'push' operation"))
  (:documentation "Fixed size queue."))

(defmethod initialize-instance :after ((self queue) &key &allow-other-keys)
  (with-slots (size contents reader-pos writer-pos element-type) self
    (setf contents (make-array size :adjustable nil :element-type element-type)
          reader-pos 0
          writer-pos 0
          size 0)))


(defun make-queue (size &optional (element-type t))
  "Create a fixed size queue of the provided size and optionally of the provided type of its elements"
  (make-instance 'queue :size size :element-type element-type))

(defun queue-capacity (q)
  "Returns the actual capacity of the queue, how many element maximum could be in the queue"
  (length (slot-value q 'contents)))

(defun queue-emptyp (q)
  "Check if the queue is empty"
  (= (queue-size q) 0))

(defun queue-fullp (q)
  "Check if the queue is full"
  (= (queue-capacity q)
     (queue-size q)))

(defun queue-clear (q)
  "Reset the queue contents. After this operation the queue is empty"
  (with-slots (reader-pos writer-pos size) q
    (setf reader-pos 0
          writer-pos 0
          size 0)))

(defun queue-advance-pos (q pos &optional (count 1))
  "Increase by COUNT and wrap up the POS around the queue size"
  (mod (+ pos count) (queue-capacity q)))

(defun queue-push (q el)
  "Push the element EL to the queue Q. Return nil if the queue is full"
  (unless (queue-fullp q)
    (with-slots (writer-pos contents size) q
      (setf (svref contents writer-pos) el
            writer-pos (queue-advance-pos q writer-pos)
            size (1+ size))
      t)))
    
(defun queue-pop (q)
  "Pop and return the element from the queue Q. Return nil if the queue is empty"
  (with-slots (reader-pos contents size) q
    (unless (queue-emptyp q)
      (let ((res (svref contents reader-pos)))
        (setf reader-pos (queue-advance-pos q reader-pos)
              size (1- size))
        res))))



(defgeneric plane-equation (v1 v2 v3)
  (:documentation
   "Calculate the plane equation in format
Ax+By+Cz+D=0 and returns values A B C D
v1,v2,v3 are vertices.

The equation is calculated via
https://www.wolframalpha.com/input/?i=Collect%5Bdet%5B%7Bx-x1,+x2-x1,+x3-x1%7D,%7By-y1,+y2-y1,+y3-y1%7D,%7Bz-z1,+z2-z1,+z3-z1%7D%5D,+%7Bx,y,z%7D%5D
  
x (y1 z2 - y1 z3 - y2 z1 + y2 z3 + y3 z1 - y3 z2) + y (-x1 z2 + x1 z3 + x2 z1 - x2 z3 - x3 z1 + x3 z2) + z (x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2) - x1 y2 z3 + x1 y3 z2 + x2 y1 z3 - x2 y3 z1 - x3 y1 z2 + x3 y2 z1

Hence
A = (y1 z2 - y1 z3 - y2 z1 + y2 z3 + y3 z1 - y3 z2)
B = (-x1 z2 + x1 z3 + x2 z1 - x2 z3 - x3 z1 + x3 z2)
C = (x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2)
D = - x1 y2 z3 + x1 y3 z2 + x2 y1 z3 - x2 y3 z1 - x3 y1 z2 + x3 y2 z1"))

(defun plane-equation-impl (x1 y1 z1 x2 y2 z2 x3 y3 z3)
  "Actual calculation of the plane equation"
  (let ((A (- (+ (* y1 z2) (* y2 z3) (* y3 z1))
              (+ (* y1 z3) (* y2 z1) (* y3 z2))))
        (B (- (+ (* x1 z3) (* x2 z1) (* x3 z2))
              (+ (* x1 z2) (* x2 z3) (* x3 z1))))
        (C (- (+ (* x1 y2) (* x2 y3) (* x3 y1))
              (+ (* x1 y3) (* x2 y1) (* x3 y2))))
        (D (- (+ (* x1 y3 z2) (* x2 y1 z3) (* x3 y2 z1))
              (+ (* x1 y2 z3) (* x2 y3 z1) (* x3 y1 z2)))))
    (list A B C D)))


(defmethod plane-equation ((v1 list) (v2 list) (v3 list))
  "Calculate plane equation parameters by given 3 lists of coordinates"
  (let* ((x1 (first v1))
         (x2 (first v2))
         (x3 (first v3))
         (y1 (second v1))
         (y2 (second v2))
         (y3 (second v3))
         (z1 (third v1))
         (z2 (third v2))
         (z3 (third v3)))
    (plane-equation-impl x1 y1 z1 x2 y2 z2 x3 y3 z3)))


(defun plane-normal-oriented (v1 v2 v3)
  (if (not (plane-positive-oriented-p v1 v2 v3))
      ;; if not, swap the vertices
      (list v2 v1 v3))
  (list v1 v2 v3))

(defun vec-minus (v1 v2)
  "Subtract V2 from V1. V1 and V2 are lists of 3 coordinates."
  (list (- (first v1) (first v2))
        (- (second v1) (second v2))
        (- (third v1) (third v2))))

(defun vec-dot-product (u v)
  "Dot product of vectors u and v. U and V are lists of 3 coordinates.
See http://mathworld.wolfram.com/DotProduct.html"
  (+ (* (first u) (first v))
     (* (second u) (second v))
     (* (third u) (third v))))

(defun vec-cross-product (u v)
  "Cross product of vectors u and v. U and V are lists of 3 coordinates.
See http://mathworld.wolfram.com/CrossProduct.html"
  (let ((ux (first u)) (uy (second u)) (uz (third u))
        (vx (first v)) (vy (second v)) (vz (third v)))
    (list
     (- (* uy vz) (* uz vy))
     (- (* uz vx) (* ux vz))
     (- (* ux vy) (* uy vx)))))
        

(defun plane-positive-oriented-p (v1 v2 v3)
  ;; calculate the normal to the plane constructed from the
  ;; first 3 points
  (let* ((normal (plane-equation v1 v2 v3))
         ;; calculate the cos of the angle between 2 vectors
         ;; on a plane and normal, to determine the orientation
         ;; by the sign of the angle
         (angle (vec-dot-product
                 (vec-cross-product (vec-minus v3 v1)
                                    (vec-minus v2 v1))
                 normal)))
    (format t "Normal: ~a Angle: ~f~%" normal angle)
    (> angle 0)))
