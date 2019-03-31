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

