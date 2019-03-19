(in-package :maze-gen)

(defparameter *max-rows* 39)
(defparameter *max-cols* 39)
(defparameter *max-rooms* 10)
(defparameter *max-rooms-attempts* 200)

(defparameter +nothing+      #x00000000)
(defparameter +blocked+      #x00000001)
(defparameter +room+         #x00000002)
(defparameter +corridor+     #x00000004)

(defparameter +min-room-size+ 3)
(defparameter +max-room-size+ 9)


(defclass dungeon ()
  ((rows :initarg :rows :initform *max-rows* :type fixnum)
   (cols :initarg :cols :initform *max-cols* :type fixnum)
   (nrooms :initarg :nrooms :initform *max-rooms* :type fixnum)
   (grid)
   (rooms :initform nil)))

(defmethod initialize-instance :after ((self dungeon) &key &allow-other-keys)
  "Constructor for the pack-file class"
  (with-slots (rows cols grid) self
    (setf grid (make-array (list rows cols) :element-type 'fixnum :initial-element +nothing+))))


(defmethod add-rooms ((self dungeon))
  (with-slots (rows cols grid nrooms rooms) self
    (loop with created-rooms = 0 
          for attempt below *max-rooms-attempts*
          for room = (create-room rows cols)
          while (< created-rooms nrooms)
          when (room-sound self room)
          do
          (push room rooms)
          (emplace-room self room)
          (incf created-rooms))))

(defun create-room (rows cols)
  (let* ((w (+ 2 +min-room-size+ (random (- +max-room-size+ +min-room-size+))))
         (h (+ 2 +min-room-size+ (random (- +max-room-size+ +min-room-size+))))
         (c (random (- cols w)))
         (r (random (- rows h))))
    (list r c w h)))

(defun rooms-intersects-p (room1 room2)
  (destructuring-bind (y1 x1 w1 h1) room1
    (destructuring-bind (y2 x2 w2 h2) room2
      (not
       (or (< (+ x1 w1) x2)
           (< (+ x2 w2) x1)
           (< (+ y1 h1) y2)
           (< (+ y2 h2) y1))))))

(defmethod room-sound ((self dungeon) room)
  "Check if any room intersects with this room"
  (with-slots (rooms) self
    (notany (lambda (r) (rooms-intersects-p room r)) rooms)))


(defmethod emplace-room ((self dungeon) room)
  "Place room on a grid along with the borders"
  (with-slots (grid) self
    (destructuring-bind (r c w h) room
      (loop for i below w
            do
            (loop for j below h
                  for x =
                  ;; determine borders
                  (if (or (= i 0) (= j 0) (= i (1- w)) (= j (1- h))) 
                      +blocked+
                      ;; or room contents
                      +room+)
                  do 
                  (setf (aref grid (+ r j) (+ c i)) x))))))


(defmethod print-object ((self dungeon) stream)
  (with-slots (rows cols grid) self
    (loop for i below rows
          do (loop for j below cols
                   do (format stream "~d" (aref grid i j))
                   finally (format stream "~%")))))


(defmethod draw ((self dungeon))
  (with-slots (rows cols grid) self
    (loop for i below rows
          do
          (when (= i 0)
            (format t "+")
            (loop for k below cols do (format t "-"))
            (format t "+~%"))
          (format t "|")
          (loop for j below cols
                for c = (aref grid i j)
                for d = (cond ((= c +nothing+) " ")
                              ((/= 0 (logand c +room+)) ".")
                              ((/= 0 (logand c +corridor+)) ".")
                              ((/= 0 (logand c +blocked+)) "#")
                              (t " "))
                do
                (format t "~a" d)
                finally 
                (format t "|~%"))
          (when (= i (1- rows))
            (format t "+")
            (loop for k below cols do (format t "-"))
            (format t "+~%"))))
  (values))
