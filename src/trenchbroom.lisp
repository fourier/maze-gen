(in-package :maze-gen)

;; some constants


(defparameter *hallway-width* (* 16 10)
  "Hallway width")
;; cell size is hallway-width x hallway-width then

(defparameter *wall-width* (* 16 2)
  "Wall width")

(defparameter *wall-height* (* 16 8)
  "Wall height")

(defparameter *wall-texture* "wall9_8"
  "Wall texture from START.WAD")

(defparameter *floor-texture* "sfloor4_2"
  "Floor texture from START.WAD")

(defparameter *ceil-texture* "cop1_6"
  "Ceil texture from START.WAD")

 
(defun wall-to-corners (x-top y-top x-bottom y-bottom)
  "Convert the wall in cell-indices format to the
actual coordinates on a plane.
Return the list of conses - coordinates - of
all 4 points, corners of the wall on a plane"
  (let ((result nil)
        (vertical (= x-top x-bottom))
        (xa (+ (* x-top (+ *wall-width* *hallway-width*)) *wall-width*))
        (ya (* y-top (+ *wall-width* *hallway-width*)))
        (xb (+ (* x-bottom (+ *wall-width* *hallway-width*)) *wall-width*))
        (yb (* y-bottom (+ *wall-width* *hallway-width*))))
    (when vertical
        (setf xa (- xa *wall-width*)
              xb (- xb *wall-width*)))
    (push (cons xb yb) result)
    (push (cons xa ya) result)
    ;; now 2 points calculated, need to calculate another 2
    ;; by adding wall width to either x or y coordinates
    (if (= x-top x-bottom)
        ;; vertical wall
        (setf result (append result (list (cons (+ xb *wall-width*) yb)
                                          (cons (+ xa *wall-width*) ya))))
        ;; horizontal wall
        (setf result (append result (list (cons xb (+ yb *wall-width*))
                                          (cons xa (+ ya *wall-width*))))))

    (nreverse result)))
            

(defun 2d-corners-to-3d-corners (corners)
  "Convert the list of 4 2d-points to the list of 8 3d-points"
  (append (mapcar
           (lambda (c)
             (list (car c) (cdr c) 0))
           corners)
          (mapcar
           (lambda (c)
             (list (car c) (cdr c) *wall-height*))
           corners)))

(defun 3d-corners-to-planes (3d-corners)
  "Convert the list of 8 3d-corders of the brush to the
list of 6 planes - each formed by 3 3d-corders"
  (destructuring-bind (p1 p2 p3 p4 pp1 pp2 pp3 pp4) 3d-corners
    (list (list p1 p2 p3)
          (list pp1 pp2 pp3)
          (list p1 p2 pp2)
          (list p1 p4 pp4)
          (list p2 p3 pp3)
          (list p3 p4 pp4))))
      
                                                     
(defmethod export-trenchbroom ((self grid) filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "// Game: Quake
// Format: Standard
// entity 0
{
\"classname\" \"worldspawn\"
\"wad\" \"C:/q1mapping/wads/START.WAD\"~%")
    (export-brushes self out)
    (format out "}~%")))


(defun export-brushes (grid out)
  (let* ((nrows (grid-nrows grid))
         (ncols (grid-ncols grid))
         (full-cell-width (+ *hallway-width* *wall-width*))
         (center-x (/ (* ncols full-cell-width) 2))
         (center-y (/ (* nrows full-cell-width) 2))
         (cell-w (/ area-w ncols))
         (cell-h (/ area-h nrows))
         (offset-x (- center-x))
         (offset-y (- center-y))
         (walls (grid-create-walls grid)))
    (mapcar (lambda (w)
              (export-brush 
               (3d-corners-to-planes (2d-corners-to-3d-corners (apply #'wall-to-corners w))) out))
            walls)))

(defun export-brush (planes out)
  (format out "{~%")
  (dolist (p planes)
    (dolist (n p)
      (format out "(~{~d~^ ~}) " n))
    (format out "~a -0 -0 -0 1 1~%" *wall-texture*))
  (format out "}~%"))
