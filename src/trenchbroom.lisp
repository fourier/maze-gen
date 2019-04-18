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

 
(defun 2d-corners-to-3d-corners (corners &key (offset-z 0) (height *wall-height*))
  "Convert the list of 4 2d-points to the list of 8 3d-points"
  (append (mapcar
           (lambda (c)
             (list (car c) (cdr c) offset-z))
           corners)
          (mapcar
           (lambda (c)
             (list (car c) (cdr c) (+ offset-z height)))
           corners)))

(defun 3d-corners-to-planes (3d-corners)
  "Convert the list of 8 3d-corders of the brush to the
list of 6 planes - each formed by 3 3d-corders.
The 3d corners are supposed to have the following orientation
and numbering:
First 4 is the corners of the lower plane of the brick
Next 4 are corners just above the first 4. The numbering is
like this:

  4---------3
  |         |
  |         |
  |         |
  1---------2

The order of points in plane, when looking from outside the face:

   1
   |
   |
   |
   |
   0-----------2

"
  (destructuring-bind (p1 p2 p3 p4 pp1 pp2 pp3 pp4) 3d-corners
    (declare (ignore pp2))
    (let ((result
           (list
            (list p4 p3 p1)
            (list pp4 pp1 pp3)
            (list p4 p1 pp4)
            (list p1 p2 pp1)
            (list p3 pp3 p2)
            (list p4 pp4 p3))))
      result)))
                                                     
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
         (offset-x (- center-x))
         (offset-y (- center-y))
         (walls (grid-make-blocks grid))
         (floors (grid-make-floors grid)))
    (mapcar (lambda (w)
              (export-brush 
               (3d-corners-to-planes (2d-corners-to-3d-corners (move-wall w offset-x offset-y))) out))
            walls)
    (mapcar (lambda (w)
              (export-brush
               (3d-corners-to-planes (2d-corners-to-3d-corners (move-wall w offset-x offset-y)
                                                               :offset-z 16 :height 16)) out
               :texture *floor-texture*))
            floors)))

        

(defun export-brush (planes out &key (texture *wall-texture*))
  (format out "{~%")
  (dolist (p planes)
    (dolist (n p)
      (format out "(~{~d~^ ~}) " n))
    (format out "~a 0 0 0 1 1~%" texture))
  (format out "}~%"))


(defun create-horizontal-wall (&key (extend-west nil) (extend-east nil))
  ;; the wall is created like this:
  ;;
  ;;      *hallway-width*
  ;;
  ;; (x4 y4)-----------(x3 y3)  
  ;;    |                 |
  ;;    |                 |   *wall-width*
  ;;    |                 |
  ;; (x1 y1)-----------(x2 y2)
  ;;
  (let* ((x1 0)
         (y1 *wall-width*)
         (x2 *hallway-width*)
         (y2 y1)
         (x3 x2)
         (y3 0)
         (x4 0)
         (y4 0))
    (when extend-west 
      (decf x1 *wall-width*)
      (decf x4 *wall-width*))
    (when extend-east 
      (incf x2 *wall-width*)
      (incf x3 *wall-width*))
    (list (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4))))
  

(defun create-vertical-wall (&optional (cut-on-top-p nil))
  ;; the wall is created like this:
  ;;
  ;;      *wall-width*
  ;;
  ;; (x4 y4)----(x3 y3)  
  ;;    |          |
  ;;    |          |   *hallway-width*
  ;;    |          |
  ;;    |          |
  ;;    |          |
  ;;    |          |
  ;; (x1 y1)----(x2 y2)
  ;;
  (let* ((x1 0)
         (y1 (+ *wall-width* *hallway-width*))
         (x2 *wall-width*)
         (y2 y1)
         (x3 x2)
         (y3 0)
         (x4 0)
         (y4 0))
    (when cut-on-top-p
      (incf y3 *wall-width*)
      (incf y4 *wall-width*))
    (list (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4))))


(defun move-wall (wall offset-x offset-y)
  "Move the wall to specified offset-x and offset-y.
Return the new wall"
  (mapcar (lambda (p) (cons (+ (car p) offset-x) (+ (cdr p) offset-y)))
          wall))

(defun extend-wall (wall direction length)
  "Extend the wall - updating coordinates - in specified direction.
Return the new wall"
  (destructuring-bind (p1 p2 p3 p4) wall
    (eswitch (direction)
      ('north (list p1 p2
                    (cons (car p3) (- (cdr p3) length))
                    (cons (car p4) (- (cdr p4) length))))
      ('south (list (cons (car p1) (+ (cdr p1) length))
                    (cons (car p2) (+ (cdr p2) length))
                    p3 p4))
      ('west (list (cons (- (car p1) length) (cdr p1)) p2
                   p3
                   (cons (- (car p4) length) (cdr p4))))
      ('east (list p1 (cons (+ (car p2) length) (cdr p2))
                   (cons (+ (car p3) length) (cdr p3))
                   p4)))))

(defun grid-make-blocks (grid)
  "Create a list of blocks - foundations of the walls for the grid.
Every block is a list of pairs (x, y) - the corners of the block."
  (let* ((nrows (grid-nrows grid))
         (ncols (grid-ncols grid))
         ;; first we create the northen big wall and
         ;; western big wall and push them to result
         (result (list (extend-wall (create-horizontal-wall) 'east
                                    (+ (* (1- ncols) (+ *wall-width* *hallway-width*))
                                       (* 2 *wall-width*))) ; 2 bounding walls
                       (extend-wall (move-wall (create-vertical-wall) 0 *wall-width*) 'south
                                    (* (1- nrows) (+ *wall-width* *hallway-width*))))))
    (dotimes (r nrows)
      (dotimes (c ncols)
        (let* ((cell (grid-cell grid r c))
               (s (cell-get-neighbour cell 'south))
               (e (cell-get-neighbour cell 'east))
               (w (cell-get-neighbour cell 'west))
               (offset-x (+ *wall-width* (* c (+ *hallway-width* *wall-width*))))
               (offset-y (+ *wall-width* (* r (+ *hallway-width* *wall-width*))))
               (has-eastern-wall (not (cell-linked-p cell e)))
               (has-southern-wall (not (cell-linked-p cell s)))
               (has-western-wall (not (cell-linked-p cell w)))
               (last-horizontal-wall-extended-to-east
                (and w (not (cell-linked-p w (cell-get-neighbour w 'south))))))
          ;; different kind of blocks
          ;; we never create northern or western boundaries
          ;; so only possible options are:
          ;; +   +   +   +
          ;;   a |     b 
          ;; +---+   +---+
          ;; 
          ;; +   +   +   +
          ;;   c |     d  
          ;; +   +   +   +
          (cond ((and has-southern-wall has-eastern-wall) ;; case a
                 (push (move-wall (create-horizontal-wall :extend-west
                                                          (and (not has-western-wall)
                                                               (not last-horizontal-wall-extended-to-east))
                                                          :extend-east (not has-eastern-wall))
                                  offset-x (+ *hallway-width* offset-y)) result)
                 (push (move-wall (create-vertical-wall)
                                  (+ *hallway-width* offset-x) offset-y) result))

                ((and has-southern-wall (not has-eastern-wall)) ;; case b
                 (push (move-wall (create-horizontal-wall :extend-west
                                                          (and (not has-western-wall)
                                                               (not last-horizontal-wall-extended-to-east))
                                                          :extend-east (not has-eastern-wall))
                                  offset-x (+ *hallway-width* offset-y)) result))
                ((and has-eastern-wall (not has-southern-wall)) ;; case c
                 (push (move-wall (create-vertical-wall)
                                  (+ *hallway-width* offset-x) offset-y) result))))))
          (nreverse result)))


(defun grid-make-floors (grid)
  "Create a list of blocks - floors of the cells.
Every block is a list of pairs (x, y) - the corners of the block."
  (let* ((nrows (grid-nrows grid))
         (ncols (grid-ncols grid))
         (width (+ *hallway-width* *wall-width*))
         (height (+ *hallway-width* *wall-width*))
         (total-width (+ (* nrows width) *wall-width*))
         (total-height (+ (* ncols height) *wall-width*))
         (result nil))
    ;; just one big block for now
    (push (list (cons 0 total-height)
                (cons total-width total-height)
                (cons total-width 0)
                (cons 0 0))
          result)
    result))

;;;;;; Test functions

(defun export-brick-brush (out)
  (export-brush 
   (3d-corners-to-planes
    (create-brick)) out))

;; test function: creates a brick of proper node orientation
(defun create-brick ()
  '((0 32 0) (32 32 0) (32 0 0) (0 0 0)
    (0 32 16) (32 32 16) (32 0 16) (0 0 16)))
