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

                 
                                                     


#|
(defmethod export-trenchbroom ((self grid) filename)
  (let* ((nrows (grid-nrows grid))
         (ncols (grid-ncols grid))
         (full-cell-width (+ *hallway-width* *wall-width*))
         (center-x (/ (* ncols full-cell-width) 2))
         (center-y (/ (* nrows full-cell-width) 2)))
    (loop for r below nrows
          do
          (loop for c below ncols
                do

          
    (flet ((draw-cell (c)
             (let ((x1 (+ area-x (* (cell-col c) cell-w)))
                   (y1 (+ area-y (* (cell-row c) cell-h)))
                   (x2 (+ area-x (* (1+ (cell-col c)) cell-w)))
                   (y2 (+ area-y (* (1+ (cell-row c)) cell-h)))
                   (n (cell-get-neighbour c 'north))
                   (s (cell-get-neighbour c 'south))
                   (e (cell-get-neighbour c 'east))
                   (w (cell-get-neighbour c 'west)))
               ;; upper boundary - if no north cell
               (unless n (gp:draw-line pane x1 y1 x2 y1))
               ;; westen boundary - if no west cell
               (unless w (gp:draw-line pane x1 y1 x1 y2))
               ;; eastern boundary - if no eastern cell linked
               (unless (cell-linked-p c e) (gp:draw-line pane x2 y1 x2 y2))
               ;; southern boundary - if no southern cell linked
               (unless (cell-linked-p c s) (gp:draw-line pane x1 y2 x2 y2)))))
      ;; draw border
      (gp:draw-rectangle pane area-x area-y area-w area-h :filled t :foreground :grey85)
      (grid-map grid #'draw-cell))))

|#