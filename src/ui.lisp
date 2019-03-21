(in-package :maze-gen)

(defparameter *version* 0.1)

;; shortcut/convenience macro
(defmacro with-pane-process ((self) &body body)
  `(capi:apply-in-pane-process ,self
                              (lambda ()
                                ,@body)))


(define-interface maze-gen-ui ()
  (grid)
  (:menus
   (application-menu
    "File"
    ((:component
      (("About"
        :callback
        (lambda ()
          (display-message-on-screen
           (convert-to-screen nil)
           "Procedural Maze Generator ~a~%Copyright (c) Alexey Veretennikov(fourier) 2019" *version*))
        :callback-type :none))))))
  (:panes
   (draw-board output-pane
               :min-width 600
               :min-height 600
               :draw-with-buffer t
               :resize-callback 'on-resize-draw-board
               :display-callback 'on-redisplay-draw-board)
   (algorithm-panel radio-button-panel
                   :visible-max-width nil
                   :visible-max-height nil
                   :items '((binary-tree . "Binary tree") (sidewinder . "Sidewinder"))
                   :layout-class 'column-layout
                   :layout-args '(:adjust :right ;;:internal-border 20
                                  :uniform-size-p t)
                   :print-function #'cdr)
   (size-slider slider :start 10 :end 100 :tick-frequency 10)
   (convert-button push-button :text "Regenerate" :callback 'on-generate-button))
  (:layouts
   (algorithm-options-layout column-layout
                             '(algorithm-panel)
                             :title "Algorithms"
                             :title-position :frame)
   (geometry-options-layout column-layout
                            '(size-slider)
                            :title "Size of the maze"
                            :title-position :frame)
   (options-layout row-layout '(geometry-options-layout algorithm-options-layout))
   (main-layout column-layout '(draw-board
                                options-layout
                                convert-button)
                :adjust :center
                :y-ratios '(1 nil nil)
                :internal-border 20))
  (:menu-bar application-menu)
  (:default-initargs :title (format nil "Procedural Maze Generator ~a" *version*)
   :layout 'main-layout))


(defmethod initialize-instance :after ((self maze-gen-ui) &key &allow-other-keys)
  (on-generate-button nil self))


(defun on-generate-button (data self)
  "Callback called then the user press Regenerate button"
  (declare (ignore data))           
  (with-slots (draw-board grid size-slider algorithm-panel) self
    (let* ((size (range-slug-start size-slider))
           (algo (car (choice-selected-item algorithm-panel)))
           (algo-fun (cond ((eql algo 'binary-tree) #'binary-tree)
                           ((eql algo 'sidewinder) #'sidewinder)
                           (t #'identity))))
      (setf grid (funcall algo-fun (make-instance 'grid :rows size :cols size)))
      ;; force redisplay
      (gp:invalidate-rectangle draw-board))))


(defun on-redisplay-draw-board (pane x y width height)
  (with-slots (grid) (capi:element-interface pane)
    ;; calculate draw area
    (let* ((area-x (+ 5 x))
           (area-y (+ 5 y))
           (area-w (- width 10 ))
           (area-h (- height 10))
           ;; shortcuts for sizes
           (nrows (grid-nrows grid))
           (ncols (grid-ncols grid))
           ;; calculate labyrinth "cell" width/height
           (cell-w (/ area-w ncols))
           (cell-h (/ area-h nrows)))
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
        (grid-map grid #'draw-cell)))))


(defun on-resize-draw-board (pane x y width height)
  (on-redisplay-draw-board pane x y width height))


(defun main-ui ()
  (capi:display (make-instance 'maze-gen-ui)))

