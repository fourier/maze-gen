(in-package :maze-gen)

(defparameter *version* 0.1)

;; shortcut/convenience macro
(defmacro with-pane-process ((self) &body body)
  `(capi:apply-in-pane-process ,self
                              (lambda ()
                                ,@body)))

(define-interface maze-trenchbroom-window ()
  ((grid :initarg :grid))
  (:panes
   (tb-preview output-pane
               :min-width 600
               :min-height 600
               :draw-with-buffer t
               :resize-callback (lambda (pane x y width height)
                                  (on-redisplay-tb-preview pane x y width height))
               :display-callback 'on-redisplay-tb-preview)
   (export-button push-button
                  :text "Export to TrenchBroom..."
                  :data grid
                  :callback-type :data
                  :callback 'on-export-tb-button))
  (:layouts
   (main-layout column-layout
                '(tb-preview export-button)
                :gap 10 ;; distance between elements
                :internal-border 20 ;; distance to border of the frame
                :adjust :center))
  (:default-initargs :title (format nil "Trenchbroom export preview") :layout 'main-layout))
  

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
        :callback-type :none)
       ("Export..."
        :callback
        #'preview-trechbroom
        :callback-type :interface))))))
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
           (cell-h (/ area-h nrows))
           (walls (grid-make-walls grid)))
      ;; draw border
      (gp:draw-rectangle pane area-x area-y area-w area-h :filled t :foreground :grey85)      
      (dolist (w walls)
        (destructuring-bind (x1 y1 x2 y2) w
          (gp:draw-line pane
                        (+ area-x (* x1 cell-w))
                        (+ area-y (* y1 cell-h))
                        (+ area-x (* x2 cell-w))
                        (+ area-y (* y2 cell-h))))))))




(defun on-resize-draw-board (pane x y width height)
  (on-redisplay-draw-board pane x y width height))

(defun preview-trechbroom (iface)
  (capi:display (make-instance 'maze-trenchbroom-window :grid (slot-value iface 'grid))))


(defun on-redisplay-tb-preview (pane x y width height)
  (with-slots (grid) (capi:element-interface pane)
    (let ((blocks
           (grid-make-blocks grid)))
      (dolist (b blocks)
        (destructuring-bind ((x1 . y1) (x2 . y2) (x3 . y3) (x4 . y4))
            (mapcar (lambda (p) (cons (+ x (/ (car p) 4)) (+ y (/ (cdr p) 4))))
                    b)
          (gp:draw-line pane x1 y1 x2 y2)
          (gp:draw-line pane x2 y2 x3 y3)
          (gp:draw-line pane x3 y3 x4 y4)
          (gp:draw-line pane x4 y4 x1 y1))))))


(defun on-export-tb-button (data)
  (multiple-value-bind (filename successp filter-name)
      (prompt-for-file "Enter a filename:"
                       :if-exists :prompt :if-does-not-exist :ok
                       :operation :save
                       :filter "*.map")
    (declare (ignore filter-name))
    (when successp
      (export-trenchbroom data filename))))


(defun main-ui ()
  (capi:display (make-instance 'maze-gen-ui)))

