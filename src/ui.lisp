(in-package :maze-gen)

(defparameter *version* 0.2)

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
                   :items `((binary-tree . "Binary tree")
                            (sidewinder . "Sidewinder")
                            (aldous-broder . "Aldous-Broder"))
                   :layout-class 'column-layout
                   :layout-args '(:adjust :right ;;:internal-border 20
                                  :uniform-size-p t)
                   :print-function #'cdr)
   (draw-options-panel check-button-panel
                       :visible-max-width nil
                       :visible-max-height nil
                       :items '((:draw-start-end . "Start/End cells")
                                (:distance-intensity . "Distance intensity")
                                (:shortest-path . "Shortest path"))
                       :print-function #'cdr
                       :layout-class 'column-layout
                       :layout-args '(:adjust :right ;;:internal-border 20
                                      :uniform-size-p t)
                       :selection-callback 'on-draw-options-checkbox-selected
                       :retract-callback 'on-draw-options-checkbox-retracted)
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
   (draw-options-layout column-layout
                        '(draw-options-panel)
                        :title "Drawing options"
                        :title-position :frame)
   (options-layout row-layout '(geometry-options-layout
                                algorithm-options-layout
                                draw-options-layout))
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
           (algo-fun (car (choice-selected-item algorithm-panel))))
      (setf grid (funcall algo-fun (make-instance 'grid :rows size :cols size)))
      ;; force redisplay
      (gp:invalidate-rectangle draw-board))))


(defmethod setting-selected ((self maze-gen-ui) option)
  "Check if the settings checkbox selected."
  (when-let (selected (mapcar #'car (choice-selected-items (slot-value self 'draw-options-panel))))
    (member option selected)))


(defun on-redisplay-draw-board (pane x y width height)
  (let ((interface (capi:element-interface pane)))
    (with-slots (grid) interface
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
             (walls (grid-make-walls grid))
             (pixmap (gp:create-pixmap-port pane width height :background :grey :clear t)))
        ;; draw border
        (gp:draw-rectangle pixmap area-x area-y area-w area-h :filled t :foreground :grey85)
        ;; optionally draw some other attributes
        (draw-options pixmap grid area-x area-y cell-w cell-h
                      :draw-distances (setting-selected interface :distance-intensity)
                      :draw-start-end (setting-selected interface :draw-start-end)
                      :draw-shortest-path (setting-selected interface :shortest-path))
        (dolist (w walls)
          (destructuring-bind (x1 y1 x2 y2) w
            (gp:draw-line pixmap
                          (+ area-x (* x1 cell-w))
                          (+ area-y (* y1 cell-h))
                          (+ area-x (* x2 cell-w))
                          (+ area-y (* y2 cell-h))
                          :thickness 5
                          :foreground :purple)))
        ;; show the pixmap. 
        (gp:copy-pixels pane pixmap 0 0 width height 0 0)))))

(defun draw-options (pixmap grid area-x area-y cell-w cell-h
                            &key (draw-distances nil)
                            (draw-start-end nil)
                            (draw-shortest-path nil))
  (multiple-value-bind (start-end distances) (dijkstra-longest-path grid)
    (let ((max-distance (gethash (cdr start-end) distances))
          (radius (max 2 (- (/ (min cell-w cell-h) 2.0) 4)))
          (shortest-path (dijkstra-shortest-path grid
                                                 (car start-end)
                                                 (cdr start-end)
                                                 distances)))
      ;; first draw the filled cells with color = intencity, distance
      ;; between 2 star/end points
      (when draw-distances
        (grid-map grid
                  (lambda (c)
                    (let* ((col (cell-col c))
                           (row (cell-row c))
                           (d (gethash c distances))
                           (color (color:make-rgb 0 (/ (- max-distance d) max-distance) 0)))
                      (gp:draw-rectangle pixmap (+ area-x (* col cell-w)) (+ area-y (* row cell-h))
                                         (1+ cell-w) (1+ cell-h)
                                         :foreground color
                                         :filled t)))))
      ;; then draw the start/end points
      (when draw-start-end
        (gp:draw-circle pixmap
                        (+ area-x (* (cell-col (car start-end)) cell-w)
                           (/ cell-w 2.0))
                        (+ area-y (* (cell-row (car start-end)) cell-h)
                           (/ cell-h 2.0))
                        radius
                        :foreground :red
                        :filled t)
        (gp:draw-circle pixmap
                        (+ area-x (* (cell-col (cdr start-end)) cell-w)
                           (/ cell-w 2.0))
                        (+ area-y (* (cell-row (cdr start-end)) cell-h)
                           (/ cell-h 2.0))
                        radius
                        :foreground :red
                        :filled nil
                        :thickness (max 4 (/ radius 2))))
      (when draw-shortest-path
        (let* ((start-x (+ area-x (* (cell-col (car start-end)) cell-w) (/ cell-w 2.0)))
               (start-y (+ area-y (* (cell-row (car start-end)) cell-w) (/ cell-h 2.0)))
               (gp-path
                (mapcar (lambda (c)
                          (let ((cx (- (+ area-x (* (cell-col c) cell-w)
                                          (/ cell-w 2.0))
                                       start-x))
                                (cy (- (+ area-y (* (cell-row c) cell-h)
                                          (/ cell-h 2.0))
                                       start-y)))
                            (list :line cx cy)))
                        (cdr shortest-path))))
          (gp:draw-path pixmap gp-path start-x start-y :thickness 2 :foreground :yellow))))))


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


(defmethod on-draw-options-checkbox-selected (data (self maze-gen-ui))
  "Callback called when selected one of settings checkboxes"
  (with-slots (draw-board) self
    (gp:invalidate-rectangle draw-board)))

(defmethod on-draw-options-checkbox-retracted (data (self maze-gen-ui))
  "Callback called when retracted selection of settings checkboxes"
  (with-slots (draw-board) self
    (gp:invalidate-rectangle draw-board)))

  
(defun main-ui ()
  (capi:display (make-instance 'maze-gen-ui)))

