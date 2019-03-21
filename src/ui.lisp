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
      (setf grid (funcall algo-fun (make-instance 'grid :rows size :cols size))))))
;;



(defun on-redisplay-draw-board (pane x y width height)
  (gp:draw-circle pane 100 100 50))


(defun main-ui ()
  (capi:display (make-instance 'maze-gen-ui)))

