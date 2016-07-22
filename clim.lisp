(ql:quickload "mcclim")
(ql:quickload "cl-fad")
(defpackage #:clim-demo
  (:use #:clim #:clim-lisp #:cl-fad))

(in-package #:clim-demo)

;; hello world

(define-application-frame
  hello-world ()
  ((greeting :initform "Hello World"
             :accessor greeting))
  (:pane (make-pane 'hello-world-pane)))

(defclass hello-world-pane (clim-stream-pane) ())

(defmethod handle-repaint ((pane hello-world-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    (draw-rectagne* pane 0 0 w h
                    :filled t
                    ink (pane-background pane))
    (draw-text* pane
                (greeting *application-frame*)
                (floor w 2) (floor h 2)
                :align-x :center
                :align-y :center)))

(run-frame-top-level (make-application-frame 'hello-world))

;; color-editor

(defun make-color-slider (id initval label)
  (labelling (:label label)
    (make-pane ':slider :id id :orientation :horizontal :value initval
               :max-value 1 :min-value 0
               :drag-callback #'color-slider-dragged
               :value-changed-callback #'color-slider-value-changed)))

(define-application-frame color-editor ()
  (current-color-pane
    drag-feedback-pane
    (red :initform 1.0)
    (green :initform 0.0)
    (blue :initform 0.0))
  (:pane (with-slots (drag-feedback-pane current-color-pane red green blue)
           *application-frame*
           (vertically ()
             (setf current-color-pane
                   (make-pane 'application-pane :min-height 100 :max-height 100
                              :background (make-rgb-color red green blue)))
           (horizontally (:min-height 200 :max-height 200)
             (1/3 (make-color-slider 'red red "Red"))
             (1/3 (make-color-slider 'green green "Green"))
             (1/3 (make-color-slider 'blue blue "Blue")))
           +fill+
           (setf drag-feedback-pane
                 (make-pane 'application-pane :min-height 100 :max-height 100
                            :background (make-rgb-color red green blue))))))
  (:menu-bar t))

(defun color-slider-dragged (slider value)
  (with-slots (drag-feedback-pane red green blue) *application-frame*
    (setf (medium-background drag-feedback-pane)
          (ecase (gadget-id slider)
            (red (make-rgb-color value green blue))
            (green (make-rgb-color red value blue))
            (blue (make-rgb-color red green value))))
    (redisplay-frame-pane *application-frame* drag-feedback-pane)))

(defun color-slider-value-changed (slider new-value)
  (with-slots (current-color-pane red green blue) *application-frame*
    ;; gadget-id symbols match slot names in color editor
    (setf (slot-value *application-frame* (gadget-id slider)) new-value)
    (setf (medium-background current-color-pane)
          (make-rgb-color red green blue))
    (redisplay-frame-pane *application-frame* current-color-pane)))

(define-color-editor-command (com-quit :name "Quit" :menu t) ()
                             (frame-exit *application-frame*))

(run-frame-top-level (make-application-frame 'color-editor))

;; drawing

(define-application-frame
  draw-frame ()
  ((lines :accessor lines :initform nil) ;; lines in drawing
   (strings :accessor strings :initform nil)) ;; texts in drawing
  (:panes (draw-pane (make-pane 'draw-pane))
   (interactor :interactor))
  (:layouts (default-default (vertically ()
                               draw-pane
                               interactor)))
  (:menu-bar t)
  (:command-definer t) ; generates a command defining macro for this frame class
                       ; of form "define-<frame-name>-command
  (:top-level (default-frame-top-level))) ; special form used as top level command loop

(defclass draw-pane
  (standard-extended-input-stream ; must have precedence over basic-pane
    basic-pane
    permanent-medium-sheet-output-mixin)
  ())

(defmethod handle-repaint ((pane draw-pane) region)
  (with-application-frame (frame)
    (call-next-method) ; Paints the background
    (dolist (line (lines frame))
      (draw-line pane (car line) (cdr line)))
    (dolist (pair (strings frame))
      (draw-text pane (cdr pair) (car pair)))))

(define-draw-frame-command
  (com-draw-add-string :menu t :name t)
  ((string 'string) (x 'integer) (y 'integer))
  (push (cons (make-point x y) string)
        (strings *application-frame*))
  (update-draw-pane))

(define-draw-frame-command
  (com-draw-add-line :menu t :name t)
  ((x1 'integer) (y1 'integer) (x2 'integer) (y2 'integer))
  (with-slots (lines) *application-frame*
    (push (cons (make-point x1 y1) (make-point x2 y2))
          lines))
  (update-draw-pane))

(define-draw-frame-command
  (com-draw-clear :menu t :name t) ()
  (with-slots (lines strings) *application-frame*
    (setf lines nil strings nil))
  (update-draw-pane))

;; Auxiliary Method
(defun update-draw-pane ()
  (repaint-sheet (find-pane-named *application-frame* 'draw-pane) +everywhere+))

(defmethod handle-event ((pane draw-pane) (event pointer-button-press-event))
  ;; Start line tracking when left pointer button is pressed
  (when (eql (pointer-event-button event) +pointer-left-button+)
    (track-line-drawing pane
                        (pointer-event-x event)
                        (pointer-event-y event))))

(defmethod handle-event ((pane draw-pane) (event key-press-event))
  (when (keyboard-event-character event)
    (multiple-value-bind (x y) (stream-pointer-position pane)
      ;; Start with empty string, as a key release event will be received anyway
      (track-text-drawing pane "" x y)))
  (update-draw-pane))

(defun track-line-drawing (pane startx starty)
  (let ((lastx startx)
        (lasty starty))
    (with-drawing-options (pane :ink +flipping-ink+)
      (draw-line* pane startx starty lastx lasty)
      (tracking-pointer (pane)
        (:pointer-motion (&key window x y)
         (draw-line* pane startx starty lastx lasty) ; delete old
         (draw-line* pane startx starty x y) ; draw new
         (setq lastx x lasty y))
        (:pointer-button-release (&key event x y)
         (when (eql (pointer-event-button event) +pointer-left-button+)
           (draw-line* pane startx starty lastx lasty)
           (execute-frame-command *application-frame*
                                 `(com-draw-add-line ,startx ,starty ,x ,y))
           (return-from track-line-drawing nil)))))))

(defun track-text-drawing (pane current-string current-x current-y)
  (tracking-pointer (pane)
    (:pointer-motion (&key window x y)
     ;; can't use flipping ink for text, hence redraw
     (handle-repaint pane +everywhere+)
     (setq current-x x current-y y)
     (draw-text* pane current-string x y))
    (:keyboard (&key gesture)
     (when (and (typep gesture 'key-release-event)
                (keyboard-event-character gesture))
       (setf current-string
             (concatenate 'string
                          current-string
                          (string (keyboard-event-character gesture))))
       (handle-repaint pane +everywhere+)
       (draw-text* pane current-string current-x current-y)))
    (:pointer-button-release (&key event x y)
     (when (eql (pointer-event-button event) +pointer-left-button+)
       (excute-frame-command *application-frame*
                             `(com-draw-add-string ,current-string ,x ,y))
       (return-from track-text-drawing nil)))))

(run-frame-top-level (make-application-frame 'draw-frame))

;; Directory browser

(define-application-frame
  file-browser ()
  ((active-files :initform nil :accessor active-files))
  (:panes
    (file-browser :application
                  :display-function '(dirlist-display-files)
                  ;; Call the display-function whenever the command
                  ;; loop makes a full-cycle
                  :display-time :command-loop)
    (interactor :interactor))
  (:layouts (default (vertically ()
                                file-browser
                                interactor))))

(define-presentation-type dir-pathname ()
                          :inherit-from 'pathname)

(defmethod dirlist-display-files ((frame file-browser) pane)
  ;; Clear old displayed entries
  (clear-output-record (stream-output-history pane))

  (dolist (file (active-files frame))
    ;; Instead of write-string, use present to that the link to
    ;; object file and the semantic info of file is retained
    (present file
             (if (cl-fad:directory-pathname-p file) 'dir-pathname 'pathname) :stream pane)
    (terpri pane)))

(define-file-browser-command
  (com-edit-directory :name "Edit Directory")
  ((dir 'dir-pathname))
  (setf (active-files *application-frame*)
        (cl-fad:list-directory dir)))

(define-presentation-to-command-translator
  pathname-to-edit-command
  (dir-pathname ; source presentation-type
    com-edit-directory ; target-command
    file-browser ; command-table
    :gesture :select ; use this translator for pointer clicks
    :documentation "Edit this path") ; used in context menu
    (object) ; argument list
    (list object)) ; arguments for target-command

(define-file-browser-command (com-quit :name t) ()
                             (frame-exit *application-frame*))

(defmethod adopt-frame :after (frame-manager (frame file-browser))
  (declare (ignore frame-manager))
  (execute-frame-command frame
                         `(com-edit-directory ,(make-pathname :directory '(:absolute)))))

(run-frame-top-level (make-application-frame 'file-browser))
