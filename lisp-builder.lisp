(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
(ql:quickload "lispbuilder-sdl-examples")
(ql:quickload "lispbuilder-sdl-gfx-examples")

(defparameter *random-color* sdl:*white*)
(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 400 400 :title-caption "Move a rectangle with the mouse")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
      (:idle ()
       ;; change color of box if mouse depressed
       (when  (sdl:mouse-left-p)
         (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))

       ;; clear the display on each loop
       (sdl:clear-display sdl:*black*)

       ;; draw the box with center at mouse
       (sdl-gfx:draw-filled-trigon (sdl:point :x 140 :y 80) (sdl:point :x 100 :y 90) (sdl:point :x 40 :y 40)  :color *random-color*)
       ;; redraw display
       (sdl:update-display)))))
(mouse-rect-2d)

; (sdl-examples:fireworks)
; (sdl-examples:explosion)
; (sdl-examples:flood-fill)
; (sdl-examples:particles)
; (sdl-examples:random-rects)
; (sdl-gfx-examples:shape-primitives)
