;;;; Sandbox screen - for testing stuff

(in-package #:tswr-asteroids)



(defclass sandbox-screen (game-screen)
  ((stars :initform nil
          :accessor sandbox-stars))
  )



;;; The tested stuff



(defmethod on-phase-in ((screen sandbox-screen) previous-screen)
  (declare (ignore previous-screen))
  ;; TODO magic initialization of whatever's there to play with
  (setf (sandbox-stars screen) (make-starfield 200)))

(defmethod on-key-event ((screen sandbox-screen) key state repeat)
  (macrolet ((on-key-down (scancode &body code)
               `(when (and (eql key-code ,scancode)
                           (sdl2:key-down-p state))
                  ,@code)))
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
      (log:trace key state key-code repeat)

      (on-key-down :scancode-space
                   (make-starfield 200))

      (on-key-down :scancode-f1
                   (switch-game-screen :menu))

      (on-key-down :scancode-escape
                   (switch-game-screen :menu)))))

(defmethod on-render ((screen sandbox-screen) dt)
  
  (draw-starfield (sandbox-stars screen))

  (p2dg:with-color (1 1 1)
    (draw-text "Sandbox"
               :font +default-font+
               :size 16
               :x 790
               :y 590
               :alignment-x :right
               :alignment-y :top)))
