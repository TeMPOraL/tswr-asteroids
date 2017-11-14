;;;; Defeat screen

(in-package #:tswr-asteroids)



(defparameter +default-defeat-screen-timeout+ 1.5)



(defclass defeat-screen (game-screen)
  ((time-to-switch :initform +default-defeat-screen-timeout+
                   :initarg :time-to-switch
                   :accessor defeat-time-to-switch)

   (switch-counter :initform 0
                   :accessor defeat-switch-counter)))



(defmethod on-phase-in ((screen defeat-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (defeat-switch-counter screen)
        (defeat-time-to-switch screen)))

(defmethod on-tick ((screen defeat-screen) dt)
  (decf (defeat-switch-counter screen) dt)
  (when (< (defeat-switch-counter screen) 0.0)
    (switch-game-screen :menu)))

(defmethod on-render ((screen defeat-screen) dt)
  (p2dg:with-color (1 0 0)
    (draw-text "DEFEAT!"
               :size 72
               :x 400
               :y 300
               :alignment-x :center)))

