;;;; Get ready screen

(in-package #:tswr-asteroids)



(defparameter +default-banner-screen-timeout+ 1.5)



(defclass get-ready-screen (game-screen)
  ((new-game-p :initform nil
               :accessor get-ready-new-game-p)

   (time-to-switch :initform +default-banner-screen-timeout+
                   :initarg :time-to-switch
                   :accessor get-ready-time-to-switch)

   (switch-counter :initform 0
                   :accessor get-ready-switch-counter)))



(defmethod on-phase-in :before ((screen get-ready-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (get-ready-switch-counter screen)
        (get-ready-time-to-switch screen)))

(defmethod on-phase-in ((screen get-ready-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (get-ready-new-game-p screen) nil))

(defmethod on-phase-in ((screen get-ready-screen) (previous-screen menu-screen))
  (declare (ignore previous-screen))
  (setf (get-ready-new-game-p screen) t))

(defmethod on-tick ((screen get-ready-screen) dt)
  (decf (get-ready-switch-counter screen) dt)
  (when (< (get-ready-switch-counter screen) 0.0)
    (switch-game-screen :main-game)))

(defmethod on-render ((screen get-ready-screen) dt)
  (p2dg:with-color (0 1 0)
    (draw-text "GET READY!"
               :size 72
               :x 400
               :y 300
               :alignment-x :center)))

