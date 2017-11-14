;;;; Banner screen

(in-package #:tswr-asteroids)



(defparameter +default-banner-screen-timeout+ 1.5)



(defclass banner-screen (game-screen)
  ((time-to-switch :initform +default-banner-screen-timeout+
                   :initarg :time-to-switch
                   :accessor banner-screen-time-to-switch
                   :type real)
   (switch-counter :initform 0
                   :accessor banner-screen-switch-counter
                   :type real)
   (next-screen :initform (error "Need to specify next screen!")
                :initarg :next-screen
                :accessor banner-screen-next-screen)))

(defmethod on-phase-in ((screen banner-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (banner-screen-switch-counter screen)
        (banner-screen-time-to-switch screen)))

(defmethod on-tick ((screen banner-screen) dt)
  (decf (banner-screen-switch-counter screen) dt)
  (when (< (banner-screen-switch-counter screen) 0.0)
    (switch-game-screen (banner-screen-next-screen screen))))
