;;;; Get ready screen

(in-package #:tswr-asteroids)



(defclass get-ready-screen (banner-screen)
  ())



(defmethod on-render ((screen get-ready-screen) dt)
  (declare (ignore dt))
  (p2dg:with-color (0 1 0)
    (draw-text "GET READY!"
               :size 72
               :x 400
               :y 300
               :alignment-x :center)))

