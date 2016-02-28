(in-package #:tswr-asteroids)

(defclass ~bullet (entity)
  ((color :initarg :color
          :initform (p2dm:make-vector-4d 0.0 0.0 0.0 1.0))
   (life-time :initarg :life-time
              :initform 2.0)
   
   (speed-limit :initform 1000)))

(defmethod update-logic ((~bullet ~bullet) dt)
  (decf (slot-value ~bullet 'life-time) dt))

(defmethod deadp ((~bullet ~bullet))
  (<= (slot-value ~bullet 'life-time) 0.0))

(defmethod render ((~bullet ~bullet))
  (gl:load-identity)
  (with-slots (position color) ~bullet
    (gl:translate (p2dm:vec-x position)
                  (p2dm:vec-y position)
                  0.0)

    (gl:color (p2dm:vec-x color) (p2dm:vec-y color) (p2dm:vec-z color) (p2dm:vec-w color))
    (gl:scale 2.5 2.5 1.0)
    (p2dg:draw-circle-outline :resolution 16)))
