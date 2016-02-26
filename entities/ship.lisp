(in-package #:tswr-asteroids)

(defclass ship (entity)
  ((color :initarg :color
          :initform (p2dm:make-vector-4d 0.0 0.0 0.0 1.0))))

(defmethod render ((ship ship))
  (gl:load-identity)

  (with-slots (position orientation color) ship
    (gl:translate (p2dm:vec-x position)
                  (p2dm:vec-y position)
                  0.0)
    (gl:rotate orientation 0 0 1)

    (gl:color (p2dm:vec-x color) (p2dm:vec-y color) (p2dm:vec-z color) (p2dm:vec-w color))

    (gl:with-primitive :triangles
      (gl:vertex 0.0 15.0)
      (gl:vertex -10.0 -15.0)
      (gl:vertex 10.0 -15.0))))
