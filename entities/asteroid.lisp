(in-package #:tswr-asteroids)

(defclass asteroid (entity)
  ((color :initarg :color
          :initform (p2dm:make-vector-4d))
   
   (size :initarg :size
         :initform 4.0)))

(defmethod render ((asteroid asteroid))
  (gl:load-identity)

  (with-slots (position orientation color size) asteroid
    (gl:translate (p2dm:vec-x position)
                  (p2dm:vec-y position)
                  0.0)
    (gl:rotate orientation 0 0 1)
    (gl:scale size size 0.0)

    (gl:color (p2dm:vec-x color) (p2dm:vec-y color) (p2dm:vec-z color) (p2dm:vec-w color))

    (gl:with-primitive :line-loop
      (gl:vertex 10.0 -5.0)
      (gl:vertex 10.0 5.0)
      (gl:vertex 5.0 10.0)
      (gl:vertex -5.0 10.0)
      (gl:vertex -10.0 5.0)
      (gl:vertex -10.0 -5.0)
      (gl:vertex -5.0 -10.0)
      (gl:vertex 5.0 -10.0))))