(in-package #:tswr-asteroids)

(defclass entity ()
  ((position :initarg :position
             :initform (p2dm:make-vector-2d)
             :type p2dm:vector-2d)
   (velocity :initarg :velocity
             :initform (p2dm:make-vector-2d)
             :type p2dm:vector-2d)

   (orientation :initarg :orientation
                :initform p2dm:+standard-float-zero+
                :type p2dm:standard-float)
   
   (angular-velocity :initarg angular-velocity
                     :initform p2dm:+standard-float-zero+
                     :type p2dm:standard-float)))

(defgeneric update-motion (entity dt)
  (:documentation "Upsates the entity's position based on physics parameters."))

(defgeneric render (entity)
  (:documentation "Renders the entity."))

;;; defaults
(defmethod update-motion (entity dt)
  (with-slots (position velocity orientation angular-velocity) entity
    (p2dm:add-to-vector position
                        (p2dm:scaled-vector velocity dt))

    (incf orientation (* angular-velocity dt))))

