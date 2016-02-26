(in-package #:tswr-asteroids)

(defclass entity ()
  ((position :initarg :position
             :initform (p2dm:make-vector-2d)
             :type p2dm:vector-2d)
   (velocity :initarg :velocity
             :initform (p2dm:make-vector-2d)
             :type p2dm:vector-2d)
   (acceleration :initarg :acceleration
                 :initform (p2dm:make-vector-2d)
                 :type p2dm:vector-2d)

   (speed-limit :initarg :speed-limit
                :initform 500.0)

   (orientation :initarg :orientation
                :initform p2dm:+standard-float-zero+
                :type p2dm:standard-float)
   
   (angular-velocity :initarg :angular-velocity
                     :initform p2dm:+standard-float-zero+
                     :type p2dm:standard-float)))

(defgeneric update-motion (entity dt)
  (:documentation "Updates the entity's position based on physics parameters."))

(defgeneric update-logic (entity dt)
  (:documentation "Updates the game logic behind this entity."))

(defgeneric deadp (entity)
  (:documentation "Checks if the entity is dead and thus ready to remove."))

(defgeneric render (entity)
  (:documentation "Renders the entity."))

;;; default implementations
(defmethod update-motion (entity dt)
  (with-slots (position velocity acceleration speed-limit orientation angular-velocity) entity
    (p2dm:add-to-vector position
                        (p2dm:scaled-vector velocity dt))
    (p2dm:add-to-vector velocity
                        (p2dm:scaled-vector acceleration dt))

    (if (> (p2dm:vector-value velocity) speed-limit)
        (setf velocity (p2dm:scaled-vector (p2dm:normalized-vector velocity)
                                           speed-limit)))

    (incf orientation (* angular-velocity dt))))

(defmethod update-logic (entity dt)
  nil)

(defmethod render (entity)
  (error "Not yet implemented, but should be."))

(defmethod deadp (entity)
  return nil)

