(in-package #:tswr-asteroids)

(p2de:defsystem collision-detector
  (position collision-sphere))

(defmethod p2de:do-system ((system collision-detector) entity dt)
  (declare (ignore dt))
  (dolist (other-id (p2de::entities system)) ;FIXME accessing unexported function
    (let ((other (p2de:entity-by-id other-id)))
      (when (and (not (eql (p2de:entity-id entity) other-id))
                 (collidep entity other))
        (make-collision-pair (p2de:entity-id entity) other-id)))))

(defun collidep (entity other)
  (let* ((p1 (p2de:find-component entity 'position))
         (p2 (p2de:find-component other 'position))
         (s1 (p2de:find-component entity 'collision-sphere))
         (s2 (p2de:find-component other 'collision-sphere))
         (distance-squared (p2dm:distance-between-vectors-squared (slot-value p1 'position)
                                                                  (slot-value p2 'position)))
         (threshold (p2dm:square (float (+ (slot-value s1 'radius)
                                           (slot-value s2 'radius))))))
    (and (< distance-squared threshold)
         (layers-can-collide (slot-value s1 'layer)
                             (slot-value s2 'layer)))))

(defun make-collision-pair (entity-id other-id)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'collision-pair
                        :entity-1-id entity-id
                        :entity-2-id other-id)))


(defun layers-can-collide (layer-1 layer-2)
  ;; FIXME make it declarative and automatically SYMMETRICAL
  (cond ((eql layer-1 :bullet)
         (eql layer-2 :asteroid))
        
        ((eql layer-1 :asteroid)
         (or (eql layer-2 :bullet)
             (eql layer-2 :ship)))
        
        ((eql layer-1 :ship)
         (or (eql layer-2 :asteroid)
             (eql layer-2 :powerup)))
        
        ((eql layer-1 :powerup)
         (eql layer-2 :ship))
        (t nil)))
