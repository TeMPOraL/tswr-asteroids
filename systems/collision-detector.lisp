(in-package #:tswr-asteroids)

(p2de:defsystem collision-detector
  (position collision-sphere))


;;; Collision layers.

(defparameter *collision-layers-alist* '() "An alist describing all layers a given layer can collide with.")

(defun layers-can-collide (layer-1 layer-2)
  "Registers `LAYER-1' and `LAYER-2' as mutually collideable."
  (flet ((collision-possible (collider collidee)
           (if (assoc collider *collision-layers-alist*)
               (pushnew collidee (cdr (assoc collider *collision-layers-alist*)))
               (push (list collider collidee) *collision-layers-alist*))))
    (collision-possible layer-1 layer-2)
    (collision-possible layer-2 layer-1)))

(defun layers-can-collide-p (layer-1 layer-2)
  (member layer-2 (cdr (assoc layer-1 *collision-layers-alist*))))

(layers-can-collide :bullet :asteroid)
(layers-can-collide :asteroid :ship)
(layers-can-collide :ship :powerup)



;;; NOTE
;;; Collisions are n^2 here.
;;; Possible optimization: split entities into separate lists, one for each collision layer
;;; and process only possible collisions.
;;; IDEA: In ECS, add on-register and on-unregister hooks.

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
         (layers-can-collide-p (slot-value s1 'layer)
                               (slot-value s2 'layer)))))

(defun make-collision-pair (entity-id other-id)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'collision-pair
                        :entity-1-id entity-id
                        :entity-2-id other-id)))


