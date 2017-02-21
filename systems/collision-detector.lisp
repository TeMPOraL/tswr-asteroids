(in-package #:tswr-asteroids)

(defparameter *debug-collision-checks-per-tick* 0)

(p2de:defsystem collision-detector
  (position collision-sphere))


;;; Collision layers.

(defparameter *collision-layers-alist* '() "An alist describing all layers a given layer can collide with.")
(defparameter *collision-layers-entities-lists* '() "An alist mapping layer -> entities that are on this layer.")

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

(defun layers-that-can-collide-with (layer)
  (cdr (assoc layer *collision-layers-alist*)))

(defun entities-for-collision-layer (layer)
  (cdr (assoc layer *collision-layers-entities-lists*)))

(layers-can-collide :bullet :asteroid)
(layers-can-collide :asteroid :ship)
(layers-can-collide :ship :powerup)



(defmethod initialize-instance :after ((system collision-detector) &key)
  (setf *collision-layers-entities-lists* '()))

(defmethod p2de:do-system ((system collision-detector) entity dt)
  (declare (ignore dt))
  (dolist (collision-layer (layers-that-can-collide-with (slot-value (p2de:find-component entity 'collision-sphere) 'layer)))
    (dolist (other-id (entities-for-collision-layer collision-layer))
      (p2dprof:with-profiling (:collision-checks :description "collision checks per frame")
       (let ((other (p2de:entity-by-id other-id)))
         (when (and (not (eql (p2de:entity-id entity) other-id))
                    (collidep entity other))
           (make-collision-pair (p2de:entity-id entity) other-id)))))))

(defmethod p2de:entity-added ((system collision-detector) entity)
  (when-let ((id (p2de:entity-id entity))
             (collision-sphere (p2de:find-component entity 'collision-sphere)))
    (let ((layer (slot-value collision-sphere 'layer)))
      (if (assoc layer *collision-layers-entities-lists*)
          (pushnew id (cdr (assoc layer *collision-layers-entities-lists*)))
          (push (list layer id) *collision-layers-entities-lists*)))))

(defmethod p2de:entity-removed ((system collision-detector) entity)
  (when-let ((id (p2de:entity-id entity)))
    (dolist (layer-entities *collision-layers-entities-lists*)
      (deletef layer-entities id))))

(defun collidep (entity other)
  (let* ((p1 (p2de:find-component entity 'position))
         (p2 (p2de:find-component other 'position))
         (s1 (p2de:find-component entity 'collision-sphere))
         (s2 (p2de:find-component other 'collision-sphere))
         (distance-squared (p2dm:distance-between-vectors-squared (slot-value p1 'position)
                                                                  (slot-value p2 'position)))
         (threshold (p2dm:square (float (+ (slot-value s1 'radius)
                                           (slot-value s2 'radius))))))

    (incf *debug-collision-checks-per-tick*)
    (and (< distance-squared threshold)
         (layers-can-collide-p (slot-value s1 'layer)
                               (slot-value s2 'layer)))))

(defun make-collision-pair (entity-id other-id)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'collision-pair
                        :entity-1-id entity-id
                        :entity-2-id other-id)))


