(in-package #:tswr-asteroids)

(defparameter *debug-render-kinematics* nil "Draw kinematics information on the objects.")
(defparameter *debug-render-collision* nil "Draw collision data on the objects.")

(defparameter +debug-color-velocity+ (p2dg:make-color-4 1.0 0.0 0.0 1.0))
(defparameter +debug-color-acceleration+ (p2dg:make-color-4 0.0 1.0 0.0 1.0))

(p2de:defsystem renderer
  (position renderable))

(defmethod p2de:do-system ((system renderer) entity dt)
  (declare (ignore system dt))
  (with-slots (position) (p2de:find-component entity 'position)
    (with-slots (sprite color scale) (p2de:find-component entity 'renderable)
      ;;
      (gl:load-identity)
      (gl:with-pushed-matrix
        (p2dg:translate2 position)
        (gl:with-pushed-matrix
          (if-let ((orientation (p2de:find-component entity 'orientation)))
            (p2dg:rotatez* (orientation orientation)))
          (p2dg:scale2-uniform scale)
          (p2dg:color4 color)
          (draw-pseudo-sprite sprite))
        (when *debug-render-kinematics*
          (when-let ((kinematics (p2de:find-component entity 'kinematics)))
            (gl:with-pushed-matrix
              (p2dg:color4 +debug-color-velocity+)
              (draw-vector-marker-to-point (slot-value kinematics 'velocity))
              (p2dg:color4 +debug-color-acceleration+)
              (draw-vector-marker-to-point (slot-value kinematics 'acceleration)))))
        (when *debug-render-collision*
          (when-let ((bounding-sphere (p2de:find-component entity 'collision-sphere)))
            (gl:with-pushed-matrix
              (p2dg:color4 (make-color-from-layer (slot-value bounding-sphere 'layer)))
              (p2dg:scale2-uniform (slot-value bounding-sphere 'radius))
              (p2dg:draw-circle-outline))))))))

(defun draw-pseudo-sprite (sprite)
  "Pseudo because it's not really a `SPRITE', but rather immediate-mode geometry."
  (case sprite
    (:asteroid (p2dg:draw-regular-polygon-outline 7))
    (:ship (gl:scale 1 1.5 1)
           (p2dg:draw-triangle))
    (:ship-accelerating (gl:scale 1 1.5 1)
                        (p2dg:draw-triangle)
                        (gl:scale 0.5 1.5 0.5)
                        (p2dg:draw-triangle))
    (:bullet (p2dg:draw-circle-outline :resolution 16))
    (t (p2dg:draw-square))))

(defun draw-vector-marker-to-point (point)
  "Draws a vector marker from (0 0) to `POINT'."
  (gl:with-primitive :lines
                     (gl:vertex 0.0 0.0)
                     (gl:vertex (p2dm:vec-x point) (p2dm:vec-y point))))

(defun make-color-from-layer (layer)
  (if (eql layer :undefined)
      (p2dg:make-color-4 1.0 0.0 0.0 1.0)
      (p2dg:make-color-4 1.0 1.0 1.0 1.0)))

(defun renderer-toggle-debug-kinematics ()
  (setf *debug-render-kinematics* (not *debug-render-kinematics*)))

(defun renderer-toggle-debug-collision ()
  (setf *debug-render-collision* (not *debug-render-collision*)))
