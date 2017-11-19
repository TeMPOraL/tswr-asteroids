(in-package #:tswr-asteroids)

(defparameter *debug-render-kinematics* nil "Draw kinematics information on the objects.")
(defparameter *debug-render-collision* nil "Draw collision data on the objects.")

(defparameter +debug-color-velocity+ (p2dg:make-color-4 1.0 0.0 0.0 1.0))
(defparameter +debug-color-acceleration+ (p2dg:make-color-4 0.0 1.0 0.0 1.0))

(p2de:defsystem renderer
  (position renderable))

(defmethod p2de:do-system ((system renderer) entity dt)
  (declare (ignore system))
  (with-slots (position) (p2de:find-component entity 'position)
    (with-slots (sprite color scale) (p2de:find-component entity 'renderable)
      ;;
      (gl:load-identity)
      (gl:with-pushed-matrix

        ;; Draw actual graphics
        (p2dglu:translate2 position)
        (gl:with-pushed-matrix
          (if-let ((orientation (p2de:find-component entity 'orientation)))
            (p2dglu:rotatez* (orientation orientation)))
          (p2dglu:scale2-uniform scale)
          (draw-pseudo-sprite sprite color dt))

        ;; Debug - rendering kinematics
        (when *debug-render-kinematics*
          (when-let ((kinematics (p2de:find-component entity 'kinematics)))
            (gl:with-pushed-matrix
              (p2dglu:color4 +debug-color-velocity+)
              (draw-vector-marker-to-point (slot-value kinematics 'velocity))
              (p2dglu:color4 +debug-color-acceleration+)
              (draw-vector-marker-to-point (slot-value kinematics 'acceleration)))))

        ;; Debug - rendering collisions
        (when *debug-render-collision*
          (when-let ((bounding-sphere (p2de:find-component entity 'collision-sphere)))
            (gl:with-pushed-matrix
              (p2dglu:color4 (make-color-from-layer (slot-value bounding-sphere 'layer)))
              (p2dglu:scale2-uniform (slot-value bounding-sphere 'radius))
              (p2dglu:draw-circle-outline))))))))

(defun draw-pseudo-sprite (sprite color dt)
  "Pseudo because it's not really a `SPRITE', but rather a function wrapping immediate-mode geometry calls."
   (funcall sprite color dt))

(defun draw-vector-marker-to-point (point)
  "Draws a vector marker from (0 0) to `POINT'."
  (gl:with-primitive :lines
                     (gl:vertex 0.0 0.0)
                     (gl:vertex (p2dm:vec-x point) (p2dm:vec-y point))))

(defun make-color-from-layer (layer)
  ;; TODO actually make different colors for different collision layers
  (if (eql layer :undefined)
      (p2dg:make-color-4 1.0 0.0 0.0 1.0)
      (p2dg:make-color-4 1.0 1.0 1.0 1.0)))

(defun renderer-toggle-debug-kinematics ()
  (setf *debug-render-kinematics* (not *debug-render-kinematics*)))

(defun renderer-toggle-debug-collision ()
  (setf *debug-render-collision* (not *debug-render-collision*)))


;;; SPRITES
;;; Sprites are lambdas doing actual rendering code.
;;; XXX We abuse the fact that, at least on SBCL, lambdas that don't close over anything
;;; will be returned directly, as _the same_ object, so they're essentially free to create
;;; and don't use up memory.

(defun make-ship-sprite ()
  (lambda (color dt)
    (declare (ignore dt))
    (p2dglu:color4 color)
    (gl:scale 1 1.5 1)
    (p2dglu:draw-triangle-outline)
    (when (has-respawn-shield)
      (p2dg:with-color (0 0 1)
        (gl:line-width 2.0)
        (p2dglu:draw-circle-outline)
        (gl:line-width 1.0) ;FIXME that's magic knowledge that this is the default
        ))))

(defun make-ship-accelerating-sprite ()
  (lambda (color dt)
    (declare (ignore dt))
    (p2dglu:color4 color)
    (gl:scale 1 1.5 1)
    (p2dglu:draw-triangle-outline)
    (when (has-respawn-shield)
      (p2dg:with-color (0 0 1)
        (p2dglu:draw-circle-outline)))
    (gl:scale 0.5 1.5 0.5)
    (p2dglu:draw-triangle-outline)))

(defun make-bullet-sprite ()
  (lambda (color dt)
    (declare (ignore dt))
    (p2dglu:color4 color)
    (p2dglu:draw-circle-outline :resolution 16)))

(defun make-powerup-sprite ()
  (lambda (color dt)
    (declare (ignore dt))
    (p2dglu:color4 color)
    (p2dglu:rotatez* (p2dm:deg->rad 45.0))
    (p2dglu:scale2-uniform 0.75)
    (p2dglu:draw-square-outline)))

(defun make-asteroid-sprite ()
  (flet ((noise-fun ()
           (- 1.0 (p2dm:random-float -0.1 0.1))))
   (let* ((polygon-sides (+ 32 (random 32)))
          (step-angle (coerce (/ p2dm:+2pi+ polygon-sides) 'p2dm:standard-float))
          (vertex-pairs '()))
     (dotimes (step polygon-sides)
       (let ((noise-val (noise-fun)))
        (push (cons (* noise-val (cos (* step step-angle)))
                    (* noise-val (sin (* step step-angle))))
              vertex-pairs)))
     (lambda (color dt)
       (declare (ignore dt))

       ;; Draw black background to pretend asteroid is solid
       (p2dg:with-color (0 0 0)
         (gl:With-primitive :polygon
           (dolist (point vertex-pairs)
             (gl:vertex (car point) (cdr point)))))

       ;; Draw actual outline
       (p2dglu:color4 color)
       (gl:with-primitive :line-loop
         (dolist (point vertex-pairs)
           (gl:vertex (car point) (cdr point))))))))
