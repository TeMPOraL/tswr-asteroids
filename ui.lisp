;;;; UI state and code to be shared throughout the game.

(in-package #:tswr-asteroids)

(defparameter +default-font+ "fonts/Vera/VeraMono.ttf")
(defparameter +bold-font+ "fonts/Vera/VeraMoBd.ttf")
(defparameter +default-text-size+ 10)



;;; XXX NOTE development code hacking away with engine's internals.
(defun draw-text (text &key (font +default-font+) (size +default-text-size+) (x 0.0) (y 0.0) (alignment-x :left) (alignment-y :center))

  (let* ((font (p2dg::get-rendered-font font :size size))
         (rendered-text (p2dg::render-text font text)))
    (when rendered-text

      ;; XXX this here basically reimplements #'p2dg::draw
      (let ((tx (case alignment-x
                  (:left x)
                  (:center (- x (/ (p2dg::width rendered-text) 2)))
                  (:right (- x (p2dg::width rendered-text)))
                  (otherwise x)))
            (ty (case alignment-y
                  (:top (- y (p2dg::height rendered-text)))
                  (:center (- y (/ (p2dg::height rendered-text) 2)))
                  (:bottom y)
                  (otherwise y))))
        (gl:with-pushed-matrix
          (gl:translate tx ty 0)

          (p2dg::%draw rendered-text)))

      ;; TODO maybe cache it instead of killing every frame?
      (p2dg::%free-text rendered-text))))

