;;;; UI state and code to be shared throughout the game.

(in-package #:tswr-asteroids)

(defparameter +default-font+ "fonts/Vera/VeraMono.ttf")
(defparameter +bold-font+ "fonts/Vera/VeraMoBd.ttf")
(defparameter +default-text-size+ 10)


;;; Layout constants
(defparameter +layout-title-y-centerline+ 400)
(defparameter +layout-secondary-first-centerline+ 250)


;;; Magic text cache
;;; It's pretty dumb - just grows indefinetly, without removing long-unused values.
(defparameter *magic-text-cache* (make-hash-table :test 'equal))

(defun clear-magic-text-cache ()
  (log:debug (hash-table-count *magic-text-cache*))
  (maphash-values (lambda (text)
                    (p2dg::%free-text text))
                  *magic-text-cache*)
  (clrhash *magic-text-cache*))



;;; XXX NOTE development code hacking away with engine's internals.
(defun draw-text (text &key (font +default-font+) (size +default-text-size+) (x 0.0) (y 0.0) (alignment-x :left) (alignment-y :center) (transient nil))
  "`TRANSIENT' means this particular text might not show up again often, or ever, so there's no point in caching it."
  (labels ((render-text ()
             (p2dg::render-text (p2dg::get-rendered-font font :size size) text))
           (key ()
             `(,font ,size ,text))
           (get-from-cache ()
             (unless transient
               (gethash (key) *magic-text-cache*)))
           (maybe-store-in-cache (what)
             (unless transient
               (setf (gethash (key) *magic-text-cache*) what))
             what))
    (declare (dynamic-extent (function render-text) (function key) (function get-from-cache) (function maybe-store-in-cache)))

    (let ((rendered-text (or (get-from-cache)
                             (maybe-store-in-cache (render-text)))))
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

        (when transient
          (p2dg::%free-text rendered-text))))))

