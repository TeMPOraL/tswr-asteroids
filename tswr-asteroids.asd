;;; -*- mode: lisp -*-
;;; tswr-asteroids.asd

(asdf:defsystem #:tswr-asteroids
  :serial t
  :long-name "TSW Redux - Asteroids"
  :author "Jacek Złydach"
  :version (:read-file-from "version.lisp" :at (1 2 2))
  :description "A toy game."
                                        ; :long-description "todo"

  :license "MIT"
  :homepage "https://github.com/TeMPOraL/tswr-asteroids"
  :bug-tracker "https://github.com/TeMPOraL/tswr-asteroids/issues"
  :source-control (:git "https://github.com/TeMPOraL/tswr-asteroids.git")
  :mailto "temporal.pl+tswr@gmail.com"

  :encoding :utf-8

  :depends-on (#:alexandria
               #:parendeck2d)

  :components ((:file "package")
               (:file "version")

               (:module "entities"
                        :components ((:file "entity")
                                     (:file "asteroid")
                                     (:file "bullet")
                                     (:file "ship")))
               (:file "main")))
