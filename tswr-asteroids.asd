;;; -*- mode: lisp -*-
;;; tswr-asteroids.asd

(asdf:defsystem #:tswr-asteroids
  :serial t
  :long-name "TSW Redux - Asteroids"
  :author "Jacek Złydach"
  :version (:read-file-form "version.lisp" :at (1 2 2))
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

               (:module "components"
                        :components ((:file "asteroid")
                                     (:file "buff")
                                     (:file "bullet")
                                     (:file "collision-pair")
                                     (:file "collision-sphere")
                                     (:file "decays")
                                     (:file "drops-powerup")
                                     (:file "gives-score")
                                     (:file "gun")
                                     (:file "kinematics")
                                     (:file "orientation")
                                     (:file "player-controlled")
                                     (:file "position")
                                     (:file "powerup")
                                     (:file "renderable")
                                     (:file "ship")
                                     (:file "wraps-around")))

               (:module "entities"
                        :components ((:file "entity-creator")))

               (:module "systems"
                        :components ((:file "input")
                                     (:file "basic-physics")
                                     (:file "collision-detector")
                                     (:file "collision-handler")
                                     (:file "game-area-wrapper")
                                     (:file "gun-cooldown-updater")
                                     (:file "decayer")
                                     (:file "ship-effects")
                                     (:file "renderer")))
               (:file "game-rules")
               (:file "main")))
