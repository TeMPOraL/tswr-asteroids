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
               
               (:file "game-constants")

               (:module "components"
                        :components ((:file "animation")
                                     (:file "asteroid")
                                     (:file "buff")
                                     (:file "bullet")
                                     (:file "collision-pair")
                                     (:file "collision-sphere")
                                     (:file "decays")
                                     (:file "drops-powerup")
                                     (:file "game-area-border-policy")
                                     (:file "gives-score")
                                     (:file "gun")
                                     (:file "kinematics")
                                     (:file "orientation")
                                     (:file "player-controlled")
                                     (:file "position")
                                     (:file "powerup")
                                     (:file "renderable")
                                     (:file "ship")))

               (:module "entities"
                        :components ((:file "entity-creator")))

               (:module "systems"
                        :components ((:file "input")
                                     (:file "basic-physics")
                                     (:file "collision-detector")
                                     (:file "collision-handler")
                                     (:file "game-area-customs")
                                     (:file "gun-cooldown-updater")
                                     (:file "decayer")
                                     (:file "ship-effects")
                                     (:file "animation-updater")
                                     (:file "renderer")))
               (:file "game-rules")
               (:module "screens"
                        :components ((:file "screens")
                                     (:file "screen-menu")
                                     (:file "screen-defeat")
                                     (:file "screen-get-ready")
                                     (:file "screen-main-game")))
               (:file "main")))
