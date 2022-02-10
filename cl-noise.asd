;;;; CL-NOISE system definition

(in-package :asdf-user)

(defsystem #:cl-noise
  :name "cl-noise"
  :author "Jake Grossman <jake.r.grossman@gmail.com>"
  :license "Unlicense"
  :description "Noise generators"
  :depends-on ("alexandria" "unix-opts" "zpng")
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:module generators
                           :pathname "generators"
                           :components ((:file "uniform")
                                        (:file "perlin")
                                        (:file "simplex")))
               (:file "cl-noise")))
