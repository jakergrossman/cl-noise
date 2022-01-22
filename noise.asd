;;;; NOISE system definition

(in-package :asdf-user)

(defsystem #:noise
  :name "noise"
  :author "Jake Grossman <jake.r.grossman@gmail.com>"
  :license "Unlicense"
  :description "Noise generators"
  :depends-on ("zpng")
  :serial t
  :pathname "src"
  :components ((:file "packages")
               (:file "gen")
               (:file "image")))

               
(defsystem #:noise/binary
  :build-operation program-op
  :build-pathname "noise"
  :entry-point "noise:toplevel"
  :pathname "src"
  :depends-on ("noise" "unix-opts")
  :components ((:file "noise")))
