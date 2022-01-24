;;;; LxNOISE system definition

(in-package :asdf-user)

(defsystem #:noise
  :name "noise"
  :author "Jake Grossman <jake.r.grossman@gmail.com>"
  :license "Unlicense"
  :description "Noise generators"
  :depends-on ("alexandria")
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:file "gen")))

(defsystem #:noise/png
  :depends-on ("noise" "unix-opts" "zpng")
  :build-operation program-op
  :build-pathname "bin/lnoise-png"
  :entry-point "noise.image:toplevel"
  :components ((:file "src/image")))

(defsystem #:noise/txt
  :depends-on ("noise" "unix-opts" "alexandria")
  :build-operation program-op
  :build-pathname "bin/lnoise"
  :entry-point "noise.text:toplevel"
  :components ((:file "src/text")))
