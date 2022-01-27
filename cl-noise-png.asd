(defsystem #:cl-noise-png
  :version "0.1"
  :author "Jake Grossman"
  :description "A noise PNG generator"
  :license "Unlicense"
  :depends-on ("cl-noise")
  :serial t
  :components ((:file "examples/png")))

(defsystem #:cl-noise-png/binary
  :depends-on ("cl-noise-png")
  :build-operation program-op
  :build-pathname "png"
  :entry-point "cl-noise-png:toplevel")
