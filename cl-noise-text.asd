(defsystem #:cl-noise-text
  :version "0.1"
  :author "Jake Grossman"
  :description "A noise text generator"
  :license "Unlicense"
  :depends-on ("cl-noise")
  :serial t
  :components ((:file "examples/text")))

(defsystem #:cl-noise-text/binary
  :depends-on ("cl-noise-text")
  :build-operation program-op
  :build-pathname "text"
  :entry-point "cl-noise-text:toplevel")
