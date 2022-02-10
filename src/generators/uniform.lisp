(in-package :cl-noise.gen.uniform)

(defun uniform (&rest unused)
  (declare (ignore unused))
  "Generate a random value of uniform noise in the range [-1.0, 1.0].

Takes any amount of arguments to allow use with MAKE-GENERATOR."
  (- (cl:random (+ 2.0 double-float-epsilon)) 1.0))

