;;;; NOISE package definitions
;;;;
;;;; This is free and unencumbered software released in the public domain

(in-package :cl-user)

;;;; entry point

(uiop:define-package #:noise
  (:use :cl)
  (:export :toplevel))

(uiop:define-package #:noise.gen
  (:use :cl)
  (:export :uniform
           :uniform-perm
           :perlin
           :perlin-generator
           :perlin-buffer
           :with-perlin-defaults))

(uiop:define-package #:noise.image
  (:use :cl)
  (:export :toplevel))

(uiop:define-package #:noise.text
  (:use :cl)
  (:export :toplevel))
