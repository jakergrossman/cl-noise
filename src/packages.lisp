;;;; NOISE package definitions
;;;;
;;;; This is free and unencumbered software released in the public domain

(in-package :cl-user)

(eval-when (:execute)
  (setq *read-default-float-format* 'long-float))

(uiop:define-package #:cl-noise.gen.uniform
    (:use :cl)
  (:export
   :uniform))

(uiop:define-package #:cl-noise.gen.perlin
    (:use :cl)
  (:export
   :perlin
   :+ken-perlin-perm+))

(uiop:define-package #:cl-noise.gen.simplex
    (:use :cl)
  (:export :simplex)
  (:import-from :cl-noise.gen.perlin
                +ken-perlin-perm+))

(uiop:define-package #:cl-noise.gen
    (:use :cl)
  (:use-reexport
   :cl-noise.gen.uniform
   :cl-noise.gen.perlin
   :cl-noise.gen.simplex))

(uiop:define-package #:cl-noise
    (:use :cl)
  (:export
   :noise
   :dimensions
   :uniform-perm

   :make-noise-desc
   :noise-desc
   :noise-desc-generator
   :noise-desc-size
   :noise-desc-octaves
   :noise-desc-persistence
   :noise-desc-frequency-scale
   :noise-desc-sample-width
   :noise-desc-offset

   :make-buffer
   :make-generator

   :with-buffer
   :with-generator))
