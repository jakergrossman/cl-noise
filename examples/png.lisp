;;;; png.lisp
;;;;
;;;; Write noise data to images

(uiop:define-package #:cl-noise-png
    (:use :cl :cl-noise)
  (:export :toplevel :noise-png))

(in-package :cl-noise-png)

(defun noise-color-value (n)
  "Convert noise value N to a number in the range [0, 255]"
  (ash (truncate (* (1+ n) 255)) -1))

(defun noise-png (file desc)
  "Generates a PNG filled with noise from the generator described by DESC
with the generator arguments GEN-ARGS."
  (check-type file pathname)
  (check-type desc noise-desc)
  (with-open-file (stream file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))
    (let* ((size (noise-desc-size desc))
           (width (car size))
           (height (cadr size))
           (png (make-instance 'zpng:pixel-streamed-png :color-type :grayscale
                                                        :width width
                                                        :height height))
           (generator (make-generator desc)))
      (zpng:start-png png stream)
      (dotimes (i (* width height))
        (zpng:write-pixel (list (noise-color-value (funcall generator))) png))
      (zpng:finish-png png))))

(defun usage ()
  (opts:describe
   :prefix (format nil "Usage: png OPTIONS FILE [FILE ...]")))

(defun parse-size-string (str)
  "Parse all digits out of STR, treating consecutive digits as
base 10 numbers"

  (loop with len = (length str)
        for start = 0 then (position-if #'digit-char-p str :start end)
        while (and start (< start len))
        for (value end) = (multiple-value-list
                           (parse-integer str :start start :junk-allowed t))
        collect value))

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")

  (:name :size
   :description "The size of the noise as a comma-separated list in row-major order"
   :short #\s
   :long "size"
   :default (lambda () (list 3 3))
   :meta-var "ROWS,COLUMNS"
   :arg-parser #'parse-size-string)

  (:name :octaves
   :description "Number of octaves to use for perlin noise"
   :short #\o
   :long "octaves"
   :default 1
   :arg-parser #'parse-integer)

  (:name :persistence
   :description "Weight of each octave relative to the last"
   :short #\p
   :long "persistence"
   :default 0.5
   :arg-parser #'read-from-string)

  (:name :sample-width
   :description "Noise sample size"
   :short #\w
   :long "sample-width"
   :default 8.0
   :arg-parser #'read-from-string)

  (:name :generator
   :description (with-output-to-string (str)
                  (format str "Specifies the noise generation function~%")
                  (format str " One of [~{`~A'~^, ~}]"
                          '(:uniform :perlin :simplex)))
   :short #\g
   :long "generator"
   :default "perlin"
   :arg-parser #'string-upcase)

  (:name :ken-perlin-perm
   :description "Whether to use Ken Perlins original permutation"
   :short #\k
   :long "ken-perlin-perm")

  (:name :offset
   :description "Offset of first noise data point from origin"
   :long "offset"
   :meta-var "Y[,X]"
   :default (lambda () (list 0 0))
   :arg-parser #'parse-size-string))

(defun check-options (options)
  "Validate user options"
  (loop for (key value) on options by #'cddr do
    (case key
      (:help (usage) (return nil))

      (:size
       (unless (and (consp value) (= 2 (length value)))
         (format *error-output*
                 "Size should be of the form ROWS,COLUMNS, e.g.: 2,3~%")
         (usage)
         (return nil)))

      (:octaves
       (unless (typep value '(integer 1 *))
         (format *error-output*
                 "Octaves must be an integer >= 1! (~A)~%"
                 value)
         (usage)
         (return nil)))

      (:persistence
       (unless (typep value '(and real (satisfies plusp)))
         (format *error-output*
                 "Persistence must be greater than 0! (~A)~%"
                 value)
         (usage)
         (return nil)))

      (:sample-width
       (unless (typep value '(and real (satisfies plusp)))
         (format *error-output*
                 "Sample-Width must be greater than 0! (~A)~%"
                 value)
         (usage)
         (return nil)))

      (:generator
       (unless (find (intern (string-upcase value) "KEYWORD")
                     (list :uniform :perlin :simplex))
         (format *error-output*
                 "Generator must be one of: `uniform', `perlin', `simplex'. (~A)~%"
                 value)
         (usage)
         (return nil)))

      (:offset
       (unless (typep value '(cons number))
         (format *error-output*
                 "Offset should be of the form Y[,X]!~%")
         (usage)
         (return nil)))

      ;; flag trap
      ((:ken-perlin-perm))

      (otherwise
       (error "Unhandled option ~A: ~A!~%" key value)))

      finally (return t)))

(declaim (inline describe-png))
(defun describe-png (path desc &optional (stream *standard-output*))
  (let* ((size               (noise-desc-size desc))
         (octaves            (noise-desc-octaves desc))
         (persistence        (noise-desc-persistence desc))
         (noise-sample-width (noise-desc-sample-width desc))
         (generator          (noise-desc-generator desc))
         (offset             (noise-desc-offset desc)))
    (format stream "~&Generating PNG `~A'~%" path)
    (format stream "  Generator: ~A~%" generator)
    (format stream "  Size: ~{~A~^x~}~%" size)
    (format stream "  Octaves: ~A~%" octaves)
    (format stream "  Persistence: ~A~%" persistence)
    (format stream "  Noise sample width: ~A~%" noise-sample-width)
    (format stream "  Offset: ~{~A~^,~}~%" offset)))

(defun run (args)
  "Run PNG program as if ARGS where the command line arguments"
  (multiple-value-bind (options free-args) (opts:get-opts args)
    (when (check-options options)
      (when (not free-args)
        (format *error-output* "No files passed!~%")
        (usage))
      (setf *random-state* (make-random-state t))
      (let* ((size                       (getf options :size))
             (octaves                    (getf options :octaves))
             (persistence                (getf options :persistence))
             (noise-sample-width         (getf options :sample-width))
             (offset                     (getf options :offset))
             (generator   (string-upcase (getf options :generator)))
             (ken-perlin-p               (getf options :ken-perlin-perm))
             (desc (make-noise-desc
                    :generator (intern generator "CL-NOISE.GEN")
                    :generator-keys (unless ken-perlin-p
                                      (list :perm (uniform-perm 256)))

                    :size size
                    :offset offset
                    :octaves octaves
                    :persistence persistence
                    :sample-width noise-sample-width)))
        (dolist (path free-args)
          (ecase (intern generator "KEYWORD")
            ((:uniform :perlin :simplex)
             (describe-png path desc)
             (noise-png (pathname path) desc))))))))

(defun toplevel ()
  #+:SBCL
  (sb-ext:disable-debugger)
  (let ((args (opts:argv)))
    (unless (cdr args)
      (usage)
      (opts:exit))
    (run (cdr args))))
