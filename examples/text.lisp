;;;; text.lisp
;;;;
;;;; Write noise data to text

(uiop:define-package #:cl-noise-text
    (:use :cl :cl-noise)
  (:export :toplevel :noise-text))

(in-package :cl-noise-text)

(defun noise-text (desc &optional (stream *standard-output*))
  "Output 1 or 2 dimensional noise described by DESC to STREAM"
  (check-type desc cl-noise:noise-desc)
  (let ((gen (cl-noise:make-generator desc))
        (size (cl-noise:noise-desc-size desc)))
    (unless (<= (length size) 2)
      (error "noise-text only supports 1 or 2 dimensional noise, not ~A!~%"
             (length size)))

    (loop with minor-size = (first (last size))
          for row-major-pos from 1
          for noise = (funcall gen)
          for char-suffix = (if (zerop (mod row-major-pos minor-size))
                                #\Newline
                                #\Space)
          while noise do
          (format stream "~F" noise)
          (format stream "~c" char-suffix))))

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
   :meta-var "ROWS[,COLUMNS]"
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

(defun usage ()
  (opts:describe
   :prefix (format nil "Usage: text OPTIONS [FILE ...]")))

(defun check-options (options)
  (loop for (key value) on options by #'cddr do
    (case key
      (:help (usage) (return nil))

      (:size
       (unless (typep value '(cons (integer 1 *)))
         (format *error-output*
                 "Size should be of the form ROWS[,COLUMNS], e.g.: 2,3~%")
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

(declaim (inline describe-text))
(defun describe-text (path desc &optional (stream *standard-output*))
  (let* ((size               (noise-desc-size desc))
         (octaves            (noise-desc-octaves desc))
         (persistence        (noise-desc-persistence desc))
         (noise-sample-width (noise-desc-sample-width desc))
         (generator          (noise-desc-generator desc))
         (offset             (noise-desc-offset desc)))
    (format stream "~&Generating text `~A'~%" path)
    (format stream "  Generator: ~A~%" generator)
    (format stream "  Size: ~{~A~^x~}~%" size)
    (format stream "  Octaves: ~A~%" octaves)
    (format stream "  Persistence: ~A~%" persistence)
    (format stream "  Noise sample width: ~A~%" noise-sample-width)
    (format stream "  Offset: ~{~A~^,~}~%" offset)))

(defun run (args)
  "Run text program as if ARGS where the command line arguments"
  (multiple-value-bind (options free-args) (opts:get-opts args)
    (when (check-options options)
      (setf *random-state* (make-random-state t))
      (let* ((size                       (getf options :size))
             (octaves                    (getf options :octaves))
             (persistence                (getf options :persistence))
             (noise-sample-width         (getf options :sample-width))
             (generator   (string-upcase (getf options :generator)))
             (ken-perlin-p               (getf options :ken-perlin-perm))
             (offset                     (getf options :offset))
             (desc (cl-noise:make-noise-desc
                    :generator (intern generator "CL-NOISE.GEN")
                    :generator-keys (unless ken-perlin-p
                                      (list :perm (cl-noise:uniform-perm 256)))

                    :size size
                    :offset offset
                    :octaves octaves
                    :persistence persistence
                    :sample-width noise-sample-width)))
        (cond
          (free-args (dolist (path free-args)
                       (describe-text path desc)
                       (with-open-file (stream path
                                               :direction :output
                                               :if-exists :supersede
                                               :if-does-not-exist :create)

                         (noise-text desc stream))))
          (t (noise-text desc)))))))

(defun toplevel ()
  #+:SBCL
  (sb-ext:disable-debugger)
  (run (cdr (opts:argv))))
