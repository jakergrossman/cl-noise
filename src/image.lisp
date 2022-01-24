;;;; image.lisp
;;;;
;;;; Write noise data to images

(in-package :noise.image)

(defun noise-color-value (n)
  "Convert noise value N to a number in the range [0, 255]"
  (floor (/ (* (1+ n) 255) 2.0)))

(defun uniform-png (file &key size (color-type :grayscale))
  "Generates a SIZExSIZE PNG filled with random colors of TYPE and saves it to FILE"
  (check-type file pathname)
  (check-type size (integer 1 *))
  (setf *random-state* (make-random-state t))
  (let* ((samples
           (ecase color-type
             (:grayscale 1)
             (:grayscale-alpha 2)
             (:truecolor 3)
             (:truecolor-alpha 4)))
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type color-type
                             :width size
                             :height size)))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (dotimes (i size)
        (dotimes (j size)
          (zpng:write-pixel (loop for n below samples
                                  collect
                                  (noise-color-value (noise.gen:uniform)))
                            png)))
      (zpng:finish-png png))))

(defun perlin-png (file &key size octaves persistence perm scale (color-type :truecolor))
  "Generates a SIZExSIZE PNG filled with perlin noise and saves it to FILE

If COLOR-TYPE specifies a color type with more than one component, a separate
perlin generator will be made for each component and composited together to produce
the resulting pixel"
  (check-type file pathname)
  (check-type size (integer 1 *))
  (setf *random-state* (make-random-state t))
  (let ((png (make-instance 'zpng:pixel-streamed-png
                            :color-type color-type
                            :width size
                            :height size)))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (let* ((components
               (ecase color-type
                 (:grayscale 1)
                 (:grayscale-alpha 2)
                 (:truecolor 3)
                 (:truecolor-alpha 4)))
             (generators
               (loop for n below components
                     collect
                     (noise.gen::perlin-generator :size size
                                                  :octaves octaves
                                                  :persistence persistence
                                                  :perm (or perm
                                                            (noise.gen:uniform-perm 256))
                                                  :scale scale))))

        (dotimes (y size)
          (dotimes (x size)
            (zpng:write-pixel
             (loop for g in generators
                   collect (noise-color-value (funcall g)))
             png)))
        (zpng:finish-png png)))))

(defparameter *current-program* nil
  "Name of the currently running program")

(defparameter *common-options*
  '((:name :help
     :description "Print this help text"
     :short #\h
     :long "help")
    (:name :size
     :description "The side length of the generated image"
     :short #\s
     :long "size"
     :arg-parser #'parse-integer)
    (:name :output
     :description "Name of the output PNG file"
     :short #\o
     :long "output-file"
     :default #P"output.png")
    (:name :color-type
     :description (with-output-to-string (str)
                    (format str "The color type to use~%")
                    (format str " One of [~{`~a'~^, ~}]"
                            '(:grayscale :truecolor)))
     :short #\c
     :long "color-type"
     :default :grayscale
     :arg-parser (lambda (x) (read-from-string (format nil ":~a" x)))))
  "Options common to all executables")

(defparameter *options*
  '(("lperlin-png" . ((:name :octaves
                       :description "Number of octaves to use for perlin noise"
                       :long "octaves"
                       :arg-parser #'parse-integer)
                      (:name :persistence
                       :description "Weight of each octave relative to the last"
                       :long "persistence"
                       :arg-parser #'read-from-string))))
  "alist for options specific to each executable")

(defun usage ()
  (opts:describe
   :prefix (format nil "Usage: ~a OPTIONS" *current-program*)))

(defun check-options (options)
  (cond
    ((getf options :help)
     (usage)
     (opts:exit 0))
    ((not (getf options :size))
     (format *error-output* "No size specified!~%")
     (usage)
     (opts:exit 1))
    ((not (member (getf options :color-type) '(:grayscale :truecolor)))
     (format *error-output* "Unknown color specifier!~%")
     (usage)
     (opts:exit 1))
    ((and (getf options :octaves)
          (not (typep (getf options :octaves) '(integer 1 *))))
     (format *error-output* "Octaves must be an integer >= 1! (~a)~%" (getf options :octaves))
     (usage)
     (opts:exit 1))
    ((and (getf options :persistence)
          (not (typep (getf options :persistence) '(and (float 0 1.0) (satisfies plusp)))))
     (format *error-output* "Persistence must be a float in the range (0 1]. (~a)~%"
             (getf options :persistence))
     (usage)
     (opts:exit 1))))

(defun run (args)
  "Run *CURRENT-PROGRAM* with options ARGS"
  (let* ((additional-opts (assoc *current-program* *options* :test #'string=))
         (opts (append *common-options* (cdr additional-opts))))
    (eval (macroexpand `(opts:define-opts ,@opts)))
    (multiple-value-bind (options) (opts:get-opts args) 
      (check-options options)
      (let ((output      (getf options :output))
            (size        (getf options :size))
            (color-type  (getf options :color-type))
            (octaves     (getf options :octaves))
            (persistence (getf options :persistence)))
        (format t "Generating PNG `~a'~%" output)
        (format t "  Size: ~ax~a~%" size size)
        (format t "  Color Type: ~a~%" color-type)
        (alexandria:eswitch (*current-program* :test #'string=)
                            ("lnoise-png"
                             (format t "  Generator: ~a~%" :uniform)
                             (uniform-png output :size size :color-type color-type))

                            ("lperlin-png"
                             (noise.gen:with-perlin-defaults ((:octaves octaves)
                                                              (:persistence persistence))
                               (format t "  Generator: ~a~%" :perlin)
                               (format t "  Octaves: ~a~%" octaves)
                               (format t "  Persistence: ~a~%" persistence)
                               (perlin-png output
                                           :size size
                                           :octaves octaves
                                           :persistence persistence
                                           :color-type color-type))))))))

(defun toplevel ()
  (let* ((args (opts:argv))
         (proc (and args (file-namestring (first args)))))
    (setf *current-program* proc)
    (cond
      ((or (string= proc "lnoise-png")
           (string= proc "lperlin-png"))
       (setf *current-program* proc)
       (run args))
      (t
       (format *error-output*
               "lnoise multi-executable called with invalid executable `~a'~%"
               proc)))))
