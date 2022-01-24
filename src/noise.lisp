(in-package :noise.image)

(setf *random-state* (make-random-state t))

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
  '(("lperlin" . ((:name :octaves
                         :description "Number of octaves to use for perlin noise"
                         :long "octaves"
                         :arg-parser #'parse-integer)
                  (:name :persistence
                         :description "Weight of each octave relative to the last"
                         :long "persistence"
                         :arg-parser #'read-from-string))))
  "alist for options specific to each executable")

(defun run (output size generator color-type &optional octaves persistence)
  (format t "Generating PNG `~a'~%" output)
  (format t "  Size: ~ax~a~%" size size)
  (format t "  Generator: ~a~%" generator)
  (format t "  Color Type: ~a~%" color-type)
  (ecase generator
         (:uniform (uniform-png output
                                            :size size
                                            :color-type color-type))
         (:perlin
          (noise.gen:with-perlin-defaults ((:octaves octaves)
                                           (:persistence persistence))
            (format t "  Octaves: ~a~%" octaves)
            (format t "  Persistence: ~a~%" persistence)
            (perlin-png output
                                            :size size
                                            :octaves octaves
                                            :persistence persistence
                                            :color-type color-type)))
  (terpri)))

(defun usage ()
  (opts:describe
   :prefix (format nil "Usage: ~a OPTIONS" *current-program*)))


(defun check-options (options)
  (cond
   ((getf options :help)
    (usage)
    (sb-ext:exit :code 0))
   ((not (getf options :size))
    (format *error-output* "No size specified!~%")
    (usage)
    (sb-ext:exit :code 1))
   ((not (member (getf options :color-type) '(:grayscale :truecolor)))
    (format *error-output* "Unknown color specifier!~%")
    (usage)
    (sb-ext:exit :code 1))
   ((and (getf options :octaves)
         (not (typep (getf options :octaves) '(integer 1 *))))
    (format *error-output* "Octaves must be an integer >= 1! (~a)~%" (getf options :octaves))
    (usage)
    (sb-ext:exit :code 1))
   ((and (getf options :persistence)
         (not (typep (getf options :persistence) '(and (float 0 1) (satisfies plusp)))))
    (format *error-output* "Persistence must be a float in the range (0 1]. (~a)~%"
            (getf options :octaves)))))

(defun toplevel-luniform ()
  (setf *current-program* "luniform")
  (let* ((additional-opts (assoc *current-program* *options*))
         (opts (append *common-options* (cdr additional-opts))))
    (eval (macroexpand `(opts:define-opts ,@opts)))
    (multiple-value-bind (options) (opts:get-opts)
                         (check-options options)
                         (run (getf options :output)
                              (getf options :size)
                              :uniform
                              (getf options :color-type)))))

(defun toplevel-lperlin ()
  (setf *current-program* "lperlin")
  (let* ((additional-opts (assoc *current-program* *options*))
         (opts (append *common-options* (cdr additional-opts))))
    (eval (macroexpand `(opts:define-opts ,@opts)))
    (multiple-value-bind (options) (opts:get-opts)
                         (check-options options)
                         (run (getf options :output)
                              (getf options :size)
                              :perlin
                              (getf options :color-type)
                              (getf options :octaves)
                              (getf options :persistence)))))

(defun toplevel ()
  "Redirect to appropriate procedure based on the passed noise generator"
  (let* ((args (opts:argv))
         (proc (and args (> (length args) 1) (second args))))
    (cond
     ((string= proc "lperlin")
      (toplevel-lperlin))
     ((string= proc "luniform")
      (toplevel-luniform))
     (t (format *error-output*
                "lnoise multi-executable called with invalid executable `~a'~%"
                proc)))))
