;;;; text.lisp
;;;;
;;;; Write noise data to text

(in-package :noise.text)

(defun perlin-text (stream &key size octaves persistence perm scale)
  "Output SIZExSIZE data points of perlin noise to STREAM"
  (check-type size (integer 1 *))
  (setf *random-state* (make-random-state t))
  (let ((generator (noise.gen:perlin-generator :size size
                                               :octaves octaves
                                               :persistence persistence
                                               :perm (or perm
                                                         (noise.gen:uniform-perm 256))
                                               :scale scale)))
    (dotimes (y size)
      (dotimes (x size)
        (princ (funcall generator) stream)
        (when (< x (1- size))
          (princ #\Space stream)))
      (princ #\Newline stream))))

(defun uniform-text (stream &key size)
  "Output SIZE data points of uniform noise to STREAM"
  (check-type size (integer 1 *))
  (setf *random-state* (make-random-state t))
  (dotimes (i size)
      (princ (noise.gen:uniform) stream)
    (when (< i (1- size))
        (princ #\Space stream)))
  (princ #\Newline stream))

(defparameter *current-program* nil
  "Name of the currently running program")

(defparameter *common-options*
  '((:name :help
           :description "Print this help text"
           :short #\h
           :long "help")
    (:name :size
           :description "The size of the noise data"
           :short #\s
           :long "size"
           :arg-parser #'parse-integer))
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
   ((and (getf options :octaves)
         (not (typep (getf options :octaves) '(integer 1 *))))
    (format *error-output* "Octaves must be an integer >= 1! (~a)~%" (getf options :octaves))
    (usage)
    (opts:exit 1))
   ((and (getf options :persistence)
         (not (typep (getf options :persistence) '(and (float 0 1) (satisfies plusp)))))
    (format *error-output* "Persistence must be a float in the range (0 1]. (~a)~%"
            (getf options :octaves)))))

(defun run (args)
  (let* ((additional-opts (assoc *current-program* *options* :test #'string=))
         (opts (append *common-options* (cdr additional-opts))))
    (eval (macroexpand `(opts:define-opts ,@opts)))
    (multiple-value-bind (options files) (opts:get-opts args)
      (check-options options)
      (dolist (file (or files '(NIL)))
        (let ((stream (if (not file)
                          *standard-output*
                          (open file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type 'character))))
          (alexandria:eswitch (*current-program* :test #'string=)
            ("lnoise" (uniform-text stream
                                    :size (getf options :size)))
            ("lperlin" (perlin-text stream
                                  :size (getf options :size)
                                  :octaves (getf options :octaves)
                                  :persistence (getf options :persistence))))
          (close stream))))))

(defun toplevel ()
  (let* ((args (opts:argv))
         (proc (and args (file-namestring (first args)))))
    (cond
      ((or (string= "lnoise" proc)
           (string= "lperlin" proc))
       (setf *current-program* proc)
       (run (cdr args)))
      (t
       (format *error-output*
               "lnoise multi-executable called with invalid executable `~a'~%"
               proc)))))
