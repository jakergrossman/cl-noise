(in-package :noise)

(setf *random-state* (make-random-state t))

(opts:define-opts
 (:name :help
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
        :default #P"output.png"
        :arg-parser #'pathname)
 (:name :generator
        :description (with-output-to-string (str)
                       (format str "Which generator to use to generate noise~%")
                       (format str " One of [~{`~a'~^, ~}]" '(:uniform :perlin)))
        :short #\g
        :long "generator"
        :default :uniform
        :arg-parser (lambda (x) (read-from-string (format nil ":~a" x))))
 (:name :color-type
        :description (with-output-to-string (str)
                       (format str "The color type to use~%")
                       (format str " One of [~{`~a'~^, ~}]" '(:grayscale :truecolor)))
        :short #\c
        :long "color-type"
        :default :grayscale
        :arg-parser (lambda (x) (read-from-string (format nil ":~a" x))))
 (:name :octaves
        :description "Number of octaves to use for perlin noise"
        :long "octaves"
        :arg-parser #'parse-integer)
 (:name :persistence
        :description "Weight of each perlin octave relative to the last"
        :long "persistence"
        :arg-parser #'read-from-string))

(defun run (output size generator color-type &optional octaves persistence)
  (format t "Generating PNG `~a'~%" output)
  (format t "  Size: ~ax~a~%" size size)
  (format t "  Generator: ~a~%" generator)
  (format t "  Color Type: ~a~%" color-type)
  (ecase generator
         (:uniform (noise.image:uniform-png output
                                            :size size
                                            :color-type color-type))
         (:perlin
          (noise.gen:with-perlin-defaults ((:octaves octaves)
                                           (:persistence persistence))
            (format t "  Octaves: ~a~%" octaves)
            (format t "  Persistence: ~a~%" persistence)
            (noise.image:perlin-png output
                                            :size size
                                            :octaves octaves
                                            :persistence persistence
                                            :color-type color-type)))
  (terpri)))

(defun usage ()
  (opts:describe
   :prefix (with-output-to-string (str)
                                  (format str "Usage: noise [-h] [--size SIZE] [--output-file OUTPUT]~%") 
                                  (format str "             [--generator GENERATOR] [--color-type COLOR-TYPE]~%")
                                  (format str "             [--octaves OCTAVES] [--persistence PERSISTENCE]~%"))
   :args "[keywords]"))

(defun check-options (options)
  (cond
   ((getf options :help)
    (usage)
    (sb-ext:exit :code 0))
   ((not (getf options :size))
    (print "No size specified!" *error-output*)
    (usage)
    (sb-ext:exit :code 1))
   ((not (member (getf options :generator) '(:uniform :perlin)))
    (print "Unknown generator specified!" *error-output*)
    (usage)
    (sb-ext:exit :code 1))
   ((not (member (getf options :color-type) '(:grayscale :truecolor)))
    (print "Unknown color specifier!" *error-output*)
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


(defun toplevel ()
  (multiple-value-bind (options) (opts:get-opts)
    (check-options options)
    (run (getf options :output)
         (getf options :size)
         (getf options :generator)
         (getf options :color-type)
         (getf options :octaves)
         (getf options :persistence))))

