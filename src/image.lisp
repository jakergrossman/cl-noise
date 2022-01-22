;;;; image.lisp
;;;;
;;;; Write noise data to images

(in-package :noise.image)

(defun noise-color-value (n)
  "Convert noise value N to a number in the range [0, 255]"
  (floor (/ (* (- n -1.0) 255)
            (- 1.0 -1.0))))

(defun uniform-png (file &key size (color-type :grayscale))
  "Creates a SIZExSIZE PNG filled with random colors of TYPE and saves it to FILE"
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

(defun perlin-png (file &key size octaves persistence perm width (color-type :truecolor))
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
                                                                 :width width))))

                              (dotimes (y size)
                                (dotimes (x size)
                                  (zpng:write-pixel
                                   (loop for g in generators
                                         collect (noise-color-value (funcall g)))
                                   png)))
                              (zpng:finish-png png)))))
