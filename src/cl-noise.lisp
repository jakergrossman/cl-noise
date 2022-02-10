;;;; cl-noise.lisp
;;;;
;;;; cl-noise API functions and macros

(in-package :cl-noise)

(defun noisep (n)
  "Predicate for valid noise values in range [-1.0, 1.0]"
  (<= -1.0 n 1.0))

(deftype noise ()
  `(satisfies noisep))

(defun uniform-perm (n)
  "Generate a random permutation of the numbers in the range [0, N)"
  (let ((perm (make-array n :initial-contents (loop for i below n collect i))))
    ;; swap perm[i] and perm[random(i, N)]
    (loop for i below n
          and swap-i = (+ (cl:random (- n i)) i)

          for a = (aref perm i)
          and b = (aref perm swap-i)

          do (setf (aref perm i) b
                   (aref perm swap-i) a))
    perm))

(deftype dimensions ()
  "A set of integers representing dimension sizes"
  `(cons (integer 1 *)))

(defstruct noise-desc
  "Descriptor struct for noise generation parameters."
  (generator #'cl-noise.gen:uniform :type (or function keyword symbol))
  (generator-keys nil               :type (or null cons))
  (size '(1)                        :type dimensions)
  (octaves 1                        :type (integer 1 *))
  (persistence 0.5                  :type (and real (satisfies plusp)))
  (frequency-scale 2                :type (or real (cons real)))
  (offset nil                       :type (or null (cons real)))
  (sample-width 16.0                :type (and real (satisfies plusp))))

(defun noise (point desc)
  "Calculate a value of noise described the the noise descriptor DESC at POINT"
  (with-accessors ((generator       noise-desc-generator)
                   (generator-keys  noise-desc-generator-keys)
                   (octaves         noise-desc-octaves)
                   (persistence     noise-desc-persistence)
                   (frequency-scale noise-desc-frequency-scale)
                   (offset          noise-desc-offset))
      desc

    ;; apply possible offset
    (when offset
      ;; TODO: errors when:
      ;; - offset is non-nil and length != point dimension count
      (loop for p on point
            for o in offset do
              (incf (car p) o)))

    ;; set effective scalars
    (unless (consp frequency-scale)
      ;; TODO: errors when:
      ;; - frequency scale is a cons and length != point dimension count
      (setf frequency-scale
            (loop for n in point collect frequency-scale)))

    ;; calculate noise
    ;; TODO: handle float overflow
    (loop for o below octaves
          for amplitude = 1 then (* amplitude persistence)
          for max-value = 1 then (+ max-value amplitude)
          for noise     = (apply generator (cons point generator-keys))

          sum (* amplitude noise) into total

          do (loop for p on point
                   for s in frequency-scale do
                     (rplaca p (* (car p) s)))

          finally (return (/ total max-value)))))

(defun indices (row-major-pos size)
  "Calculate indices in row-major order given ROW-MAJOR-POS and SIZE"
  (check-type row-major-pos (integer 0 *))
  (check-type size (cons (integer 1 *)))
  (nreverse
   (loop for val = row-major-pos then (floor val s)
         for s in size
         for d-index = (mod val s)
         collect d-index)))

(defun make-generator (desc)
  "Return a closure that returns all data points in the noise described by DESC
in row-major order"
  (let* ((size         (noise-desc-size desc))
         (sample-width (noise-desc-sample-width desc))
         (step         (/ sample-width (reduce #'max size)))

         (row-major-pos 0)
         (area (reduce #'* size)))
      (lambda ()
        (unless (>= row-major-pos area)
          (prog ((noise-value  (noise (loop for p in (indices row-major-pos size)
                                             collect (* step p))
                                      desc)))
              (incf row-major-pos)
              (return noise-value))))))

(defun make-buffer (desc)
  "Return a buffer of noise described by DESC and generator arguments GEN-ARGS."
  (loop with size = (noise-desc-size desc)
        with area = (reduce #'* size)
        with buf = (make-array size)
        with generator = (make-generator desc)

        for i from 0
        for noise = (funcall generator)
        while noise
        do (setf (row-major-aref buf i) noise)
        finally (return buf)))

(defmacro with-generator ((name &rest desc-args) &body body)
  "Execute the body BODY with the noise generator function
by DESC-ARGS bound to NAME"
  `(let ((,name (make-generator (make-noise-desc ,@desc-args))))
     ,@body))

(defmacro with-buffer ((name &rest desc-args) &body body)
  "Execute the body BODY with a buffer of noise described by DESC-ARGS
bound to NAME"
  `(let ((,name (make-buffer (make-noise-desc ,@desc-args))))
     ,@body))
