;;;; simplex.lisp
;;;;
;;;; 1, 2, 3, and 4 dimensional simplex noise

(in-package :cl-noise.gen.simplex)

(alexandria:define-constant +grad-3d+
    (make-array 12 :initial-contents
  '((1 1 0) (-1 1 0) (1 -1 0) (-1 -1 0)
    (1 0 1) (-1 0 1) (1 0 -1) (-1 0 -1)
    (0 1 1) (0 -1 1) (0 1 -1) (0 -1 1)))
  :test #'equalp)

(defun dot (a b)
  (loop for a-elt in a
        for b-elt in b
        sum (* a-elt b-elt)))

;; TODO: implement 3D and 4D simplex

(defun simplex2D (y x &key (perm +ken-perlin-perm+))
  "Calculate a single point of 2D simplex noise"
  (labels ((p (n) (aref perm (logand n 255)))
           (p-mod-12 (n) (mod (p n) 12)))
    (let* (;; stretching factors
           (F (* 0.5 (- (sqrt 3) 1)))
           (G (/ (- 3 (sqrt 3)) 6))

           ;; skew input space to find containing simplex cell
           (s (* F (+ x y)))
           (i (floor (+ x s)))
           (j (floor (+ y s)))
           (d (* G (+ i j)))

           ;; unskew cell origin back to x,y space
           (x-origin (- i d))
           (y-origin (- j d))

           ;; x,y distance from cell origin
           (x0 (- x x-origin))
           (y0 (- y y-origin))

           ;; determine containing simplex
           (rel (> x0 y0))

           ;; offset for second (middle) corner of simplex in i,j coords
           (i1 (if rel 1 0))
           (j1 (if rel 0 1))

           ;; offsets for middle corner of simplex in x,y unskewed coords
           (x1 (+ x0 (- i1) G))
           (y1 (+ y0 (- j1) G))

           ;; offsets for last corner in x,y unskewed coords
           (x2 (+ x0 -1 (* 2 G)))
           (y2 (+ y0 -1 (* 2 G)))

           ;; hashed gradient indices of three simplex corners
           (unit-i (logand i 255))
           (unit-j (logand j 255))

           (gi0 (p-mod-12 (+ unit-i (p unit-j))))
           (gi1 (p-mod-12 (+ unit-i i1 (p (+ unit-j j1)))))
           (gi2 (p-mod-12 (+ unit-i 1 (p (1+ unit-j)))))

           ;; contribution from three corners
           (t0 (- 0.5 (* x0 x0) (* y0 y0)))
           (t1 (- 0.5 (* x1 x1) (* y1 y1)))
           (t2 (- 0.5 (* x2 x2) (* y2 y2)))

           (n0 (if (minusp t0) 0 (* t0 t0 t0 t0 (dot (aref +grad-3d+ gi0)
                                                     (list x0 y0)))))
           (n1 (if (minusp t1) 0 (* t1 t1 t1 t1 (dot (aref +grad-3d+ gi1)
                                                     (list x1 y1)))))
           (n2 (if (minusp t2) 0 (* t2 t2 t2 t2 (dot (aref +grad-3d+ gi2)
                                                     (list x2 y2))))))
      ;; sum of contributions is final noise value
      ;; result is scaled to return values in the interval [-1, 1]
      (* 70 (+ n0 n1 n2)))))

(defun simplex (point &key (perm +ken-perlin-perm+))
  "Calculate a value of 1, 2, 3, or 4 dimensional simplex noise at POINT.

PERM-a uniform permutation of the numbers [0,256)"
  (check-type point (cons number))
  (case (length point)
    ;; dimensions in row-major order (w z y x)
    (1 (simplex2D 0 (first point) :perm perm))
    (2 (simplex2D (first point)
                  (second point)
                  :perm perm))
    (3 (error "3D simplex noise is not implemented yet"))
    (4 (error "4D simplex noise is not implemented yet"))
    (otherwise (error "~A is not a valid dimension for SIMPLEX. Must be in 2, 3, or 4.~%"
                      (length point)))))

