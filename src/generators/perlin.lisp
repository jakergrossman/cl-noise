;;;; perlin.lisp
;;;;
;;;; 1, 2, 3, and 4 dimensional perlin noise.

(in-package :cl-noise.gen.perlin)

(declaim (type (vector (mod 256) 256) +ken-perlin-perm+))
(alexandria:define-constant +ken-perlin-perm+
    (make-array 256
                :element-type '(mod 256)
                :initial-contents
                (list 151  160  137  91   90   15   131  13
                      201  95   96   53   194  233  7    225
                      140  36   103  30   69   142  8    99
                      37   240  21   10   23   190  6    148
                      247  120  234  75   0    26   197  62
                      94   252  219  203  117  35   11   32
                      57   177  33   88   237  149  56   87
                      174  20   125  136  171  168  68   175
                      74   165  71   134  139  48   27   166
                      77   14   158  231  83   111  229  122
                      60   211  133  230  220  105  92   41
                      55   46   245  40   244  102  143  54
                      65   25   63   161  1    216  80   73
                      209  76   132  187  208  89   18   169
                      200  196  135  130  116  188  159  86
                      164  100  109  198  173  186  3    64
                      52   217  226  250  124  123  5    202
                      38   147  118  126  255  82   85   212
                      207  206  59   227  47   16   58   17
                      182  189  28   42   223  183  170  213
                      119  248  152  2    44   154  163  70
                      221  153  101  155  167  43   172  9
                      129  22   39   253  19   98   108  110
                      79   113  224  232  178  185  112  104
                      218  246  97   228  251  34   242  193
                      238  210  144  12   191  179  162  241
                      81   51   145  235  249  14   239  107
                      49   192  214  31   181  199  106  157
                      184  84   204  176  115  121  50   45
                      127  4    150  254  138  236  205  93
                      222  114  67   29   24   72   243  141
                      128  195  78   66   215  61   156  180))
  :test #'equalp
  :documentation "Reference perlin permutation")

;; TODO: separately implement 2D, 3D, 4D perlin noise and higher-level generator function
;;;; based on Ken Perlin's "Improved Noise reference implementation"
;;;; https://cs.nyu.edu/~perlin/noise/
(defun perlin3D (z y x &key (perm +ken-perlin-perm+))
  "Calculate perlin noise at point (X, Y, Z)"
  (check-type x real)
  (check-type y real)
  (check-type z real)
  (check-type perm vector)
  (flet (;; access Nth element of PERM, with wrapping
         (p (n) (aref perm (mod n (length perm))))


         ;; linear interpolation
         (lerp (amt a b) (+ a (* amt (- b a))))

         ;; smooth fade
         (fade (n) (* n n n (+ (* n (- (* n 6) 15)) 10)))

         (gradient (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    
    (let* (;; unit cube containing point (x, y, z)
           (unit-x (logand (floor x) 255))
           (unit-y (logand (floor y) 255))
           (unit-z (logand (floor z) 255))

           ;; relative position in unit cube
           (cube-x (mod x 1.0))
           (cube-y (mod y 1.0))
           (cube-z (mod z 1.0))

           ;; faded location for smooth result
           (u (fade cube-x))
           (v (fade cube-y))
           (w (fade cube-z))

           ;; hash coordinates of 8 cube corners
           (a (+ (p unit-x) unit-y))
           (aa (+ (p a) unit-z))
           (ab (+ (p (1+ a)) unit-z))
           (b (+ (p (1+ unit-x)) unit-y))
           (ba (+ (p b) unit-z))
           (bb (+ (p (1+ b)) unit-z)))
      (lerp w
            (lerp v
                  (lerp u
                        (gradient (p aa) cube-x cube-y cube-z)
                        (gradient (p ba) (1- cube-x) cube-y cube-z))
                  (lerp u
                        (gradient (p ab) cube-x (1- cube-y) cube-z)
                        (gradient (p bb) (1- cube-x) (1- cube-y) cube-z)))
            (lerp v
                  (lerp u
                        (gradient (p (1+ aa)) cube-x cube-y (1- cube-z))
                        (gradient (p (1+ ba)) (1- cube-x) cube-y (1- cube-y)))
                  (lerp u
                        (gradient (p (1+ ab)) cube-x (1- cube-y) (1- cube-z))
                        (gradient (p (1+ bb)) (1- cube-x) (1- cube-y) (1- cube-z))))))))

(defun perlin (point &key (perm +ken-perlin-perm+))
  "Generate 1D, 2D, 3D, or 4D perlin noise, based on the size of POINT"
  (check-type point (cons number))
  (case (length point)
    ;; coordinates in row-major order (w z y x)
    (1 (perlin3D 0 0 (first point) :perm perm))
    (2 (perlin3D 0
                 (first point)
                 (second point)
                 :perm perm))
    (3 (perlin3D (first point)
                 (second point)
                 (third point)
                 :perm perm))
    (4 (error "4D Perlin is not implemented yet~%"))
    (otherwise (error "~A is not a valid dimension for PERLIN. Must be in 2, 3, or 4.~%"
                      (length point)))))

