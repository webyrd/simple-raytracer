;; Simple ray-tracer, adapted from section 9.8
;; of Paul Graham's 'ANSI Common Lisp',
;; 1996, Prentice-Hall Inc.
;;
;; Adapted by Clinton Campbell and Will Byrd

;; Math utilities from Fig 9.2

(define sq (lambda (x) (* x x)))

(define mag
  (lambda (x y z)
    (sqrt (+ (sq x) (sq y) (sq z)))))

(define unit-vector
  (lambda (x y z)
    (let ((d (mag x y z)))
      (values (/ x d) (/ y d) (/ z d)))))

(struct point (x y z))

(define distance
  (lambda (p1 p2)
    (mag (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2))
         (- (point-z p1) (point-z p2)))))

(define minroot
  (lambda (a b c)
    (if (zero? a)
        (/ (- c) b)
        (let ((disc (- (sq b) (* 4 a c))))
          (if (negative? disc)
              #f
              (let ((discrt (sqrt disc)))
                (min (/ (+ (- b) discrt) (* 2 a))
                     (/ (- (- b) discrt) (* 2 a)))))))))


;; Ray-tracing code from figure 9.3 on page 153

;; color is a scalar value (possibly inexact) from 0 to 1, inclusive
(struct surface (color))

;; all the objects in our virtual world
(define *world* '())

(define eye (point 0 0 200))

(define tracer
  (lambda (pathname . args)
    (let ((res (if (null? args)
                   1
                   (first args))))
      (with-output-to-file pathname
        (lambda ()
          (printf "P2\n~s ~s\n~s\n" (* res 100) (* res 100) 255)
          (let ((inc (exact->inexact (/ res))))
            (do ((y -50 (+ y inc)))
                ((< (- 50 y) inc))
              (do ((x -50 (+ x inc)))
                  ((< (- 50 x) inc))
                (displayln (color-at x y))))))
        #:mode 'text
        #:exists 'replace))))

(define color-at
  (lambda (x y)
    (let-values (((xr yr zr) (unit-vector (- x (point-x eye))
                                          (- y (point-y eye))
                                          (- 0 (point-z eye)))))
      (let ((intensity (inexact->exact (round (* (sendray eye xr yr zr) 255)))))
        ;; (printf "color-at ~s ~s: ~s\n" x y intensity)
        intensity))))

(define sendray
  (lambda (pt xr yr zr)
    (let-values (((s intersection-pt) (first-hit pt xr yr zr)))
      (if s
          (* (lambert s intersection-pt xr yr zr) (surface-color s))
          0))))

(define first-hit
  (lambda (pt xr yr zr)
    (let ((surface #f) (hit #f) (dist #f))
      (for-each
        (lambda (s)
          (let ((hit-pt (intersect s pt xr yr zr)))
            (when hit-pt
              (let ((d (distance hit-pt pt)))
                (when (or (not dist) (< d dist))
                  (set! surface s)
                  (set! hit hit-pt)
                  (set! dist d))))))
        *world*)
      (values surface hit))))

(define lambert
  (lambda (s intersection-pt xr yr zr)
    (let-values (((xn yn zn) (normal s intersection-pt)))      
      (let ((intensity (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))
        ;; (printf "lambert ~s ~s ~s: ~s\n" xr yr zr intensity)
        intensity))))

;; Code from figure 9.5 on page 156

(struct sphere surface (radius center))

(define define-sphere
  (lambda (x y z r c)
    (let ((s (sphere c
                     r
                     (point x y z))))
      (set! *world* (cons s *world*))
      s)))

(define intersect
  (lambda (s pt xr yr zr)
    (cond
      ((sphere? s)
       (sphere-intersect s pt xr yr zr))
      (else (error 'intersect "unknown surface type")))))

(define sphere-intersect
  (lambda (s pt xr yr zr)
    (let ((c (sphere-center s)))
      (let ((n (minroot (+ (sq xr) (sq yr) (sq zr))
                        (* 2 (+ (* (- (point-x pt) (point-x c)) xr)
                                (* (- (point-y pt) (point-y c)) yr)
                                (* (- (point-z pt) (point-z c)) zr)))
                        (+ (sq (- (point-x pt) (point-x c)))
                           (sq (- (point-y pt) (point-y c)))
                           (sq (- (point-z pt) (point-z c)))
                           (- (sq (sphere-radius s)))))))
        (if n
            (point (+ (point-x pt) (* n xr))
                   (+ (point-y pt) (* n yr))
                   (+ (point-z pt) (* n zr)))
            #f)))))

(define normal
  (lambda (s pt)
    (cond
      ((sphere? s)
       (sphere-normal s pt))
      (else (error 'normal "unknown surface type")))))

(define sphere-normal
  (lambda (s pt)
    (let ((c (sphere-center s)))
      (unit-vector (- (point-x c) (point-x pt))
                   (- (point-y c) (point-y pt))
                   (- (point-z c) (point-z pt))))))

;; code from figure 9.6 on page 157

(define ray-test
  (lambda args
    (let ((res (if (null? args)
                   1
                   (first args))))
      (set! *world* '())
      (define-sphere 0 -300 -1200 200 .8)
      (define-sphere -80 -150 -1200 200 .7)
      (define-sphere 70 -100 -1200 200 .9)
      (do ((x -2 (add1 x)))
          ((> x 2))
        (do ((z 2 (add1 z)))
            ((> z 7))
          (define-sphere (* x 200) 300 (* z -400) 40 .75)))
      (tracer "spheres.pgm" res))))
