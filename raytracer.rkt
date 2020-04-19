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
              (error 'minroot
                     (format "disc is negative: ~s" disc))
              (let ((discrt (sqrt disc)))
                (min (/ (+ (- b) discrt) (* 2 a))
                     (/ (- (- b) discrt) (* 2 a)))))))))

