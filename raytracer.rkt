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
                ;; ?? The description of minroot on page 151 says that
                ;; it returns the "smallest real 'x' for which 'ax^2 +
                ;; bx + c = 0'".  Does "smallest" mean "closest to
                ;; negative infinity (in which case the code in the
                ;; book is correct), or "whose absolute value is
                ;; closest to 0" (in which case the code in
                ;; incorrect)?  (No errata reported at
                ;; http://www.paulgraham.com/ancomliser.html)
                (min (/ (+ (- b) discrt) (* 2 a))
                     (/ (- (- b) discrt) (* 2 a)))))))))
