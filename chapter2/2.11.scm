;constructor
(define (make-interval a b) (cons (exact->inexact a) (exact->inexact b)))

;selectors
(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

;helper functions
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (mul-interval-better x y)
  (cond	((and (negative? (lower-bound x)) (positive? (upper-bound x)) (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
			(max (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))))
	((and (negative? (lower-bound x)) (negative? (lower-bound y)) (negative? (upper-bound x)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (positive? (lower-bound x)) (positive? (lower-bound y)) (positive? (upper-bound x)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (negative? (lower-bound x)) (negative? (upper-bound x)) (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (negative? (lower-bound x)) (positive? (upper-bound x)) (negative? (lower-bound y)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
	((and (negative? (lower-bound x)) (negative? (upper-bound x)) (positive? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
	((and (positive? (lower-bound x)) (positive? (upper-bound x)) (negative? (lower-bound y)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	((and (negative? (lower-bound x)) (positive? (upper-bound x)) (positive? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
	((and (positive? (lower-bound x)) (positive? (upper-bound x)) (negative? (lower-bound y)) (positive? (upper-bound y)))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))

(define (div-interval x y)
  (cond ((or (= 0 (lower-bound y)) (= 0 (upper-bound y))
	     (and (< (lower-bound y) 0) (> (upper-bound y) 0)))
	 (display "divisor spans zero"))
	 ;(error "divisor spans zero"))
	(else
	 (mul-interval x 
		       (make-interval (/ 1.0 (upper-bound y))
				      (/ 1.0 (lower-bound y)))))))

(define (width-interval a)
  (/ (- (upper-bound a) (lower-bound a)) 2))

(define (parallel-resistance x y)
  (let ((denom (add-interval (make-interval (/ 1 (lower-bound x))
				    (/ 1 (upper-bound x)))
		     (make-interval (/ 1 (lower-bound y))
				    (/ 1 (upper-bound y))))))
    (make-interval (/ 1 (lower-bound denom))
		   (/ 1 (upper-bound denom)))))

;tests
(define (display-sign x)
  (if (positive? x)
      (display "+")
      (display "-")))

(define (test-mul-interval x y)
  (let ((basic (mul-interval x y))
	(better (mul-interval-better x y)))
    (display "[") 
    (display "(") (display-sign (lower-bound x)) (display ",") (display-sign (upper-bound x)) (display ")")
    (display ", ")
    (display "(") (display-sign (lower-bound y)) (display ",") (display-sign (upper-bound y)) (display ")")
    (display "]: ")
    (if (equal? basic better)
	(display "Pass! ")
	(display "Fail! "))
    (display x) (display " * ") (display y) (display " = ")
    (display basic) (display ", ") (display better) 
    (newline)))

(display "Normal cases: 1 test") (newline)
(test-mul-interval (make-interval -2 -1) (make-interval -3 -2))
(test-mul-interval (make-interval -40 -1) (make-interval -30 5))
(test-mul-interval (make-interval -3 2) (make-interval -5 -4))
(test-mul-interval (make-interval -3 -2) (make-interval 10 30))
(test-mul-interval (make-interval 3.44 10.456) (make-interval -100 -99))
(test-mul-interval (make-interval -9 2.86) (make-interval 0.001 59))
(test-mul-interval (make-interval 3.22 3.23) (make-interval -4 5))
(test-mul-interval (make-interval 6.12 7.48) (make-interval 4.465 4.935))
(display "Special case: 2 tests [(-,+), (-,+)]") (newline)
(test-mul-interval (make-interval -3 1) (make-interval -4 1))
(test-mul-interval (make-interval -3 10) (make-interval -4 120))


