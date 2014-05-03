;constructors
(define (make-interval a b) 
  (cons (exact->inexact a) (exact->inexact b)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

;selectors
(define (lower-bound a) (car a))

(define (upper-bound a) (cdr a))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;helper functions
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

(define (mul-interval-naive x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (mul-interval x y)
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

(define (parallel-resistance x y)
  (let ((denom (add-interval (make-interval (/ 1 (lower-bound x))
				    (/ 1 (upper-bound x)))
		     (make-interval (/ 1 (lower-bound y))
				    (/ 1 (upper-bound y))))))
    (make-interval (/ 1 (lower-bound denom))
		   (/ 1 (upper-bound denom)))))

;tests
(define r1 (make-center-percent 6.8 10))
(display "r1: ") (display r1) (display " ") (display (percent r1)) (display "%")
(newline)
(define r2 (make-center-percent 4.7 5))
(display "r2: ") (display r2) (display " ") (display (percent r2)) (display "%")
(newline)
(define r1r2 (mul-interval r1 r2))
(display "r1 * r2: ") (display r1r2) (display " ") (display (percent r1r2)) (display "%")
(newline)