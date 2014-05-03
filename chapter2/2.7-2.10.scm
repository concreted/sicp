;constructor
(define (make-interval a b) (cons a b))

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
  (cond ((and (negative? (lower-bound x)) (negative? (lower-bound y)) (negative? (upper-bound x)) (negative? (upper-bound y)))
	 (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))))

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
(define resistor1 (make-interval 6.12 7.48))     ;6.8-ohm 10%
(define resistor2 (make-interval 4.465 4.935))   ;4.7-ohm 5%
(display "r1 = ")
(display resistor1)
(newline)
(display "r1 width = ")
(display (width-interval resistor1))
(newline)
(display "r2 = ")
(display resistor2)
(newline)
(display "r2 width = ")
(display (width-interval resistor2))
(newline)
(display "Parallel resistance of r1 and r2: ")
(display (parallel-resistance resistor1 resistor2))
(newline)(newline)
(display "r1+r2 = ")
(display (add-interval resistor1 resistor2))
(newline)
(display "r1-r2 = ")
(display (sub-interval resistor1 resistor2))
(newline)
(display "r1*r2 = ")
(display (mul-interval resistor1 resistor2))
(newline)
(display "r1/r2 = ")
(display (div-interval resistor1 resistor2))
(newline)(newline)

(display (div-interval resistor1 (make-interval -1 1)))
(newline)
(display (div-interval resistor1 (make-interval -1 0)))
(newline)
(display (div-interval resistor1 (make-interval 0 1)))
(newline)