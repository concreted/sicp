; cons: returns a procedure that takes a procedure m as argument, and returns m(x,y)
(define (cons x y)
  (lambda (m) (m x y)))

; car: calls procedure z with a new procedure that takes 2 arguments, and returns the 1st
(define (car z)
  (z (lambda (p q) p)))

; cdr: calls procedure z with a new procedure that takes 2 arguments, and returns the 2nd
(define (cdr z)
  (z (lambda (p q) q)))

(define a (cons 1 5))

(define a_first (car a))
(display a_first) 
(newline)

(define a_second (cdr a))
(display a_second)
(newline)

;(car (cons 1 5))
;(car (lambda (m) (m 1 5)))
;((lambda (m) (m 1 5)) (lambda (p q) p))
;((lambda (p q) p) 1 5)
;5