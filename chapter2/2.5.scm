; cons: 2^x * 3^y
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

; car: divide z by powers of two until remainder > 0. 
(define (car z)
  (define (div-pow-2 p)
    (if (< 0 (remainder z (expt 2 (+ p 1))))
	p
	(div-pow-2 (+ p 1))))
  (div-pow-2 0))

; cdr: divide z by powers of three until remainder > 0.
(define (cdr z)
  (define (div-pow-3 p)
    (if (< 0 (remainder z (expt 3 (+ p 1))))
	p
	(div-pow-3 (+ p 1))))
  (div-pow-3 0))

(define test-pair (cons 20 199))
(display test-pair)
(newline)
(display (car test-pair))
(newline)
(display (cdr test-pair))
(newline)