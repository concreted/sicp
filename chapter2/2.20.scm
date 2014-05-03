(define (reverse l)
  (define (iterate l newlist)
    (if (null? l)
	newlist
	(iterate (cdr l) (cons (car l) newlist))))
  (iterate l '()))

(define (even? x)
  (if (= 0 (remainder x 2))
      #t
      #f))

(define (odd? x)
  (not (even? x)))

(define (same-parity x . y)
  (define (add-number a l)
    (if (null? l)
	a
	(cond ((or (and (even? (car l)) (even? x)) (and (odd? (car l)) (odd? x))) (add-number (cons (car l) a) (cdr l)))
	      (else (add-number a (cdr l))))))
  (reverse (add-number (list x) y)))

(define (same-parity-better . l)
  (if (even? (car l))
      (filter-list even? l)
      (filter-list odd? l)))

(define (filter-list f l)
  (if (null? (cdr l))
      (cond ((f (car l))
	     (list (car l)))
	    (else
	     '()))
      (cond ((f (car l)) 
	     (cons (car l) (filter-list f (cdr l))))
	    (else 
	     (filter-list f (cdr l))))))

(display (same-parity-better 2 4 5 6))
(newline)

(display (same-parity-better 1 2 3 4 5 6))
(newline)

(define f (lambda (x) (even? x)))

(display (filter-list f (list 2 3 4 5)))
(newline)