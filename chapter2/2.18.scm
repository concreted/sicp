(define (reverse l)
  (define (iterate l newlist)
    (if (null? l)
	newlist
	(iterate (cdr l) (cons (car l) newlist))))
  (iterate l '()))

(display (reverse (list 1 4 9 16 25)))
(newline)
