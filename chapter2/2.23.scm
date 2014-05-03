(define (for-each f l)
  (f (car l))
  (if (null? (cdr l))
      #t
      (for-each f (cdr l))))

(define (for-each-2 f l)
  (f (car l))
  (if (not (null? (cdr l)))
      (for-each-2 f (cdr l))))

(for-each-2 (lambda (x) (newline) (display x))
	  (list 57 321 88))
(newline)