(define (reverse l)
  (define (build-rev-iter new original)
    (if (null? original)
	new
	(build-rev-iter (cons (car original) new) (cdr original))))
  (build-rev-iter '() l))

(define (deep-reverse l)
  (define (build-rev-iter new original)
    (cond ((null? original)
	   new)
	  ((pair? (car original))
	   (build-rev-iter (cons (deep-reverse (car original)) new) (cdr original)))
	  (else
	   (build-rev-iter (cons (car original) new) (cdr original)))))
  (build-rev-iter '() l))


(display (deep-reverse (list 1 2 3 4 5 6)))
(newline)(newline)

(define x (list (list 1 2) (list 3 4)))

(display "x")
(newline)
(display x)
(newline)(newline)

(display "(reverse x)")
(newline)
(display (reverse x))
(newline)(newline)

(display "(deep-reverse x)")
(newline)
(display (deep-reverse x))
(newline)(newline)