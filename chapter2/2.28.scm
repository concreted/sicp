(define (fringe tree)
  (cond ((null? tree)
	 '())
	((pair? (car tree))
	 (append (fringe (car tree)) (fringe (cdr tree))))
	(else
	 (append (list (car tree)) (fringe (cdr tree))))))
	   
	

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)

(display (list x x))
(newline)
(display (fringe (list x x)))
(newline)
(display (fringe (list (list x x) (list x x))))
(newline)