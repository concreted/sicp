(define (square item)
  (* item item))

; Produces reverse list - cons call adds new square result to front of 'answer' list
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))

(display (square-list (list 1 2 3 4)))
(newline)

; Doesn't work either - appends list of old 'answer' to single number produced by (square (car things)), producing a nested list structure
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer 
		    (square (car things))))))
  (iter items '()))

(display (square-list-2 (list 1 2 3 4)))
(newline)