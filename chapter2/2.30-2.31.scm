(define (square x)
  (* x x))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (map-tree proc tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (proc tree))
	(else (cons (map-tree proc (car tree))
		    (map-tree proc (cdr tree))))))

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map-tree square tree))

(define x (list 1
		(list 2 (list 3 4) 5)
		(list 6 7)))

(display "Direct:") (newline)
(display (square-tree x))
(newline)

(display "Using map:") (newline)
(display (square-tree-map x))
(newline)
	