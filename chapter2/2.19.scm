(define (count-change amount)
  (cc amount 5))

(define (count-change-list amount coins)
  (cc-list amount coins))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (cc-list amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc-list amount
		(except-first-denomination-list coin-values))
	    (cc-list (- amount 
		   (first-denomination-list coin-values))
		coin-values)))))

(define (no-more? l)
  (if (null? l)
      #t
      #f))

(define (except-first-denomination-list l)
  (cdr l))

(define (first-denomination-list l)
  (car l))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(define us-coins (list 50 25 10 5 1))
(define us-coins-reverse (list 1 5 10 25 50))
(define us-coins-mixed (list 5 50 25 1 10))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(display (count-change 100))
(newline)

(display (count-change-list 100 us-coins))
(newline)

; Order doesn't matter - this list is reversed
(display (count-change-list 100 us-coins-reverse))
(newline)

; Order doesn't matter - this list is mixed up
(display (count-change-list 100 us-coins-mixed))
(newline)