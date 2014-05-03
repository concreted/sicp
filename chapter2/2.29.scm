(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list structure length))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car (cdr branch)))

(define (branch-structure branch)
  (car branch))

(define (show-branch branch)
  (display "branch structure:") (display (branch-structure branch))
  (newline)
  (display "       length:") (display (branch-length branch)))

(define (show-mobile mobile)
  (display "mobile left:") (newline)
  (show-branch (left-branch mobile)) (newline)
  (display "mobile right:") (newline)
  (show-branch (right-branch mobile)) (newline))

(define (branch-weight branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (+ (branch-weight (left-branch (branch-structure branch))) 
	 (branch-weight (right-branch (branch-structure branch))))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (balanced mobile)
  (if (= (* (branch-weight (left-branch mobile)) (branch-length (left-branch mobile)))
	 (* (branch-weight (right-branch mobile)) (branch-length (right-branch mobile))))
      #t
      #f))

(define b2 (make-branch 5 2))
(define b1 (make-branch 5 1))

(define m1 (make-mobile b1 b2))
(define msimple (make-mobile b1 b1))
(define b-m1 (make-branch 5 m1))

(define m0 (make-mobile b-m1 b1))
(define mbig (make-mobile (make-branch 5 m0) (make-branch 5 m0)))

;(display m1)
;(newline)
;(display b0)
;(newline)

; a.
(display m0)
(newline)
(display (left-branch m0))
(newline)
(display (right-branch m0))
(newline)

(display "b-m1:") (newline) (show-branch b-m1)
(newline)

(show-mobile mbig)
(newline)

; b.
(display mbig) (newline)

(display (total-weight mbig))
(newline)

; c.
(display (balanced mbig))
(newline)

(display (balanced msimple))
(newline)

(display (balanced m0))
(newline)

(display (balanced m1))
(newline)