#lang racket

; Some equality testings
;   (eq? x y)       ; Returns #t if x and y are same object, #f otherwise. 
;   (equal? x y)    ; Returns #t if x and y "print the same", #f otherwise.
;   (= n1 n2 ...)   ; Returns #t if n1 = n2 = ..., they should all be numbers.
;   (zero? n)       ; Returns #t if n = 0.

; A basic if else branch
(if (> 5 2) ; Condition
    (display "True.") ; True branch
    (display "False.") ; False branch
    )
(display "\n")

; cond returns the first true expression
(cond
  ((= 2 2) (display "First"))
  ((= 2 3) (display "Second"))
  ((= 2 2) (display "Third 1") (display "Third 2"))
  (else (display "Else branch"))
  )
(display "\n")

; if nothing is true, it can return else branch but this is optional
(cond
  ((= 12 2) (display "First"))
  ((= 12 3) (display "Second"))
  ((= 12 2) (display "Third 1") (display "Third 2"))
  (else (display "Else branch"))
  )
(display "\n")

(define andTest 
  (and (= 2 2) (= 4 4) (= 2 4))
)

(define orTest
  (or (= 2 2) (= 4 4) (= 2 4))
)

(display andTest)
(display "\n")
(display orTest)
(display "\n")
(display (and)) ; Returns true if there are no expressions
(display "\n")
(display (or)) ; Returns false if there are no expressions