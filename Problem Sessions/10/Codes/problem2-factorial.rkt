#lang racket

(define (fact x) (fact-c x (create-final-cont)))
(define (fact2 x) (fact-c2 x (create-final-cont)))

;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;
; define create-final-cont here


; define fact-c here
; this is the factorial function with continuation passing
; it will be using create-fact-c x cont in its body


; define create-fact-c here
; it creates the continuation for factorial


; define apply-cont here


; define function fact-c2 here
; this is also a factorial function with continuation passing
; but unlike fact-c, this one does not use create-fact-c function.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some tests
(display (fact 5)) ; should output 120
(display "\n")
(display (fact2 5)) ; should output 120