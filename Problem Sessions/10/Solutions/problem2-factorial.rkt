#lang racket

(define (fact x) (fact-c x (create-final-cont)))
(define (fact2 x) (fact-c2 x (create-final-cont)))

;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;
; define create-final-cont here
(define (create-final-cont) (lambda (x) x))

; define fact-c here
; this is the factorial function with continuation passing
; it will be using create-fact-c x cont in its body
(define (fact-c x cont)
  (if (= x 0)
      (apply-cont cont 1)
      (fact-c (- x 1) (create-fact-c x cont) )))

; define create-fact-c here
; it creates the continuation for factorial
; this function will be used by fact-c
(define (create-fact-c x cont)
  (lambda (n) (apply-cont cont (* x n))))

; define apply-cont here, it will be used by create-fact-c
(define (apply-cont c x)
  (c x))

; define function fact-c2 here
; this is also a factorial function with continuation passing
; but unlike fact-c, this one does not use create-fact-c function.
(define (fact-c2 x cont)
  (if (= x 0)
      (cont 1)
      (fact-c2 (- x 1) (lambda (n) (cont (* x n))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some tests
(display (fact 5)) ; should output 120
(display "\n")
(display (fact2 5)) ; should output 120