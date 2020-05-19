#lang eopl

(define f ; f(x) = x^2
  (lambda (x)
    (* x x)))
(define args (list 2 3 4))
(define fpair
  (lambda (pair) (+ (car pair) (cdr pair)))) 


; apply
; Basically applies function recursively
(display (apply + args))
(display "\n")
(display (apply - args)) ; we cant apply over f
; for example for - and (2 3 4) it does: (2-3)-4)
(display "\n\n")

; map
; Maps the arguments to their outputs
(display (map f args)) ; applies arguments one by one, returns the result as a list


; There is a famous trick of apply and map used together to make 'Transpose'
(display "\n\n")
(define transpose
  (lambda (matrix)
    (apply map list matrix)))
(display (transpose '((1 2 3) (10 20 30))))
; |1  2  3 |         |1 10|
; |10 20 30| becomes |2 20|
;                    |3 30|
