#lang eopl

;Inlining example
(define f
  (lambda (x)
    (+ 2 (
          (lambda (y) (+ y y))
          x)))) ; This should do 2 + (x + x)

(display (f 2))