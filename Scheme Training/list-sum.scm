#lang racket

; list-sum: Listof(Int) -> Int
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi) (list-sum (cdr loi)))))); car + cdr (where cdr gets recursed)

; what about vector sum, we cant take cdr of those!

