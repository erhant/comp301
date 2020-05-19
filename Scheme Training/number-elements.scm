#lang eopl

; number-elements-from: Listof(SchemeVal) x Int -> Listof(List(Int, SchemeVal))
; usage: (number elements-from `(v_0 v_1 ... v_m) n) = ((n v_0) (n+1 v_1) ... (n+m v_m))
(define number-elements-from
  (lambda (lst n) ; parameters are a List and an Int
    (if (null? lst) '() ; If the list is empty just return empty
        (cons ; Otherwise, construct the (number element) pair
         (list n (car lst)) ; this is the construction
         (number-elements-from (cdr lst) (+ n 1)))))) ; the remaining pairs are appended to this guy

; this is the actual function. we needed the general version as we wrote above.
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)
    )
  )