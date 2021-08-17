#lang eopl

; occurs-free? : Sym x LcExp -> Bool
; usage : return #t if the symbol var occurs free in exp, otherwise return #f in exp, otherwise returns #f
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp)) ; First variant
      ((eqv? (car exp) 'lambda) ; Second variant (check if first is lambda,
       (and
        (not (eqv? var (car (cadr exp)))) ; Then check that the identifier is not var
        (occurs-free? var (caddr exp)))) ; Then recurse into the expression
      (else
       (or
        (occurs-free? var (car exp)) ; Recurse into left expression
        (occurs-free? var (cadr exp))))))) ; Recurse into right expression

; Recall that
; LcExp ::= Identifier
;         | (lambda (Identifier) LcExp)
;         | (LcExp LcExp)
