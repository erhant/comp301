#lang eopl

;************* Q4 *************
; Recall lecture 05 - Representation Strategies for Data Types
;Function empty-env: -> list
(define empty-env '())

;Function extend-env: Env x Sym x Val -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env))) ; Association list is here, everything is a key value pair. We add the new key value pair to the start.

;Function empty-env?: Env -> Bool
(define empty-env?
  (lambda (env) (null? env)))

;Function apply-env: Env x Sym -> Val
(define apply-env
  (lambda (env var)
    (cond 
      ((empty-env? env) '())
      ((eqv? var (car (car env))) (cdr (car env))) ; note that everything is stored as ((k1, v1) (k2, v2) ... (keyN, valueN))
      (else (apply-env (cdr env) var))))) ; if we didnt find, recurse again for the rest of env.

; TEST CASES

(define e1 empty-env)
;e1 is empty list now.

(define e2 (extend-env 'ab 3 e1))
;e2 is now ((ab . 3))

(apply-env e2 'ab)
; returns 3

(define ee2 (extend-env 'cd 5 e2)) ; ee2 = extended e2
;ee2 is ((cd . 5) (ab . 3)), so we added the extension t

(apply-env ee2 'ab)
; returns 3

(define eee2 (extend-env 'ab 7 ee2)) ; eee2 = extended ee2
;eee2 is ((ab . 7) (cd . 5) (ab . 3))

(apply-env eee2 'ab)
; returns 7

(apply-env e1 'ab)
; returns ()


;************* Q5 *************
(define has-binding?
  (lambda (env var)
    (if (empty-env? env)
	#f
	(if (eq? (caar env) var)
	    #t
	    (has-binding? (cdr env) var)))))
;Value: has-binding?

; TEST CASES

(define a empty-env)
; a is empty list now.

(define ea (extend-env 'var1 'val1 a))
; ea is ((var1 . val1))

(define eea (extend-env 'var2 'val2 ea))
; eea is((var2 . val2) (var1 . val1))

(has-binding? eea 'val1)
; returns #f

(has-binding? eea 'var2)
; returns #t





