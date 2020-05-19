#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.rkt. 

(require "data-structures.rkt")

(provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 2
(define extend-env*
  (lambda (sym-lst val-lst old-env)
    (cond
      ((or (null? sym-lst) (null? val-lst)) old-env) ; if one of them is null we just return the given environment
      ((and (pair? sym-lst) (pair? val-lst)) ; if both of them are pairs, we take the first element of both and extend the environment,
       (extend-env* (cdr sym-lst) (cdr val-lst) ; and recurse for the remaining symbol and values over this new environment
                    (extend-env (car sym-lst) (car val-lst) old-env)))
      (else (extend-env sym-lst val-lst old-env))))) ; this case is when the symbol and value is just one element, so call extend function as it is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))