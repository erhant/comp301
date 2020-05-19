#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.scm. 

(require "data-structures.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;;;;;
(provide init-env empty-env extend-env extend-env* apply-env) ; added extend-env* here for you
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;;;;;
;; recursively extend-env, implemented for you :)
(define extend-env*
  (lambda (sym-lst val-lst old-env)
    (cond
      ((or (null? sym-lst) (null? val-lst)) old-env)
      ((and (pair? sym-lst) (pair? val-lst))
       (extend-env* (cdr sym-lst) (cdr val-lst)
                    (extend-env (car sym-lst) (car val-lst) old-env)))
      (else (extend-env sym-lst val-lst old-env)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

