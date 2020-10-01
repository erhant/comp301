#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.scm. 

(require "data-structures.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;;;;;;;;;;;
(provide init-env empty-env extend-env apply-env extend-env-rec*) ; we added extend-env-rec* here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Page: 86
(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (var val saved-env)
                  (if (eqv? search-sym var)
                      val
                      (apply-env saved-env search-sym)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;;;;;;;;;;;
      (extend-env-rec* (p-names b-vars bodies saved-env) ; now we use extend-env-rec*
                            (apply-extend-env-rec* ; uses our new function
                             env p-names b-vars bodies saved-env search-sym))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;;;;;;;;;;;
(define apply-extend-env-rec*
  (lambda (env p-names b-vars bodies saved-env search-var)
    (if (null? p-names)
        (apply-env saved-env search-var)
        (if (eqv? search-var (car p-names))
            (proc-val (procedure (car b-vars) (car bodies) env))
            (apply-extend-env-rec*
             env
             (cdr p-names) (cdr b-vars) (cdr bodies) saved-env
             search-var)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;