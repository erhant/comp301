#lang eopl

;;; Constructors
;var-exp : Var→Lc-exp
(define var-exp
  (lambda (var)
    var))

;lambda-exp : Var×Lc-exp→Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda (list var) lc-exp)))

;app-exp : Lc-exp×Lc-exp→Lc-exp
(define app-exp
  (lambda (lc-exp lc-exp2)
    (list lc-exp lc-exp2)))

;;; Predicates
;var-exp? : Lc-exp→Bool
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))

;lambda-exp? : Lc-exp→Bool
(define lambda-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp)
         (eqv? (car lc-exp) 'lambda))))

;app-exp? : Lc-exp→Bool
(define app-exp?
  (lambda (lc-exp)
    (and (not (lambda-exp? lc-exp)) (pair? lc-exp))))

;;; Extractors
;var-exp->var : Lc-exp→Var
(define var-exp->var
  (lambda (lc-exp)
    lc-exp))

;lambda-exp->bound-var : Lc-exp→Var
(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cadr lc-exp)))

;lambda-exp->body : Lc-exp→Lc-exp
(define lambda-exp->body
  (lambda (lc-exp)
    (caddr lc-exp)))

;app-exp->rator : Lc-exp→Lc-exp
(define app-exp->rator
  (lambda (lc-exp)
    (car exp)))

;app-exp->rand : Lc-exp→Lc-exp
(define app-exp->rand
  (lambda (lc-exp)
    (cadr exp)))