#lang eopl
; Env = Var -> SchemeVal (so variable to value)
; Here we treat environments as procedures!

; () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

; Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

; Env x Var -> SchemeVal
; Thanks to our procedural approach apply-env just becomes calling the function with that variable.
(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define report-no-binding-found
  (lambda (binding) (begin (display "No binding found:\t") (display binding))))

(define report-invalid-env
  (lambda (env) ((display "Invalid environment") (display env))))

; Test
(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
(display (apply-env e 'y))
(display "\n")
(display (apply-env e 't))
; e(d) = 6, e(x) = 7, e(y) = 8 (because we extended bottom up, and y=8 after y=14.
(display "\n\nEnvironment:\n")
(display e)