#lang eopl

(define empty-env
  (lambda () list 'empty-env))

(define extend-env
  (lambda (var val env) ; env is the environment we are extending
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env))) ; using this many car and cdr is bad practice, write an 'extractor' for these!
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (binding) ((display "No binding found") (display binding))))

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
; e(d) = 6, e(x) = 7, e(y) = 8 (because we extended bottom up, and y=8 after y=14.
(display "\n\nEnvironment:\n")
(display e)