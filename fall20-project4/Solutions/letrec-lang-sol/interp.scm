(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure representation of continuations

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))


        ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;      
        (car-exp (exp1)
                 (value-of/k exp1 env
                             (car-cont cont)))
        
        (cdr-exp (exp1)
                 (value-of/k exp1 env
                             (cdr-cont cont)))
        
        (null?-exp (exp1)
                   (value-of/k exp1 env
                               (null?-cont cont)))
        
        (emptylist-exp ()
                       (apply-cont cont (emptylist-val)))
        
        (list-exp (exps)
                  (if (null? exps)
                             (apply-cont cont (emptylist-val)) ; finished processing exps, continue with the cont
                             (value-of/k (car exps) env ; one by one process exps
                                         (list-exps-cont '() (cdr exps) env cont)))) ; the rest happens at the continuation
                                                        
        
        (map-exp (funcexp listexp)
                 (value-of/k funcexp env ; first obtain the function
                             (map-exp-cont-afterfunc listexp env cont))) ; the rest happens at the continuation 
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))

        ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;
        
        (car-cont (saved-cont)
          (apply-cont saved-cont (expval->car val)))
        
        (cdr-cont (saved-cont)
          (apply-cont saved-cont (expval->cdr val)))
        
        (null?-cont (saved-cont)
          (apply-cont saved-cont (bool-val (expval-null? val))))
        
        (list-exps-cont (vals exps saved-env saved-cont)
          (if (null? exps)
              ; expressions are consumed, now apply continuation
              (apply-cont saved-cont
                          (let loop ((result (pair-val val (emptylist-val))) (vals vals))                                                              
                            (if (null? vals)
                                result
                                (loop (pair-val (car vals) result)
                                      (cdr vals)))))
              ; there are expressions to consume
              (value-of/k (car exps) ; take the first (consume)
                          saved-env
                          (list-exps-cont (cons val vals) ; prepend val
                                          (cdr exps) ; rest of exps
                                          saved-env
                                          saved-cont))))

        ; We get the value of func from value-of/k, which continues here. Here we have it as "val".
        (map-exp-cont-afterfunc (listexp saved-env saved-cont)
                           (value-of/k listexp saved-env
                                       (map-exp-cont-afterlist (expval->proc val) saved-env saved-cont)))

        ;We get the value of the list expression from value-of/km here we have it as "val". func was passed here at map-exp-cont-getfunc 
        (map-exp-cont-afterlist (func saved-env saved-cont)
                           (apply-procedure/k func (expval->car val)
                                              (map-exp-cont-apply func (expval->cdr val) (emptylist-val) saved-env saved-cont)))

        ; Apply procedure iteratively (mapping)
        (map-exp-cont-apply (func vals mappedVals saved-env saved-cont)
                            (if (expval-null? vals)
                                ; Now we have our values except the current val, but even if we add it the order will be in reverse
                                ; so, we loop again the re-reverse it, keeping the original order.
                                (apply-cont saved-cont (let loop ((result (pair-val val (emptylist-val))) (vals mappedVals))                                                                   
                                  (if (expval-null? vals)
                                      result
                                      (loop (pair-val (expval->car vals) result) (expval->cdr vals)))))
                                ; Recursively map the values
                                (apply-procedure/k func (expval->car vals)
                                                   (map-exp-cont-apply func (expval->cdr vals) (pair-val val mappedVals) saved-env saved-cont))))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))
  
  )
  


  
