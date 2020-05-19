#lang eopl

; Contents of lang.rkt

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
    
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    
    (expression
     ("set" identifier "=" expression)
     assign-exp)
    
    (expression
     ("newpair" "(" expression "," expression ")")
     newpair-exp)
    
    (expression
     ("left" "(" expression ")")
     left-exp)
    
    (expression
     ("setleft" expression "=" expression)
     setleft-exp)
    
    (expression
     ("right" "(" expression ")")
     right-exp)
    
    (expression
     ("setright" expression "=" expression)
     setright-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

; Contents of data-structures.rkt (has pairval2.rkt in it too)

;;;;;;;;;;;;;;;; mutable pairs ;;;;;;;;;;;;;;;;

;; model a mutable pair as two consecutive locations (left and
;; right), and represent it as a reference to the first.

;; mutpair? : SchemeVal -> Bool
;; Page: 129
;;
;; Not every reference is really a mutpair, but this test is good
;; enough, because in the implicit-refs language, you
;; can't get your hands on a reference otherwise.
(define mutpair?
  (lambda (v)
    (reference? v)))

;; make-pair : ExpVal * ExpVal -> MutPair
;; Page: 129
(define make-pair
  (lambda (val1 val2)
    (let ((ref1 (newref val1)))
      (let ((ref2 (newref val2)))
        ref1))))

;; left : MutPair -> ExpVal
;; Page: 129
(define left                       
  (lambda (p)
    (deref p)))

;; right : MutPair -> ExpVal
;; Page: 129  
(define right
  (lambda (p)
    (deref (+ 1 p))))

;; setleft : MutPair * ExpVal -> Unspecified
;; Page: 129  
(define setleft
  (lambda (p val)
    (setref! p val)))

;; setright : MutPair * Expval -> Unspecified
;; Page: 129  
(define setright
  (lambda (p val)
    (setref! (+ 1 p) val)))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, a
;;; reference, or a mutable pair. 

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (ref-val
   (ref reference?))
  (mutpair-val
   (p mutpair?))
  )

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'reference v)))))

(define expval->mutpair
  (lambda (v)
    (cases expval v
      (mutpair-val (ref) ref)
      (else (expval-extractor-error 'mutable-pair v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment data structures ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval reference?)                 
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))

;; env->list : Env -> List
;; used for pretty-printing and debugging
(define env->list
  (lambda (env)
    (cases environment env
      (empty-env () '())
      (extend-env (sym val saved-env)
                  (cons
                   (list sym val)              ; val is a denoted value-- a
                   ; reference. 
                   (env->list saved-env)))
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (cons
                        (list 'letrec p-names '...)
                        (env->list saved-env))))))

;; expval->printable : ExpVal -> List
;; returns a value like its argument, except procedures get cleaned
;; up with env->list 
(define expval->printable
  (lambda (val)
    (cases expval val
      (proc-val (p)
                (cases proc p
                  (procedure (var body saved-env)
                             (list 'procedure var '... (env->list saved-env)))))
      (else val))))

; Contents of environments.rkt 

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; (init-env) builds an environment in which:
;; i is bound to a location containing the expressed value 1, 
;; v is bound to a location containing the expressed value 5, and 
;; x is bound to a location containing the expressed value 10.  
(define init-env 
  (lambda ()
    (extend-env 
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (cond 
                         ((location search-sym p-names)
                          => (lambda (n)
                               (newref
                                (proc-val
                                 (procedure 
                                  (list-ref b-vars n)
                                  (list-ref p-bodies n)
                                  env)))))
                         (else (apply-env saved-env search-sym)))))))


;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))

; Contents of store.rkt

(define instrument-newref (make-parameter #f))

;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.
;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

;; empty-store : () -> Sto
;; Page: 111
(define empty-store
  (lambda () '()))

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
;; Page 111
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; get-store : () -> Sto
;; Page: 111
;; This is obsolete.  Replaced by get-store-as-list below
(define get-store
  (lambda () the-store))

;; reference? : SchemeVal -> Bool
;; Page: 111
(define reference?
  (lambda (v)
    (integer? v)))

;; newref : ExpVal -> Ref
;; Page: 111
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store
            (append the-store (list val)))                   
      next-ref)))                     

;; deref : Ref -> ExpVal
;; Page 111
(define deref 
  (lambda (ref)
    (list-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; Page: 112
(define setref!                       
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                ;; returns a list like store1, except that position ref1
                ;; contains val. 
                (lambda (store1 ref1)
                  (cond
                    ((null? store1)
                     (report-invalid-reference ref the-store))
                    ((zero? ref1)
                     (cons val (cdr store1)))
                    (else
                     (cons
                      (car store1)
                      (setref-inner
                       (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; get-store-as-list : () -> Listof(List(Ref,Expval))
;; Exports the current state of the store as a scheme list.
;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
;;   where foo, bar, and baz are expvals.
;; If the store were represented in a different way, this would be
;; replaced by something cleverer.
;; Replaces get-store (p. 111)
(define get-store-as-list
  (lambda ()
    (letrec
        ((inner-loop
          ;; convert sto to list as if its car was location n
          (lambda (sto n)
            (if (null? sto)
                '()
                (cons
                 (list n (car sto))
                 (inner-loop (cdr sto) (+ n 1)))))))
      (inner-loop the-store 0))))

; Contents of interp.rkt

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (initialize-store!)             
    (cases program pgm
      (a-program (body)
                 (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 132
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (deref (apply-env env var)))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (var exp1 body)       
               (let ((val (value-of exp1 env)))
                 (let ((new-env (extend-env var (newref val) env)))
                   (value-of body new-env))))
      
      (proc-exp (var body)
                (proc-val
                 (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of-operand rand env)))
                  (apply-procedure proc arg)))
      
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (value-of letrec-body
                            (extend-env-rec* p-names b-vars p-bodies env)))
      
      (begin-exp (exp1 exps)
                 (letrec 
                     ((value-of-begins
                       (lambda (e1 es)
                         (let ((v1 (value-of e1 env)))
                           (if (null? es)
                               v1
                               (value-of-begins (car es) (cdr es)))))))
                   (value-of-begins exp1 exps)))
      
      (assign-exp (x e)
                  (begin
                    (setref!
                     (apply-env env x)
                     (value-of e env))
                    (num-val 27)))
      
      (newpair-exp (exp1 exp2)
                   (let ((v1 (value-of exp1 env))
                         (v2 (value-of exp2 env)))
                     (mutpair-val (make-pair v1 v2))))
      
      (left-exp (exp1)
                (let ((v1 (value-of exp1 env)))
                  (let ((p1 (expval->mutpair v1)))
                    (left p1))))
      
      (setleft-exp (exp1 exp2)
                   (let ((v1 (value-of exp1 env))
                         (v2 (value-of exp2 env)))
                     (let ((p (expval->mutpair v1)))
                       (begin
                         (setleft p v2)
                         (num-val 82)))))
      
      (right-exp (exp1)
                 (let ((v1 (value-of exp1 env)))
                   (let ((p1 (expval->mutpair v1)))
                     (right p1))))
      
      (setright-exp (exp1 exp2)
                    (let ((v1 (value-of exp1 env))
                          (v2 (value-of exp2 env)))
                      (let ((p (expval->mutpair v1)))
                        (begin
                          (setright p v2)
                          (num-val 83)))))
      
      )))

;; apply-procedure : Proc * Ref -> ExpVal
;;;;;;;;;;;;;;; PROBLEM 3 ;;;;;;;;;;;;;;;;
; you will change this function a bit
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (let ((new-env (extend-env var val saved-env)))
                   (value-of body new-env))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; value-of-rand : Exp * Env -> Ref
;; Page: 132
;; if the expression is a var-exp, then pass the reference.
;; otherwise, evaluate the expression and pass a reference to a new
;; cell. 

(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var)) 
      (else
       (newref (value-of exp env))))))

;; store->readable : Listof(List(Ref,Expval)) 
;;                    -> Listof(List(Ref,Something-Readable))
(define store->readable
  (lambda (l)
    (map
     (lambda (p)
       (list
        (car p)
        (expval->printable (cadr p))))
     l)))

;; Interface for the new language.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;;;;;;;;;;;;;; PROBLEM 3 ;;;;;;;;;;;;;;;;
;; Test for this problem is given below.
(display (expval->num (run "
let f = proc (x)
    begin
        set x = 3;
        4
    end
in let x = 5
    in begin
        (f x);
        x
    end
")))
; This program outputs 4 using call-by-value-result,
; and outputs 3 using call-by-reference.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
