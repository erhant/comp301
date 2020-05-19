#lang eopl

;; grammar for the PROC language

(provide (all-defined-out))

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
    
	;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;
	; modify so that it can take arguments separated by commas
	; recall that arbno gave us the ability to do (x y z ...)
	; but now we want seperations by comma, such as (x, y, z, ...)
	; arbno wont work here, use 'seperated-list' instead!
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    ;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;
	; modify so that it can take arbitrary number of expressions while calling a function
    (expression
     ("(" expression expression ")")
     call-exp)
    ;;;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;
	
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

