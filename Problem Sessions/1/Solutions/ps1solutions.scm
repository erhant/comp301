#lang eopl

(display "1.12 Replace Substring\n")
;Exercise 1.12;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function:Subst symbol x symbol x s-list -> s-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         ((lambda (new old sexp) ; we write the function itself here (inlining) and then call it (see below)
            (if (symbol? sexp)
                (if (eqv? sexp old) new sexp)
                (subst new old sexp))) new old (car slist)) ; inline function is called here
         (subst new old (cdr slist))))))

(display "$ (subst 'x 'y (list 'x 'x 'y 'y))\n")
(display (subst 'x 'y (list 'x 'x 'y 'y)))
(display "\n\n1.21 Cartesian Product\n")

;Function old Subst:
(define substOLD
  (lambda (new old slist)
    (if (null? slist) ; remember the grammar, s-list is lists of s-expr's
        '()
        (cons
         (subst-in-s-exp new old (car slist)) ; process the first expr
         (substOLD new old (cdr slist)))))) ; process the rest of the expr
(define subst-in-s-exp 
  (lambda (new old sexp)
    (if (symbol? sexp) ; remember the grammar, s-expr is either a symbol or s-list
        (if (eqv? sexp old) ; if it is a symbol and it is the one we are looking for too, replace 
            new
            sexp)
        (substOLD new old sexp)))) ; if it is s-list
           

;Exercise 1.21;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function:Product list x list -> list
(define product
  (lambda (sos1 sos2)
    (prod-help sos1 sos2 sos2)))

;Function:Prod-help list x list x list -> list
(define prod-help
  (lambda (sos1 sos2 temp)
    (cond
      ((null? sos1) '()) ; if sos1 is finished we are done.
      ((null? temp) (prod-help (cdr sos1) sos2 sos2)) ; since we start with sos2 as temp, if this is null we start all over with the rest of sos1
      (else (cons (list (car sos1) (car temp)) ; first we use sos1 using car
                  (prod-help sos1 sos2 (cdr temp))))))) ; and then on the same sos1 we continue adding the rest of tmp
; So basically how it works is, fırst we exhaust sos2 on fırst element of sos1, then we repeat the same with rest of sos1 and so on...

(display "$ (product (list 'a 'b 'c) (list 'x 'y))\n")
(display (product (list 'a 'b 'c) (list 'x 'y)))
(display "\n\n1.26: Up Down\n")
;Exercise 1.26;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function:Up list -> list
(define up 
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((pair? (car lst)) (append (car lst) (up (cdr lst))))
      (else (cons (car lst) (up (cdr lst)))))))

;Function:Down list -> list
(define down
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons (list (car lst)) (down (cdr lst)))))))

(display "$ (up (list 1 '(2) 3))\n")
(display (up (list 1 '(2) 3)))
(display "\n")
(display "$ (down (list 1 2 3))\n")
(display (down (list 1 2 3)))
(display "\n\n1.34 Binary Search Tree\n")
;Exercise 1.34;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function:Path Sybmol x binary-tree(list) -> list
(define path
  (lambda (n bin-tree)
    (path-help n bin-tree '())))

;Function:Path Sybmol x binary-tree(list) x list -> list
(define path-help
  (lambda (n bin-tree waylst) ; we store our path in the waylst, initially '()
    (cond
      ((null? bin-tree) '()); Returns empty list if tree is null
      ((eqv? n (car bin-tree)) waylst) ; Returns waylst if we found our integer
      (else (append (path-help n (cadr bin-tree) (append waylst '(left))) ;else we recurse to left branch 
                    (path-help n (caddr bin-tree) (append waylst '(right)))))))) ; and right branch, note that appending empty list to some list results in that some list only!

(display (path 58 (list 4
         (list 2 '() '())
         (list 6
           '()
           (list 5 '() '())
           ))))
(display "\n\n1.36 Number Elements\n")
;Exercise 1.36;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function:G list x list -> list
(define g
  (lambda (lst1 lst2)
        (cons lst1 (map (lambda (x) (list (+ 1 (car x)) (cadr x))) lst2))))
        
;Function:Number-elements list -> list
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(display "$ (number-elements (list 'a 'b 'c))\n")
(display (number-elements (list 'a 'b 'c)))