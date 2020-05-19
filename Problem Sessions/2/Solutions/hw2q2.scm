#lang eopl

;Definition:zero::->bigit

(define zero '(0))

;Definition:bigit-cons:: int -> bigit

(define bigit-cons 
  (lambda (n) 
    (let ((x (remainder n base))) ; base is defined at the bottom
      (if (> base n)
          (list x)
          (cons x (bigit-cons (/ (- n x) base)))))))

;Definition:predecessor:: bigit -> bigit

(define predecessor 
  (lambda (n)
    (if (and (= (length n) 1) (= (car n) 1)) 
        (list 0)
        (predecessor-helper n))))

;Definition:predecessor-helper:: bigit -> bigit

(define predecessor-helper 
  (lambda (n)
    (if (pair? (cdr n))
        (cond ((= (car n) 0)
               (cons (- base 1) (predecessor-helper (cdr n))))
              (else (cons (- (car n) 1) (cdr n))))
        (list (- (car n) 1)))))
        
;Definition:successor:: bigit -> bigit

(define successor 
  (lambda (n)
    (if (= (car n) (- base 1))
        (if (pair? (cdr n))
            (cons 0 (successor (cdr n)))
            '(0 1))
        (cons (+ (car n) 1) (cdr n)))))

;Definition: is-zero?: bigit -> bool

;(define is-zero? 
;  (lambda (n) (and (= (length n) 1) (= (car n) 0))))

(define is-zero? 
  (lambda (var) 
    (if (null? var)
	#t
	(if (= (car var) 0)
	    (is-zero? (cdr var))
	    #f
	    ))))

;Definition: plus:: bigit X bigit -> bigit

(define plus 
  (lambda (a b)
    (if (is-zero? a) 
        b ; return b after doing (a-1*a) + (b+1*a)
        (plus (predecessor a) (successor b))))) ; a + b = (a-1) + (b+1)

;Definition: multi:: bigit X bigit -> bigit
               
(define multi
  (lambda (a b)
    (multi-helper a b '(0))))

;Definition: multi-helper:: bigit X bigit x bigit -> bigit

(define multi-helper 
  (lambda (a b total)
    (if (is-zero? b) total
        (multi-helper a (predecessor b) (plus a total)))))
   
;Definition: factorial:: bigit -> bigit
                    
 (define factorial 
   (lambda (n)
     (if (is-zero? (predecessor n)) (list 1) ; return 1 when we get 1!
         (multi n (factorial (predecessor n)))))) ; n * (n-1)!

(define base 10);

; Test
(display (factorial '(10))) ; note that we defined zero to be '(0)

;When the argument of factorial becomes larger, the execution time becomes longer. Obviously.
; The execution time becomes shorter when the base becomes larger. Perhaps, fewer allocations are needed when the base becomes larger.

