#lang racket

; This should return (2 + 3) + (5 - 2) = 8
(define exampleLet
  (let
      ((sum (+ 2 3))
       (sub (- 5 2)) ;we cant use 'sum' here for 'let'
       )
    (+ sum sub)
    )
  )

; This should also return 8
(define exampleLetStar
  (let*
      ((sum (+ 2 3))
       (sub (- sum 2)) ;we can use prior definitions with 'let*'
       )
    (+ sum sub)
    )
  )

; This should also return 8
(define exampleLetRec
  (letrec ((local-even? (lambda (n)
                          (if (= n 0) #t
                              (local-odd? (- n 1)))))
           ; notice how even though local-odd is defined afterwards we can use it with 'letrec'
           (local-odd? (lambda (n)
                         (if (= n 0) #f
                             (local-even? (- n 1))))))
    (list (local-even? 23) (local-odd? 23)))
  )

; This has many expressions but should return what?
(define exampleLet2
  (let
      ((sum (+ 2 3))
       (sub (- 5 2))
       )
    (+ sum sub)
    (+ 4 2)
    (+ 9 2)
    "Lol"
    )
  )


(display exampleLet)
(display "\n")
(display exampleLetStar)
(display "\n")
(display exampleLetRec)
(display "\n")
(display exampleLet2)

(display "\n")
(define myvar 5)
(let ((myvar 10)) (display myvar))
(display "\n")
(display myvar)
(set! myvar 15) ; Let didnt change it globally, but set! can.
(display "\n")
(display myvar)