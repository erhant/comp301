#lang racket

(define x (list 2 5 6))
x

(define y (cons 2 (cons 5 6))) ; Cons creates <pair> types
y ; the dot you see in display shows that this is improper.
; with an improper list, you are gonna have problem with caddr, reverse etc.

(define z (cons 2 (cons 5 (cons 6 '()))))
z ; now this is a proper list. Second element is either a pair or empty list.


(eq? x y); Will say false.
(car x)
(car y)
(cdr x)
(cdr y)
; You can chain car and cdr, such as caddr and stuff. (Up to 4 letters)
; car is the first elements, cdr is the second (rest)

(pair? y)
(pair? x)
(list? y)
(list? x)

(list-ref x 1) ; 0-indexed
(list-ref y 1)

(define xz (append x z))
xz

(reverse xz)

(length xz)
