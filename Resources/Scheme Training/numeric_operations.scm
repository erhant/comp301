#lang racket

; In addition to +, -, *, / we have some more stuff
(define n 10)
(define m 3)
(quotient n m) ; Quotient from integer division
(remainder n m) ; Remainder from integer division
(modulo n m) ; Least non-negative residue of remainder (?)
(abs -10) ; Absolute value
(min 1 2 3)
(max 1 2 3)
(sqrt 16)
(expt n m) ; Computer n^m, note that 0^0 = 1 in this program
(exp m) ; Computes e^m where e is Euler number
(log n) ; Computer ln(n)
(gcd n m) ; Computer Greatest Common Divisor
(floor 2.34)
(ceiling 2.34)

; Trigonometric
(sin 23) ; Angles in radians
(tan 23) ; Angles in radians
(cos 23) ; Angles in radians
(atan 23) ; Angles in radians

(number? 5)
(complex? 5+2i)
(real? 2.345)
(integer? 5)

(+ 4-2i 6+2i) ; Yay! It can do complex stuff

; GCD(x, y) * LCM(x, y) = x*y
(define lcm
  (lambda (x y)
    (/ (* x y) (gcd x y))
    )
  )
(lcm 5 6)