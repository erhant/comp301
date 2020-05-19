; (vector expr1 expr2 ...)
(define myVec (vector 1 2 3 4))

; Initialize a vector
(make-vector 4 (- 1 1)) ; Initializez a 4 element vector wth the last expression WTFWTFWTFWTTWFTFW

; Access an index
(vector-ref myVec 2) ; 0-indexed, gets myVec[2]

; Set an index
(vector-set! myVec 2 (exp 2 4)) ; myVec[2] = 2^4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Vector sum from lecture 02:
; This function does v_n + v_{n-1} + v_{n-2} + ... + v_0
(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0) ; first element if n=0
        (+ (vector-ref v n) (partial-vector-sum v (- n 1)))))) ; nth elem + run this for n-1

; This function sums elements in a vector using the general one above
(define vector-sum
  (lambda (v)
    (let (n (vector-length v)))
    (if (zero? n)
        0
        (partial-vector-sum v (- n 1)))))