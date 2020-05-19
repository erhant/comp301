#lang racket

; for loop params: variable, initial value, test function, increment function, iteration function

(define for
  (lambda (var initVal testF incrF iterF)
    (for-rec initVal testF incrF iterF)
    )
  )


(define for-rec
  (lambda (var testF incrF iterF)
    (iterF var)
    (if (testF var)
        (for-rec (incrF var) testF incrF iterF)
        #f
        )
    )
  )

(for-rec 0 (lambda (y) (< y 2)) (lambda (z) (+ z 1)) (lambda (t) (display t)))
        
