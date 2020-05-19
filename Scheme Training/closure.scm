;guile 2.0.11
;Closure example

#lang eopl

(define make-counter
    (lambda (starting-number)
        (let(
                (next-value 
                    (lambda () ; does not take any parameters 
                         (set! starting-number (+ starting-number 1)) ; starting-number++
                         starting-number ; this is the last expression => return starting-number
                    )
                )
            )            
            next-value ; we created this function using `starting-number` and now we return it to be accessed by `make-counter`
        )
    )
)

(define my-counter (make-counter 10))
(define my-other-counter (make-counter 100))
(display (my-counter)) (display "\n")
(display (my-other-counter)) (display "\n")
(display (my-other-counter)) (display "\n")
(display (my-counter)) (display "\n")