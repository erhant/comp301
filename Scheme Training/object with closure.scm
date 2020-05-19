;guile 2.0.11
; Object using Closure

(define make-counter-object
    (lambda (starting-number)
        (let* (
                (original-value starting-number)
                (next-value
                    (lambda ()
                        (set! starting-number (+ starting-number 1))
                        starting-number
                    )
                )
                (reset-value
                   (lambda ()
                        (set! starting-number original-value)
                   )
                )
              )
         (list (list `next next-value) (list `reset reset-value))
        )
    )
)