(module ps2 mzscheme
  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROBLEM 1
  ; Draw your answers to a paper/online environment and include the picture/screenshot in your submission.
  ;

  ;; PROBLEM 2 - Binary Tree
  ;(define path
  ; ...

  ; hint: you might define more functions if you require it
  ; check the BST's below at the tests, see how they are structured as:
  ; BST = (list int
  ;          BST
  ;          BST)
  ;     | ()


  
  ;; PROBLEM 3
  ;; Write your answers here
  ;
  ;
  ;
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the function below, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  ;;; If you don't implement the functions in order and want to test as you go, you can comment out the corresponding tests,
  ;;; otherwise, DrRacket will raise errors.
  ;; PROBLEM 2 TESTS
  (display "\nBinary Search Tree Tests\n")
  (equal?? (path 4
    (list 4
      (list 2
        '()
        '())
      (list 6
        (list 5
          '()
          '())
        '()        
      )))
           '(root)) ; '(root)
  (equal?? (path 5
    (list 4
      (list 2
        '()
        '())
      (list 6
        (list 5
          '()
          '())
        '()        
      )))
           '(root right left)) ; '(root right left)
  (equal?? (path 1
    (list 5
      '()
      (list 9
        '()
        '())
      ))
           '()) ; '()
)