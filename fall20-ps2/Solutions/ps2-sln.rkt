(module ps2 mzscheme
  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROBLEM 1
  ; See the solution PDF

  ;; PROBLEM 2 - Binary Tree
  (define path
    (lambda (n bin-tree)
      (cond
        ((null? bin-tree) '()) ; return null if tree is null
        ((eqv? n (car bin-tree)) '(root)) ; return root if integer is here
        (else (path-rec n bin-tree '(root))))))

  (define path-rec
    (lambda (n bin-tree path-so-far) ; we store our path in the waylst, initially '()
      (cond
        ((null? bin-tree) '()); Returns empty list if tree is null
        ((eqv? n (car bin-tree)) path-so-far) ; Returns waylst if we found our integer
        (else (append
               (path-rec n (cadr bin-tree) (append path-so-far '(left))) ;else we recurse to left branch 
               (path-rec n (caddr bin-tree) (append path-so-far '(right)))))))) ; and right branch, note that appending empty list to "some list" results in that "some list" only!

  ;; Problem 3
  ;; a. Evaluates to 3. The expression is syntactically correct, but x is not used anyways and it just returns 3.
  ;; b. Evaluates to -7 from -4 - 3 = -7
  ;; c. Evaluates to 0. Though out of scope of this class, when you consider that an else expression is optional in a grammar such as LET's, you can derive an expression in more than one way. Such a grammar would be called ``ambiguous''. Google ``ambiguous grammar'' if interested!
  
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