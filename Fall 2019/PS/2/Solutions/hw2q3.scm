(empty-stack)  = [0]		      The procedure empty-stack, applied to no arguments, must produce a representation of the empty stack.

(push var [s]) = [s1]	              The procedure push takes an argument and a representation of  a stack and produces a new stack.

(pop [s]) = [s1] 		      The procedure pop takes a representation of a stack and produces a new stack. Moreover, it returns the top-most argument which has just removed from the stack.

(top [s]) = (top-most-arg)	      The procedure top takes a representation of stack and returns an argument that is at the top of the stack.

(empty-stack? [s]) = { #t s=[0]
                       #f otherwise}  The empty-stack? procedure takes a representation of a stack and returns a boolean value.

constructors --> empty-stack
                 push
                 pop 

observers -->    top
                 empty-stack?
