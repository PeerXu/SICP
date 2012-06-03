(load "machine.scm")

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(start
        (perform (op initialize-register))
     gcd-loop
        (perform (op initialize-stack))
        (perform (op print) (const "\n"))
        (assign a (op prompt-read) (const "a="))
	(assign b (op prompt-read) (const "b="))
     test-b
        (test (op =) (reg b) (const 0))
	(branch (label gcd-done))
	(assign t (op rem) (reg a) (reg b))
	(assign a (reg b))
	(assign b (reg t))
	(goto (label test-b))
     gcd-done
        (perform (op print) (reg a))
	(perform (op print-stack-statistics))
	(perform (op print) (reg cx)))))
	
(start gcd-machine)
(gcd-machine 'empty-cx)
(start gcd-machine)
