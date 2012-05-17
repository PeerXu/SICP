(load "machine.scm")

(define fact-machine
  (make-machine
   '(p c n)
   (list (list '> >) (list '+ +) (list '* *))
   '(fact-loop
        (perform (op initialize-stack))
        (perform (op print) (const "\n"))
        (assign n (op prompt-read) (const "n="))
        (assign p (const 1))
	(assign c (const 1))
     iter-loop
        (test (op >) (reg c) (reg n))
	(branch (label fact-done))
	(assign p (op *) (reg p) (reg c))
	(assign c (op +) (reg c) (const 1))
	(goto (label iter-loop))
     fact-done
        (perform (op print) (reg p))
	(perform (op print-stack-statistics))
	(goto (label fact-loop)))))

(start fact-machine)
