(load "machine.scm")

(define expt-machine
  (make-machine
   '(b n continue val)
   (list (list '= =)
	 (list '- -)
	 (list '* *))
   '(expt-start
       (perform (op initialize-stack))
       (perform (op print) (const "\n"))
       (assign b (op prompt-read) (const "b="))
       (assign n (op prompt-read) (const "n="))
       (assign continue (label expt-done))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (save b)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
     after-expt
       (restore n)
       (restore b)
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))
     base-case
       (assign val (const 1))
       (goto (reg continue))
     expt-done
       (perform (op print) (reg val))
       (perform (op print-stack-statistics))
       (goto (label expt-start)))))

(start expt-machine)

