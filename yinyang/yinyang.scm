(define call/cc call-with-current-continuation)

(newline)
(let* ((yin ((lambda (x)
	       (display #\@)
	       x)
	     (call/cc (lambda (foo) foo))))
       (yang ((lambda (y)
		(display #\.)
		y)
	      (call/cc (lambda (bar) bar)))))
  (yin yang))