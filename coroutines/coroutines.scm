(define call/cc call-with-current-continuation)

(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc
   (lambda (k)
     (enqueue k)
     (proc))))

(define (yield)
  (call/cc
   (lambda (k)
     (enqueue k)
     ((dequeue)))))

(define (thread-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define (do-stuff-n-print str)
  (lambda ()
    (let loop ((n 0))
      (display str)
      (display n)
      (display "\n")
      (yield)
      (loop (1+ n)))))

(fork (do-stuff-n-print "This is AAA"))
(fork (do-stuff-n-print "Hello from BBB"))
(thread-exit)