(define (divisible? x y) (= (remainder x y) 0))

(define the-empty-stream '())

(define (stream-null? stream)
  (eq? the-empty-stream stream))

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))
  
(define-syntax delay
  (syntax-rules ()
    ((_ x) (lambda () x))))

(define (force proc)
  (proc))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

;(define (stream-map proc stream)
;  (if (stream-null? stream)
;      '()
;      (cons-stream (proc (stream-car stream))
;		   (stream-map proc (stream-cdr stream)))))

;; ex3.50 p225
(define (stream-map proc . arguments)
  (if (null? (car arguments))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car arguments))
       (apply stream-map
	      (cons proc (map stream-cdr arguments))))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
	     (stream-for-each proc (stream-cdr stream)))))

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons-stream (stream-car stream)
		   (stream-take (stream-cdr stream) (- n 1)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream stream)
  (stream-for-each display-line stream))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* factor x)) stream))

;;; p230 ex3.54
(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;;; p230 ex3.55
(define (partial-sums stream)
  (let ((sum 0))
    (stream-map (lambda (x)
		  (set! sum (+ sum x))
		  sum)
		stream)))