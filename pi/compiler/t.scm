;;;; @(#)$Id: t.scm,v 1.3 1997/05/12 07:21:28 qfwfq Exp $
; $Log: t.scm,v $
; Revision 1.3  1997/05/12 07:21:28  qfwfq
; version 0.31 - some enhancements on error handling etc.
;
; Revision 1.2  1997/04/26 13:29:15  qfwfq
; Version 0.30 - hygienic macro system with syntax-case
;
; Revision 1.1  1996/09/06 06:12:40  qfwfq
; Version 0.20 unix revision is up.
;

(write (apply (lambda x x) '(0 1 2)))
(newline)
(write (apply (lambda (a b . x) (list a b x)) '(0 1 2)))
(newline)
(write (apply (lambda (a b . x) (list a b x)) '(0 1 2 3 4)))
(newline)
(write (apply (lambda x x) '()))
(newline)
(write (apply (lambda (a . x) (list a x)) '(1)))
(newline)
(write (apply (lambda (a b c . x) (list a b c x)) '(1 2 3)))
(newline)

(write '#((#(a b c) . #(e f)) #(#(1 (x ((y z) w))) #(3) #(4 5 6)) (1 2)))
(newline)

(write '(foo (a b . c) (d e) (f) () "ABCDEF" "" #t #f #\a 123 -1/1000000000000
	 #() #(655366553665536) #(#x-100000000 6/7 3.14 1@0.7854)))
(newline)

(write (((lambda (x) x) list) -3 -2 -1 (+)))
(newline)

(write (((lambda (x) x) list) -2 -1 (+) 1 2 3 4))
(newline)

(write (((lambda (x) x) list) -1 (+) 1 2 3))
(newline)

(write (((lambda (x) x) list) (+) 1 2))
(newline)

(write ((lambda (f) (f 1 2 (+) 3 4)) (lambda v1 v1)))
(newline)

(write ((lambda (f) (f 1 2 (+) 3))
	(lambda (v1 . v2) (list v1 v2))))
(newline)

(write ((lambda (f) (f 1 2 (+)))
	(lambda (v1 v2 . v3) (list v1 v2 v3))))
(newline)

(write ((lambda (f) (f 1 (+) 2 3 4 5))
	(lambda (v1 v2 v3 . v4) (list v1 v2 v3 v4))))
(newline)

(write ((lambda (f) (f 1 (+) 2))
	(lambda (v1 v2 . v3) (list v1 v2 v3))))
(newline)

(write ((lambda (f) (f (+)))
	(lambda (v1 . v2) (list v1 v2))))
(newline)

(write ((lambda (f) (f 1 (+)))
	(lambda (v1 v2) (list v1 v2))))
(newline)

(write ((lambda (f) (f 1 2 3 (+) 4 5))
	(lambda (v1 v2 v3 v4 v5 v6)
	  (list v1 v2 v3 v4 v5 v6))))
(newline)

(write ((lambda (f) (f 2 (+) 3 (*) 4 5 6 (list) 7))
	(lambda (v1 v2 v3 v4 v5 v6 v7 v8 v9)
	  (list v1 v2 v3 v4 v5 v6 v7 v8 v9))))
(newline)

(write (((lambda (x) x) list) 1 2 3 4))
(newline)

(write (((lambda (x) x) list) 1 2))
(newline)

(((lambda (x) x) write) 1)
(newline)

(write ((lambda (f g h)
	  (list (f 1 2 3) (g 1 2 3 4) (g 1 2) (h)))
	(lambda (a b c) (list a b c))
	(lambda (a b . c) (list a b c))
	(lambda a a)))
(newline)

(((lambda (d1 f1 d2 p1 p2 d3 d4 d5 p3 d6 f2 d7)
    (lambda () (f1 (f2 d1 d2 p1 p2 d3 d4 d5 p3 d6 d7))))
  2 (lambda (x) (write x)) 3 (+) (*) 4 5 6 (list) 7 (lambda x x) 8))
(newline)

((lambda (f x g y z) (f (list x y z)) (g))
 (lambda (x) (write x)) (+) (lambda () (newline)) 2 (*))

(write ((lambda x x) 2 3 (+) 4 (*) 5 6 (+) 7 8))
(newline)

(write ((lambda x x) 2 (+) (*) 3))
(newline)

(write ((lambda x x) (+)))
(newline)

((lambda () write (write ((lambda x x))) (newline)))

(define s8 (lambda (x) (if (not x) (list 'true) 'false)))
(write (s8 #t))
(newline)
(write (s8 #f))
(newline)

(define s7 (lambda (x) (if x 'true (list 'false))))
(write (s7 #t))
(newline)
(write (s7 #f))
(newline)

(define s6 (lambda (x) (if x 'true 'false)))
(write (s6 #t))
(newline)
(write (s6 #f))
(newline)

(define ee (lambda (m)
		(lambda (err obj) (write m) (write err))))
(define s5 (lambda (x)
		(rp:call-with-error-handler (ee 'error:)
		  (lambda () (if x (rp:error) (write 'no-error))))))
(s5 #t)
(newline)
(s5 #f)
(newline)

(write ((lambda (f g)
	 (set! f (lambda (n)
		   (if (zero? n) 1 ((lambda (n) (+ (f n) (g n))) (- n 1)))))
	 (set! g (lambda (n) (if (zero? n) 1 (* (f n) (g (- n 1))))))
	 (f 3)) (rp:void) (rp:void)))
(newline)

(write ((lambda (f g)
	  (set! f (lambda () 0))
	  (set! g (lambda () 1))) (rp:void) (rp:void)))

(write ((lambda (f) (set! f (lambda (x) x))) (rp:void)))
(newline)

((lambda (f g h x)
   (set! f (lambda () (write x) (newline)))
   (set! g (lambda (x) x))
   (f))
 (rp:void) (rp:void) (lambda x x) 6)

(define *g* #f)
(write
  ((lambda (f g h x y)
     (set! f (lambda (a) (set! x a)))
     (set! g (lambda () y))
     (set! x (h (g)))
     (set! h (lambda (x) (+ x 2)))
     (set! g (lambda (a) (set! *g* a)))
     (g (h x))
     (f 4)
     x)
   #f (rp:void) (lambda (x) (+ x 1)) (rp:void) 2))
(newline)
(write *g*)
(newline)

(define s4
  (lambda (x)
    (rp:call-with-error-handler (lambda (err obj) (write (list 'error err)))
	(lambda () (if x (rp:error)) (write (list 'no 'error))))
    (newline)))
(s4 #t)
(s4 #f)

(define s3
  (lambda (x)
    (if x
      (begin (write 'true) (newline))
      (begin (write 'false) (newline)))))
(s3 #t)
(s3 #f)

((lambda (f g x y z)
   (set! f (lambda (x) (write x) (newline)))
   (set! x z)
   (set! z (apply g x y '()))
   (apply f z '()))
 (rp:void) (lambda (a b) (+ a b 1)) (rp:void) 1 2)

((lambda (x) (write x) (newline)) 'bar)

(write ((lambda (a b . c) (cons (+ a b) c)) 1 2 3 4 5))
(newline)

((lambda (a)
  ((lambda (a)
    ((lambda (y) (a y)) 1))
   (lambda (x) (write (+ x a)))))
 2)
(newline)

(write (list 0 #f #t #\A 1/3 "string"))
(newline)

(begin
  (define s1 (lambda (x) x))
  (write (s1 0))
  (write 'foo)
  (define s2 (lambda x x))
  (write (s2 1))
  (newline)
  (rp:exit))
