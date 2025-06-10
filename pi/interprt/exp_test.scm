;;;; @(#)$Id: exp_test.scm,v 1.2 1997/10/16 06:25:29 qfwfq Exp $
; $Log: exp_test.scm,v $
; Revision 1.2  1997/10/16 06:25:29  qfwfq
; Release version 0.40
;
; Revision 1.1  1997/04/26 13:29:18  qfwfq
; Version 0.30 - hygienic macro system with syntax-case
;

(define-syntax test-eval (lambda (x) (syntax-case x () ((_ e) (syntax (step e))))))
;(define-syntax test-eval (lambda (x) (syntax-case x () ((_ e) (syntax (begin (write e) (newline)))))))

(define-syntax test
  (lambda (x)
    (syntax-case x ()
      ((_ e) (syntax (begin (write 'e) (display "-->") (newline) (test-eval e)))))))

(test (car (if #f 0 (if #t '(a b)))))

(test (begin (define x 0) (set! x (+ x 1)) x))

(test ((lambda (x) (+ x 1)) 2))

;(test '(if #t))
;(test '((lambda (x) (syntax-case x ())) 'foo))
;(test '((lambda (x) (syntax-case x () . #f)) 'foo))
;(test '((lambda (x) (syntax-case x () (a b c d))) 'foo))

(test (syntax-case '(if #t then (1 2 3) else (4 5)) (then else)
	((_ x then (y ...) else (z ...)) (syntax (if x (begin y ...) (begin z ...))))))

(test (syntax-case '(foo) () ((_) (syntax (... ...)))))

(define-syntax or2
  (lambda (x)
    (syntax-case x ()
      ((_ e1 e2)
       (syntax ((lambda (t) (if t t e2)) e1))))))

;(test '(or2 0 1 2))

(define t 't-val)

(test ((lambda (if) (or2 if t)) #f))

(define-syntax block
  (lambda (x)
    (syntax-case x ()
      ((k e1 e2 ...)
       (syntax-case (implicit-identifier (syntax k) 'return) ()
	 (return (syntax (call-with-current-continuation (lambda (return) e1 e2 ...)))))))))

(test (block (map (lambda (x) (if x (return x))) '(#f #f foo #f bar))))

(define-syntax pset!
  (lambda (x)
    (syntax-case x ()
      ((_ (var val) ...)
       (syntax-case (generate-temporaries (syntax (var ...))) ()
	 ((tmp ...) (syntax ((lambda (tmp ...) (set! var tmp) ...) val ...))))))))

(define a 0)
(define b 1)

(test (begin (pset! (a b) (b a)) (cons a b)))

(test
  (let-syntax ((if (lambda (x) (syntax-case x () ((_ t s1 s2 ...) (syntax (if t (begin s1 s2 ...))))))))
    (if #t (display "true") (newline))))

(test
  (letrec-syntax ((et (lambda (x)
			(syntax-case x ()
			  ((_) (syntax #t))
			  ((_ x) (syntax x))
			  ((_ x y z ...) (syntax (if x (et y z ...) #f)))))))
    (et 'foo 'bar 'baz)))

(test
  ((lambda (n)
     (define-syntax foo
       (lambda (x)
	 (syntax-case x ()
	   ((_ a) (syntax (if (null? a) 0 (bar (car a) (cdr a))))))))
     (define (bar x y) (+ x (foo y)))
     (foo n)) '(1 2 3 4)))

(test
  ((lambda (f)
     (define-syntax foo
       (lambda (x)
	 (syntax-case x ()
	   ((_ #(e ...)) (syntax (f e ...))))))
     (define-syntax bar
       (lambda (x)
	 (syntax-case x ()
	   ((_ e ...) (syntax (foo #(e ...)))))))
     (bar 1 2 3 4)) (lambda (x y . z) (cons (+ x y) z))))

(test
  (letrec-syntax ((foo (lambda (x)
			 (syntax-case x ()
			   ((_ (x y ...) ...) (syntax (begin (define x (begin y ...)) ...)))
			   ((_ x y z) (syntax (bar x (y . z)))))))
		  (bar (lambda (x)
			 (syntax-case x ()
			   ((_ x (y ...)) (syntax (begin (de y x) ... (list (y) ...)))))))
		  (de (lambda (x)
			 (syntax-case x ()
			   ((_ x y) (syntax (define x y)))))))
    (foo (a 0) (b 1))
    (foo (lambda () (set! a (+ a b)) a) x (y z))))

(test
  (let-syntax ((foo (lambda (x)
		      (syntax-case x (k)
			((_ (k #(a b ...)) ...)
			 (syntax-case (syntax ((b ...) ...)) ()
			   (b (syntax '#((a ...) (#t b))))))))))
    (foo)))

(test
  (let-syntax ((foo (lambda (x)
		      (syntax-case x (k)
			((_ k x) (syntax 'x))
			((_ x y) (syntax x)))))
	       (bar (lambda (x)
		      (syntax-case x ()
			((_ n p t)
			 (syntax (define-syntax n (lambda (x) (syntax-case x (k) (p (syntax t)))))))))))
    (bar baz (_ k) (foo k 0))
    ((lambda (k) (baz k)) "bah")))

(test
  (letrec-syntax ((foo (lambda (x)
			 (syntax-case x ()
			   ((_ x y) (identifier? (syntax x)) (syntax (x . y)))
			   ((_ x y) (syntax (foo list (x . y))))
			   ((_ x y ...) (syntax (foo x (y ...))))))))
    (foo 1 2 3)))

(define-syntax foo
  (lambda (x)
    (syntax-case x ()
      ((_ (((x ...) ...) ...) ((y ...) ...) (z ...))
       (syntax '(() (((x y z) ...) ... #() (y z) ...) ... z ...))))))

(test (foo (((1 2) (3 4 5) (6)) ((7) (8 9))) ((a b c) (d e)) (x y)))
