; Copyright (c) 1996-99 Inujima, Masaru <qfwfq@kt.rim.or.jp>
;
; Permission to use, copy, modify, and distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;;; @(#)$Id: pisc.scm,v 1.8 2004/08/06 05:48:06 qfwfq Exp $
; $Log: pisc.scm,v $
; Revision 1.8  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.7  1999/06/29 07:49:04  qfwfq
; Check duplicate module loading.
;
; Revision 1.6  1999/06/15 07:36:00  qfwfq
; Various shared library arrangement
;
; Revision 1.5  1999/02/15 08:28:43  qfwfq
; port to Microsoft C compiler
;
; Revision 1.4  1997/10/16 06:25:04  qfwfq
; Release version 0.40
;
; Revision 1.3  1997/05/12 07:21:26  qfwfq
; version 0.31 - some enhancements on error handling etc.
;
; Revision 1.2  1997/04/26 13:29:13  qfwfq
; Version 0.30 - hygienic macro system with syntax-case
;
; Revision 1.1  1996/09/06 06:12:39  qfwfq
; Version 0.20 unix revision is up.
;

(define (rp:usage)
  (display "$Id: pisc.scm,v 1.8 2004/08/06 05:48:06 qfwfq Exp $") (newline)
  (display "usage: ")
  (display (rp:basename (car *invocation-arg*) #f))
  (display " [-module <identifier>] [-output <filename>] {-mpath <dir>}... {-load <filename>}... filename")
  (newline)
  (exit 2))

(define rp:*module-id* #f)
(define rp:*output-file* #f)
(define rp:*input-file* #f)
(define rp:*additional-macro-path* '())
(define rp:*preload-files* '())

;;; Process command line arguments
(let loop ((args (cdr *invocation-arg*)))
  (if (null? args) (rp:usage)
    (let ((arg (car args)) (args (cdr args)))
      (cond ((or (string=? arg "-h") (string=? arg "-help")) (rp:usage))
	    ((null? args) (set! rp:*input-file* arg))
	    ((string=? arg "-module")
	     (set! rp:*module-id* (car args)) (loop (cdr args)))
	    ((string=? arg "-output")
	     (set! rp:*output-file* (car args)) (loop (cdr args)))
	    ((string=? arg "-mpath")
	     (set! rp:*additional-macro-path* (cons (car args) rp:*additional-macro-path*))
	     (loop (cdr args)))
	    ((string=? arg "-load")
	     (set! rp:*preload-files* (cons (if (string=? (car args) "-") 'stdin (car args)) rp:*preload-files*))
	     (loop (cdr args)))
	    (else (rp:usage))))))

(if (not rp:*module-id*) (set! rp:*module-id* (rp:basename rp:*input-file* ".scm")))
(if (not rp:*output-file*) (set! rp:*output-file* (string-append rp:*module-id* ".c")))
(set! rp:*macro-search-list*
  (cons (car rp:*macro-search-list*) (append (reverse rp:*additional-macro-path*) (cdr rp:*macro-search-list*))))

(define rp:*toplevel-environment* (cons #t '()))
(define rp:*refarray-index* 0)
(define rp:*procedure-index* 0)

(define (rp:error-reporter file exp)
  (lambda (err obj)
    (display (rp:basename (car *invocation-arg*) #f))
    (display ": in ") (write file) (display #\:) (newline)
    (if exp (begin (display "in expression:") (newline) (rp:write-error-exp exp) (newline)))
    (rp:print-error-message err obj) (newline) (exit 1)))

(define (rp:primitive-syntax check exp)
  (if (not check) (rp:cause-error "Error in primitive syntax" exp)))

(define (rp:register-literal obj)
  (define (new-object env obj . aux)
    (do ((env env (cdr env)))
      ((null? (cdr env))
       (set-cdr! env `((literal-object ,obj ,rp:*refarray-index* ,@aux)))
       (set! rp:*refarray-index* (+ rp:*refarray-index* 1))
       (cadr env))))
  (let loop ((env rp:*toplevel-environment*))
    (if (null? (cdr env))
      (cond ((pair? obj)
	     (new-object env obj
	       (caddr (rp:register-literal (car obj)))
	       (caddr (rp:register-literal (cdr obj)))))
	    ((vector? obj)
	     (apply new-object env obj
	       (map (lambda (obj) (caddr (rp:register-literal obj))) (vector->list obj))))
	    ((and (number? obj) (real? obj) (exact? obj) (not (integer? obj)))
	     (new-object env obj
	       (caddr (rp:register-literal (negative? obj)))
	       (caddr (rp:register-literal (abs (numerator obj))))
	       (caddr (rp:register-literal (denominator obj)))))
	    ((and (number? obj) (not (real? obj)))
	     (new-object env obj
	       (caddr (rp:register-literal (real-part obj)))
	       (caddr (rp:register-literal (imag-part obj)))))
	    (else (new-object env obj)))
      (let ((env (cdr env)))
	(if (and (eq? (caar env) 'literal-object) (equal? (cadar env) obj)) (car env) (loop env))))))

(define (rp:closure-force code)
  (if (eq? (car code) 'closure-delayed)
    (begin (set-car! code 'closure)
	   (set-cdr! code (cdr ((cadr code)))))))

(define (rp:generate-closure-obj code)
  (if (eq? (car code) 'local-ref)
    (let ((vcell (caddr code)))
      (if (or (eq? (cadr vcell) 'const) (eq? (cadr vcell) 'rconst))
	(let ((val (caddr vcell)))
	  (if (eq? (car val) 'closure) (set-car! val 'closure-obj)))))))

(define (rp:exp-to-objcode exp env)
  (let ((code (rp:exp-to-code exp env)))
    (rp:generate-closure-obj code)
    (if (eq? (car code) 'lambda-exp) (rp:make-closure code) code)))

(define rp:special-generators
  (list
    (list 'quote
      (lambda (exp env)
	(rp:primitive-syntax (and (pair? (cdr exp)) (null? (cddr exp))) exp)
	(list 'literal-object (caddr (rp:register-literal (cadr exp))))))
    (list 'rp:lambda
      (lambda (exp env)
	(rp:primitive-syntax (and (pair? (cdr exp)) (pair? (cddr exp))) exp)
	(letrec ((check-formal
		  (lambda (formal n)
		    (or (and (null? formal) (< n #x400)) (and (symbol? formal) (< n #x3ff))
			(and (pair? formal) (symbol? (car formal)) (check-formal (cdr formal) (+ n 1)))))))
	  (rp:primitive-syntax (check-formal (cadr exp) 2) exp))
	(list 'lambda-exp env (cadr exp) (cddr exp))))
    (list 'if
      (lambda (exp env)
	(rp:primitive-syntax
	  (and (pair? (cdr exp)) (pair? (cddr exp)) (or (null? (cdddr exp)) (null? (cddddr exp)))) exp)
	(list 'if
	  (rp:exp-to-objcode (cadr exp) env)
	  (rp:exp-to-objcode (caddr exp) env)
	  (if (null? (cdddr exp)) (list 'void) (rp:exp-to-objcode (cadddr exp) env)))))
    (list 'set!
      (lambda (exp env)
	(rp:primitive-syntax (and (pair? (cdr exp)) (symbol? (cadr exp)) (pair? (cddr exp)) (null? (cdddr exp))) exp)
	(cons 'setbang
	  (letrec ((locate-var
		    (lambda (var venv depth)
		      (if (null? venv)
			(list (list 'global (rp:register-literal var)) (rp:exp-to-objcode (caddr exp) env))
			(let ((vcell (assq var (cdar venv))))
			  (if vcell
			    (let ((code (rp:exp-to-code (caddr exp) env)))
			      (rp:generate-closure-obj code)
			      (if (and (eq? (cadr vcell) 'unref) (eq? (caaddr vcell) 'void) (eqv? depth 0))
				(begin
				  (set-car! (cdr vcell) 'rconst)
				  (set-car! (cddr vcell)
				    (if (eq? (car code) 'lambda-exp)
				      (list 'closure-delayed (lambda () (rp:make-closure code)))
				      code))
				  (list (list 'local depth vcell) (caddr vcell)))
				(begin
				  (if (not (eq? (cadr vcell) 'var)) (rp:closure-force (caddr vcell)))
				  (if (not (eq? (cadr vcell) 'var))	; yes, necessary to check again.
				    (begin
				      (set-cdr! (cdr vcell)
					(list (caar venv) (if (eq? (cadr vcell) 'rconst) (list 'void) (caddr vcell))))
				      (set-car! (car venv) (+ (caar venv) 1))
				      (set-car! (cdr vcell) 'var)))
				  (list (list 'local depth vcell)
					(if (eq? (car code) 'lambda-exp) (rp:make-closure code) code)))))
			    (locate-var var (cdr venv) (+ depth 1))))))))
	    (locate-var (cadr exp) env 0)))))
    (list 'begin (lambda (exp env) (rp:sequence-code env (cdr exp))))
    (list 'rp:define (lambda (exp env) (rp:cause-error "Definition at illegal place" exp)))))

(define (rp:sequence-code env body)
  (if (null? body) (rp:cause-error "Null sequencing body" #f))
  (if (null? (cdr body))
    (rp:exp-to-objcode (car body) env)
    (list 'sequence (map (lambda (exp) (rp:exp-to-objcode exp env)) body))))

(define (rp:make-closure code)
  (letrec ((make-frame
	    (lambda (formals vno)
	      (cond ((null? formals) (list '() vno #f))
		    ((pair? formals)
		     (let ((fspec (make-frame (cdr formals) (+ vno 1))))
		       (cons (cons (list (car formals) 'var vno) (car fspec)) (cdr fspec))))
		    (else (list (list (list formals 'var vno)) (+ vno 1) #t))))))
    (let ((fspec (make-frame (caddr code) 0)))
      (let ((frame (cons (cadr fspec) (car fspec))))
	(list 'closure (caddr fspec) frame (rp:sequence-code (cons frame (cadr code)) (cadddr code)))))))

(define (rp:apply-code lst exp)
  (if (> (length lst) #x1ffc) (rp:cause-error "Argument count is too large" exp))
  (let ((fun (car lst))
	(args (map (lambda (code) (if (eq? (car code) 'lambda-exp) (rp:make-closure code) code)) (cdr lst))))
    (for-each rp:generate-closure-obj args)
    (if (eq? (car fun) 'lambda-exp)
      (let ((argerr (lambda () (rp:cause-error "Argument count mismatch" exp))))
	(let ((frame
	       (cons 0
		 (let loop ((formals (caddr fun)) (args args))
		   (cond ((null? formals) (if (null? args) '() (argerr)))
			 ((pair? formals)
			  (if (null? args) (argerr)
			    (cons (list (car formals) 'unref (car args)) (loop (cdr formals) (cdr args)))))
			 (else (list (list formals 'unref (list 'list args)))))))))
	  (let ((body (rp:sequence-code (cons frame (cadr fun)) (cadddr fun))))
	    (for-each
	      (lambda (vcell)
		(if (not (eq? (cadr vcell) 'var)) (rp:closure-force (caddr vcell)))
		(if (and (not (eq? (cadr vcell) 'var)) (not (eq? (car (caddr vcell)) 'closure)))
		  (let ((vno (car frame)))
		    (set-car! frame (+ vno 1))
		    (let ((val (if (eq? (cadr vcell) 'rconst) (list 'void) (caddr vcell))))
		      (set-car! (cdr vcell) 'var)
		      (set-cdr! (cdr vcell) (list vno val))))))
	      (cdr frame))
	    (list 'bind frame body))))
      (list 'apply fun args))))

(define (rp:variable-ref-code exp env depth)
  (if (null? env)
    (list 'global-ref (rp:register-literal exp))
    (let ((vcell (assq exp (cdar env))))
      (if vcell
	(begin
	  (if (eq? (cadr vcell) 'unref) (set-car! (cdr vcell) 'const))
	  (if (not (eq? (cadr vcell) 'var)) (rp:closure-force (caddr vcell)))
	  (list 'local-ref depth vcell))
	(rp:variable-ref-code exp (cdr env) (+ depth 1))))))

(define (rp:make-literal exp)
  (cond ((and (integer? exp) (exact? exp) (<= #x-20000000 exp #x1fffffff)) (list 'immediate-int exp))
	((eq? exp #f) (list 'immediate-bool-f))
	((eq? exp #t) (list 'immediate-bool-t))
	((eq? exp '()) (list 'immediate-nil))
	((char? exp) (list 'immediate-char exp))
	(else (list 'literal-object (caddr (rp:register-literal exp))))))

(define (rp:exp-to-code exp env)
  (set! exp (rp:expand-macro exp))
  (if (pair? exp)
    (cond ((assq (car exp) rp:special-generators) => (lambda (spec) ((cadr spec) exp env)))
	  ((and (null? (cdr exp)) (eq? (rp:expand-macro (car exp)) 'rp:void)) (list 'void))
	  (else (rp:apply-code (map (lambda (exp) (rp:exp-to-code exp env)) exp) exp)))
    (cond ((symbol? exp) (rp:variable-ref-code exp env 0))
	  ((or (number? exp) (string? exp) (char? exp) (boolean? exp)) (rp:make-literal exp))
	  (else (rp:cause-error "Object not allowed to be evaluated" exp)))))

(define (rp:expand-macro exp)
  (if (and (pair? exp) (eq? (car exp) 'rp:resolvant))
    (rp:expand-macro (rp:resolve-syntax (cdr exp)))
    exp))

(define (rp:new-procedure outp)
  (let ((pno rp:*procedure-index*))
    (set! rp:*procedure-index* (+ pno 1))
    (rp:emit-line outp "static rk_object rp_c_~A;" pno)
    pno))

(define (rp:start-procedure pno outp)
  (rp:emit-line outp "static rk_object RpC~A(void) {" pno)
  (rp:emit-line outp "~Trk_object c;"))

(define (rp:procedure-prologue outp)
  (newline outp)
  (let ((pno (rp:new-procedure outp))) (rp:start-procedure pno outp) pno))

(define (rp:procedure-epilogue-return outp)
  (rp:emit-line outp "~TRP_RETURN();")
  (rp:emit-line outp "}"))

(define (rp:procedure-epilogue-call-no pno outp)
  (rp:emit-line outp "~Treturn rp_c_~A;" pno)
  (rp:emit-line outp "}"))

(define (rp:procedure-epilogue-call proc outp) (rp:procedure-epilogue-call-no (cadr proc) outp))

(define (rp:load-literal code outp)
  (case (car code)
    ((immediate-int) (rp:emit-code outp "RK_MAKEINUM(~A)" (cadr code)))
    ((immediate-bool-f) (rp:emit-code outp "RK_SOBJ_FALSE"))
    ((immediate-bool-t) (rp:emit-code outp "RK_SOBJ_TRUE"))
    ((immediate-nil) (rp:emit-code outp "RK_SOBJ_NIL"))
    ((immediate-char) (rp:emit-code outp "RK_MAKEICHAR(~A)" (char->integer (cadr code))))
    ((literal-object) (rp:emit-code outp "rp_refarray[~A]" (cadr code)))))

(define (rp:non-direct procs)
  (if (null? procs) procs (if (eq? (caar procs) 'direct) (rp:non-direct (cdr procs)) procs)))

(define (rp:direct-to-proc-delayed gen outp)
  (lambda ()
    (gen "rk_eval_register[0]" "rk_eval_register[0]" 1)
    (rp:procedure-epilogue-return outp)))

(define (rp:realize-direct-proc proc outp)
  (if (eq? (car proc) 'proc) (cadr proc)
    (let ((pno (rp:procedure-prologue outp))) ((rp:direct-to-proc-delayed (cadr proc) outp)) pno)))

(define (rp:emit-procedure code pno outp)
  (let ((proc (rp:code-to-proc-delayed code outp)))
    (newline outp)
    (rp:start-procedure pno outp)
    ((if (eq? (car proc) 'proc) (cadr proc) (rp:direct-to-proc-delayed (cadr proc) outp)))))

(define rp:code-to-proc
  (let ((simple-apps '()))
    (lambda (code outp)
      (define (simple-app-code code)
	(let ((fun (let ((fcode (cadr code))) (and (eq? (car fcode) 'global-ref) (caddr (cadr fcode))))))
	  (and fun
	    (let ((args
		   (let loop ((acode (caddr code)))
		     (if (null? acode) '()
		       (let ((arg (car acode)))
			 (case (car arg)
			   ((immediate-int immediate-bool-f immediate-bool-t immediate-nil immediate-char
			    literal-object) (let ((r (loop (cdr acode)))) (and r (cons arg r))))
			   ((global-ref)
			    (let ((r (loop (cdr acode))))
			      (and r (cons (list 'global-ref (caddr (cadr arg))) r))))
			   ((local-ref)
			    (let ((r (loop (cdr acode))))
			      (and r (cons (list 'local-ref (cadr arg) (caddr (caddr arg))) r))))
			   ((apply)
			    (let ((ac (simple-app-code arg)))
			      (and ac (let ((r (loop (cdr acode)))) (and r (cons (cons 'apply ac) r))))))
			   (else #f)))))))
	      (and args (cons fun args))))))
      (define (lookup code)
	(cond ((assv (car code) simple-apps) => (lambda (l) (assoc (cdr code) (cdr l))))
	      (else #f)))
      (define (record code proc)
	(cond ((assv (car code) simple-apps) => (lambda (l) (set-cdr! l (cons (cons (cdr code) proc) (cdr l)))))
	      (else (set! simple-apps (cons (list (car code) (cons (cdr code) proc)) simple-apps)))))
      (define (genproc)
	(let ((proc (rp:code-to-proc-delayed code outp)))
	  (if (eq? (car proc) 'direct) proc
	    (let ((pno (rp:procedure-prologue outp))) ((cadr proc)) (list 'proc pno)))))
      (let ((simple (and (eq? (car code) 'apply) (simple-app-code code))))
	(cond ((and simple (lookup simple)) => cdr)
	      (else (let ((proc (genproc))) (if simple (record simple proc)) proc)))))))

(define (rp:code-to-proc-delayed code outp)
  (case (car code)
    ((immediate-int immediate-bool-f immediate-bool-t immediate-nil immediate-char literal-object)
     (list 'direct
       (lambda (store env vreg) (rp:emit-line outp "~T~A = ~V;" store (lambda () (rp:load-literal code outp))))))
    ((global-ref)
     (list 'direct
       (lambda (store env vreg)
	 (rp:emit-line outp "~Tif ((~A = ((rk_object *)rp_refarray[~A])[1]) == RK_SOBJ_UNBOUND)"
	   store (caddr (cadr code)))
	 (rp:emit-line outp "~T~TRK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, rp_refarray[~A]);" (caddr (cadr code))))))
    ((local-ref)
     (list 'direct
       (lambda (store env vreg)
	 (rp:emit-line outp "~T~A = ~V[~A];" store
	   (lambda ()
	     (letrec
	       ((access-env
		 (lambda (env d)
		   (rp:emit-code outp "((rk_object *)~V)"
		     (lambda ()
		       (if (zero? d)
			 (rp:emit-code outp "~A" env)
			 (rp:emit-code outp "~V[1]" (lambda () (access-env env (- d 1))))))))))
	       (access-env env (cadr code))))
	   (+ (caddr (caddr code)) 2)))))
    ((void)
     (list 'direct (lambda (store env vreg) (rp:emit-line outp "~T~A = RK_SOBJ_UNSPEC;" store))))
    ((closure closure-obj)
     (let ((body-pno (rp:realize-direct-proc (rp:code-to-proc (cadddr code) outp) outp)))
       (list 'direct
	 (lambda (store env vreg)
	   (rp:emit-line outp "~T~A = RpMakeCompiledClosure(rp_c_~A, ~A, ~A, ~A);"
	       store body-pno (caaddr code) (if (cadr code) 1 0) env)))))
    ((list)
     (let ((elts (map (lambda (code) (rp:code-to-proc code outp)) (cadr code))))
       (if (null? elts)
	 (list 'direct (lambda (store env vreg) (rp:emit-line outp "~T~A = RK_SOBJ_NIL;" store)))
	 (let ((nelts (length elts)) (cproc (rp:non-direct elts)))
	   (if (null? cproc)
	     (list 'direct
	       (lambda (store env vreg)
		 (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* nelts 2))
		 (do ((i 0 (+ i 1)))
		   ((eqv? i (- nelts 1))
		    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
		    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
		   (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
		   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
		     (+ (* i 2) 1) (* (+ i 1) 2)))
		 (rp:emit-line outp "~Trk_eval_register[~A] = rk_eval_register[~A] = c;" vreg (+ vreg 1))
		 (rp:emit-line outp "~Trk_valid_register = ~A;" (+ vreg 2))
		 (do ((elts elts (cdr elts))) ((null? elts))
		   ((cadar elts) "c" env (+ vreg 2))
		   (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[~A]), c);" (+ vreg 1))
		   (if (not (null? (cdr elts)))
		     (rp:emit-line outp
		       "~Trk_eval_register[~A] = RP_CDR(rk_eval_register[~A]);" (+ vreg 1) (+ vreg 1))))
		 (rp:emit-line outp "~T~A = rk_eval_register[~A];" store vreg)
		 (rp:emit-line outp "~Trk_valid_register = ~A;" vreg)))
	     (letrec
	       ((genconti
		 (lambda (elts n)
		   (let ((cproc (rp:non-direct elts)))
		     (if (null? cproc)
		       (let ((pno (rp:procedure-prologue outp)))
			 (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* nelts 2))
			 (do ((i 0 (+ i 1)))
			   ((eqv? i (- nelts 1))
			    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
			   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
			     (+ (* i 2) 1) (* (+ i 1) 2)))
			 (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];" (* n 2))
			 (rp:emit-line outp "~Trk_eval_register[0] = c;")
			 (if (not (zero? n))
			   (begin
			     (rp:emit-line outp "~Tc = rk_continuation[3];")
			     (do ((i (- n 1) (- i 1))) ((negative? i))
			       (rp:emit-line outp "~T((rk_object *)rk_eval_register[0])[~A] = RP_CAR(c);" (* i 2))
			       (if (not (zero? i)) (rp:emit-line outp "~Tc = RP_CDR(c);")))))
			 (do ((i (+ n 1) (+ i 1))) ((eqv? i nelts))
			   (rp:emit-line outp "~T((rk_object *)rk_eval_register[0])[~A] = RK_DUMMY_OBJ;" (* i 2)))
			 (if (not (eqv? (+ n 1) nelts))
			   (begin
			     (rp:emit-line outp
			       "~Trk_eval_register[1] = (rk_object)&((rk_object *)rk_eval_register[0])[~A];"
			       (* (+ n 1) 2))
			     (rp:emit-line outp "~Trk_valid_register = 2;")
			     (do ((elts elts (cdr elts))) ((null? elts))
			       ((cadar elts) "c" "rk_continuation[2]" 2)
			       (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[1]), c);")
			       (if (not (null? (cdr elts)))
				 (rp:emit-line outp "~Trk_eval_register[1] = RP_CDR(rk_eval_register[1]);")))))
			 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[4];")
			 (rp:procedure-epilogue-return outp)
			 pno)
		       (let ((conti (genconti (cdr cproc) (- nelts (length cproc)))))
			 (let ((pno (rp:procedure-prologue outp)))
			   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);"
			     (+ 6 (* 2 (- nelts (+ n (length cproc))))))
			   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
			   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
			   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
			   (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];")
			   (rp:emit-line outp "~T((rk_object *)c)[4] = rk_continuation[4];")
			   (rp:emit-line outp "~T((rk_object *)c)[5] = RK_DUMMY_OBJ;")
			   (do ((i 0 (+ i 1)))
			     ((eqv? i (- (- nelts (+ n (length cproc))) 1))
			      (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];" (+ 6 (* i 2)))
			      (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_continuation[3];" (+ 7 (* i 2))))
			     (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ 6 (* i 2)))
			     (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
			       (+ 7 (* i 2)) (+ 8 (* i 2))))
			   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
			   (if (not (eq? elts cproc))
			     (begin
			       (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];")
			       (let loop ((elts elts))
				 (if (not (eq? (cdr elts) cproc))
				   (begin
				     (loop (cdr elts))
				     (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))
				 ((cadar elts) "c" "rk_continuation[2]" 1)
				 (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[0]), c);"))))
			   (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
			   (rp:procedure-epilogue-call (car cproc) outp)
			   pno)))))))
	       (let ((conti (genconti (cdr cproc) (- nelts (length cproc)))))
		 (list 'proc
		   (lambda ()
		     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ 6 (* 2 (- nelts (length cproc)))))
		     (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
		     (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		     (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
		     (if (eq? elts cproc)
		       (rp:emit-line outp "~T((rk_object *)c)[3] = RK_SOBJ_NIL;")
		       (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];"))
		     (rp:emit-line outp "~T((rk_object *)c)[4] = (rk_object)rk_continuation;")
		     (rp:emit-line outp "~T((rk_object *)c)[5] = RK_DUMMY_OBJ;")
		     (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		     (if (not (eq? elts cproc))
		       (begin
			 (do ((i 0 (+ i 1)))
			   ((eqv? i (- (- nelts (length cproc)) 1))
			    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ 6 (* i 2)))
			    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ 7 (* i 2))))
			   (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ 6 (* i 2)))
			   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
			     (+ 7 (* i 2)) (+ 8 (* i 2))))
			 (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];")
			 (let loop ((elts elts))
			   (if (not (eq? (cdr elts) cproc))
			     (begin
			       (loop (cdr elts))
			       (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))
			   ((cadar elts) "c" "rk_continuation[2]" 1)
			   (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[0]), c);"))))
		     (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
		     (rp:procedure-epilogue-call (car cproc) outp))))))))))
    ((bind)
     (for-each (lambda (ppair) (rp:emit-procedure (cadr ppair) (car ppair) outp))
       (let loop ((vlist (cdadr code)) (first #t))
	 (if (null? vlist) '()
	   (let ((vcell (car vlist)))
	     (if (eq? (cadr vcell) 'var) (loop (cdr vlist) first)
	       (let ((body-code (cadddr (caddr vcell)))
		     (pno (begin (if first (newline outp)) (rp:new-procedure outp))))
		 (set-car! (cdddr (caddr vcell)) pno)
		 (cons (list pno body-code) (loop (cdr vlist) #f))))))))
     (let
       ((frame (cons (caadr code)
		     (map (lambda (vcell)
			    (if (not (eq? (cadr vcell) 'var)) vcell
			      (list (car vcell) (cadr vcell) (caddr vcell) (rp:code-to-proc (cadddr vcell) outp))))
			  (cdadr code))))
	(body (rp:code-to-proc (caddr code) outp)))
       (letrec
	 ((non-direct-var
	    (lambda (vlist)
	      (if (null? vlist) vlist
		(let ((vdesc (cdar vlist)))
		  (if (and (eq? (car vdesc) 'var) (not (eq? (car (caddr vdesc)) 'direct))) vlist
		    (non-direct-var (cdr vlist)))))))
	  (check-var
	    (lambda (vars cvars)
	      (if (eq? vars cvars) #f (if (eq? (cadar vars) 'var) #t (check-var (cdr vars) cvars))))))
	 (let ((flen (if (even? (car frame)) (car frame) (+ (car frame) 1))) (cvars (non-direct-var (cdr frame))))
	   (if (null? cvars)
	     (let ((body-pno (rp:realize-direct-proc body outp)))
	       (list 'proc
		 (lambda ()
		   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ flen 2))
		   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(~A, 0);" (+ flen 2))
		   (rp:emit-line outp "~T((rk_object *)c)[1] = rk_eval_register[0];")
		   (do ((i 0 (+ i 1))) ((eqv? i flen))
		     (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ i 2)))
		   (rp:emit-line outp "~Trk_eval_register[1] = c;")
		   (rp:emit-line outp "~Trk_valid_register = 2;")
		   (do ((vars (cdr frame) (cdr vars))) ((null? vars))
		     (if (eq? (cadar vars) 'var)
		       (begin
			 ((cadr (cadddr (car vars))) "c" "rk_eval_register[0]" 2)
			 (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], c);"
			   (+ (caddar vars) 2)))))
		   (rp:emit-line outp "~Trk_eval_register[0] = rk_eval_register[1];")
		   (rp:emit-line outp "~Trk_valid_register = 1;")
		   (rp:procedure-epilogue-call-no body-pno outp))))
	     (letrec
	       ((genconti
		 (lambda (vars)
		   (let ((cvars (non-direct-var vars)))
		     (if (null? cvars)
		       (let ((pno (rp:procedure-prologue outp)))
			 (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ flen 2))
			 (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(~A, 0);" (+ flen 2))
			 (rp:emit-line outp "~T((rk_object *)c)[1] = rk_continuation[2];")
			 (rp:emit-line outp "~Trk_eval_register[1] = c;")
			 (rp:emit-line outp "~Tc = rk_continuation[3];")
			 (let
			   ((processed-var
			     (let
			       ((vlist
				 (letrec
				   ((mk-vlist
				      (lambda (v vl)
					(if (eq? (cdr v) vars) (cons (car v) vl)
					  (if (eq? (cadar v) 'var)
					    (mk-vlist (cdr v) (cons (car v) vl))
					    (mk-vlist (cdr v) vl))))))
				   (mk-vlist (cdr frame) '()))))
			       (rp:emit-line outp "~T((rk_object *)rk_eval_register[1])[~A] = rk_eval_register[0];"
				 (+ (caddar vlist) 2))
			       (do ((vlist (cdr vlist) (cdr vlist))) ((null? vlist))
				 (rp:emit-line outp "~T((rk_object *)rk_eval_register[1])[~A] = RP_CAR(c);"
				   (+ (caddar vlist) 2))
				 (if (not (null? (cdr vlist))) (rp:emit-line outp "~Tc = RP_CDR(c);")))
			       (map caddr vlist))))
			   (do ((i 0 (+ i 1))) ((eqv? i flen))
			     (if (not (memv i processed-var))
			       (rp:emit-line outp
				 "~T((rk_object *)rk_eval_register[1])[~A] = RK_DUMMY_OBJ;" (+ i 2)))))
			 (rp:emit-line outp "~Trk_valid_register = 2;")
			 (do ((vars vars (cdr vars))) ((null? vars))
			   (if (eq? (cadar vars) 'var)
			     (begin
			       ((cadr (cadddr (car vars))) "c" "rk_continuation[2]" 2)
			       (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], c);"
				 (+ (caddar vars) 2)))))
			 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[4];")
			 (if (eq? (car body) 'direct)
			   (begin
			     ((cadr body) "rk_eval_register[0]" "rk_eval_register[1]" 2)
			     (rp:procedure-epilogue-return outp))
			   (begin
			     (rp:emit-line outp "~Trk_eval_register[0] = rk_eval_register[1];")
			     (rp:emit-line outp "~Trk_valid_register = 1;")
			     (rp:procedure-epilogue-call body outp)))
			 pno)
		       (let
			 ((conti (genconti (cdr cvars)))
			  (have-direct (check-var vars cvars)))
			 (let ((pno (rp:procedure-prologue outp)))
			   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(8);")
			   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
			   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
			   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
			   (if have-direct
			     (rp:emit-line outp "~T((rk_object *)c)[3] = RK_DUMMY_OBJ;")
			     (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];"))
			   (rp:emit-line outp "~T((rk_object *)c)[4] = rk_continuation[4];")
			   (rp:emit-line outp "~T((rk_object *)c)[5] = RK_DUMMY_OBJ;")
			   (rp:emit-line outp "~T((rk_object *)c)[6] = rk_eval_register[0];")
			   (rp:emit-line outp "~T((rk_object *)c)[7] = rk_continuation[3];")
			   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
			   (if have-direct
			     (begin
			       (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];")
			       (rp:emit-line outp "~Trk_eval_register[1] = RK_DUMMY_OBJ;")
			       (rp:emit-line outp "~Trk_valid_register = 2;")
			       (do ((vars vars (cdr vars))) ((eq? vars cvars))
				 (if (eq? (cadar vars) 'var)
				   (begin
				     ((cadr (cadddr (car vars))) "rk_eval_register[1]" "rk_continuation[2]" 2)
				     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(2);")
				     (rp:emit-line outp "~T((rk_object *)c)[0] = rk_eval_register[1];")
				     (rp:emit-line outp "~T((rk_object *)c)[1] = rk_eval_register[0];")
				     (rp:emit-line outp "~Trk_eval_register[0] = c;"))))
			       (rp:emit-line outp "~TRkWriteCell(&rk_continuation[3], rk_eval_register[0]);")
			       (rp:emit-line outp "~Trk_valid_register = 1;")))
			   (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
			   (rp:procedure-epilogue-call (cadddr (car cvars)) outp)
			   pno)))))))
	       (let ((conti (genconti (cdr cvars))) (have-direct (check-var (cdr frame) cvars)))
		 (list 'proc
		   (lambda ()
		     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(6);")
		     (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
		     (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		     (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
		     (rp:emit-line outp "~T((rk_object *)c)[3] = RK_SOBJ_NIL;")
		     (rp:emit-line outp "~T((rk_object *)c)[4] = (rk_object)rk_continuation;")
		     (rp:emit-line outp "~T((rk_object *)c)[5] = RK_DUMMY_OBJ;")
		     (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		     (if have-direct
		       (begin
			 (rp:emit-line outp "~Trk_eval_register[1] = RK_SOBJ_NIL;")
			 (rp:emit-line outp "~Trk_valid_register = 2;")
			 (do ((vars (cdr frame) (cdr vars))) ((eq? vars cvars))
			   (if (eq? (cadar vars) 'var)
			     (begin
			       ((cadr (cadddr (car vars))) "rk_eval_register[0]" "rk_continuation[2]" 2)
			       (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(2);")
			       (rp:emit-line outp "~T((rk_object *)c)[0] = rk_eval_register[0];")
			       (rp:emit-line outp "~T((rk_object *)c)[1] = rk_eval_register[1];")
			       (rp:emit-line outp "~Trk_eval_register[1] = c;"))))
			 (rp:emit-line outp "~TRkWriteCell(&rk_continuation[3], rk_eval_register[1]);")
			 (rp:emit-line outp "~Trk_valid_register = 1;")
			 (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")))
		     (rp:procedure-epilogue-call (cadddr (car cvars)) outp))))))))))
    ((apply)
     (let ((fun (cadr code)) (args (map (lambda (code) (rp:code-to-proc code outp)) (caddr code))))
       (let
	 ((fun-proc
	   (if (and (eq? (car fun) 'local-ref) (not (eq? (cadr (caddr fun)) 'var)))
	     (list 'closure-call (if (eq? (cadr (caddr fun)) 'const) (+ (cadr fun) 1) (cadr fun)) (cdaddr (caddr fun)))
	     (list 'object-call (rp:code-to-proc fun outp)))))
	 (let ((cproc (rp:non-direct args)))
	   (if (null? cproc)
	     (cond
	       ((eq? (car fun-proc) 'closure-call)
		(let ((frame-no (cadr fun-proc))
		      (restp (car (caddr fun-proc)))
		      (vno (car (cadr (caddr fun-proc))))
		      (body-pno (caddr (caddr fun-proc))))
		  (if (or (< (length args) (if restp (- vno 1) vno)) (and (not restp) (> (length args) vno)))
		    (rp:cause-error "Argument count mismatch" #f))
		  (let ((flen (if (even? vno) vno (+ vno 1))))
		    (list 'proc
		      (lambda ()
			(rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ flen 2))
			(rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(~A, 0);" (+ flen 2))
			(rp:emit-line outp "~T((rk_object *)c)[1] = ~V;"
			  (lambda ()
			    (letrec
			      ((get-frame
				(lambda (n)
				  (if (zero? n)
				    (rp:emit-code outp "rk_eval_register[0]")
				    (rp:emit-code outp "((rk_object *)~V)[1]" (lambda () (get-frame (- n 1))))))))
			      (get-frame frame-no))))
			(do ((i 0 (+ i 1))) ((eqv? i flen))
			  (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ i 2)))
			(rp:emit-line outp "~Trk_eval_register[1] = c;")
			(rp:emit-line outp "~Trk_valid_register = 2;")
			(do ((i 0 (+ i 1)) (args args (cdr args)))
			  ((eqv? i (if restp (- vno 1) vno))
			   (if restp
			     (if (null? args)
			       (rp:emit-line outp "~T((rk_object *)rk_eval_register[1])[~A] = RK_SOBJ_NIL;" (+ i 2))
			       (begin
				 (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* (length args) 2))
				 (do ((i 0 (+ i 1)))
				   ((eqv? i (- (length args) 1))
				    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
				    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
				   (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
				   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
				     (+ (* i 2) 1) (* (+ i 1) 2)))
				 (rp:emit-line outp "~Trk_eval_register[2] = c;")
				 (rp:emit-line outp "~Trk_valid_register = 3;")
				 (rp:emit-line outp
				   "~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], c);" (+ i 2))
				 (do ((args args (cdr args))) ((null? args))
				   ((cadar args) "c" "rk_eval_register[0]" 3)
				   (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[2]), c);")
				   (if (not (null? (cdr args)))
				     (rp:emit-line outp "~Trk_eval_register[2] = RP_CDR(rk_eval_register[2]);")))))))
			  ((cadar args) "c" "rk_eval_register[0]" 2)
			  (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], c);" (+ i 2)))
			(rp:emit-line outp "~Trk_eval_register[0] = rk_eval_register[1];")
			(rp:emit-line outp "~Trk_valid_register = 1;")
			(rp:procedure-epilogue-call-no body-pno outp))))))
	       ((eq? (car (cadr fun-proc)) 'direct)
		(let ((argno (length args)))
		  (list 'proc
		    (lambda ()
		      (if (zero? argno)
			(rp:emit-line outp "~Trk_eval_register[1] = RK_DUMMY_OBJ;")
			(if (eqv? argno 1)
			  (rp:emit-line outp "~Trk_eval_register[1] = RK_SOBJ_NIL;")
			  (begin
			    (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* 2 (- argno 1)))
			    (do ((i 0 (+ i 1)))
			      ((eqv? i (- argno 2))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
			      (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
			      (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
				(+ (* i 2) 1) (* (+ i 1) 2)))
			    (rp:emit-line outp "~Trk_eval_register[1] = c;"))))
		      (if (<= argno 1)
			(rp:emit-line outp "~Trk_eval_register[2] = RK_MAKEINUM(~A);" argno)
			(rp:emit-line outp "~Trk_eval_register[2] = c;"))
		      (rp:emit-line outp "~Trk_valid_register = 3;")
		      ((cadr (cadr fun-proc)) "rk_eval_register[3]" "rk_eval_register[0]" 3)
		      (rp:emit-line outp "~Trk_valid_register = 4;")
		      (if (zero? argno)
			(rp:emit-line outp "~Trk_eval_register[0] = RK_DUMMY_OBJ;")
			(if (eqv? argno 1)
			  ((cadar args) "rk_eval_register[0]" "rk_eval_register[0]" 4)
			  (let
			    ((last-arg
			      (letrec
				((load-args
				  (lambda (args more)
				    (if (null? (cdr args)) (car args)
				      (let ((last-arg (load-args (cdr args) #t)))
					((cadar args) "c" "rk_eval_register[0]" 4)
					(rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[2]), c);")
					(if more
					  (rp:emit-line outp "~Trk_eval_register[2] = RP_CDR(rk_eval_register[2]);"))
					last-arg)))))
				(load-args args #f))))
			    ((cadr last-arg) "rk_eval_register[0]" "rk_eval_register[0]" 4)
			    (rp:emit-line outp "~Trk_eval_register[2] = RK_MAKEINUM(~A);" argno))))
		      (rp:emit-line outp "~Treturn rp_apply_object_proc;")
		      (rp:emit-line outp "}")))))
	       (else
		(let ((argno (length args)) (conti (rp:procedure-prologue outp)))
		  (cond
		    ((zero? argno) (rp:emit-line outp "~Trk_eval_register[1] = RK_DUMMY_OBJ;"))
		    ((eqv? argno 1) (rp:emit-line outp "~Trk_eval_register[1] = RK_SOBJ_NIL;"))
		    (else
		     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* 2 (- argno 1)))
		     (do ((i 0 (+ i 1)))
		       ((eqv? i (- argno 2))
			(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
			(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
		       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
		       (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
			 (+ (* i 2) 1) (* (+ i 1) 2)))
		     (rp:emit-line outp "~Trk_eval_register[1] = c;")))
		  (if (<= argno 1)
		    (rp:emit-line outp "~Trk_eval_register[2] = RK_MAKEINUM(~A);" argno)
		    (rp:emit-line outp "~Trk_eval_register[2] = c;"))
		  (rp:emit-line outp "~Trk_eval_register[3] = rk_eval_register[0];")
		  (rp:emit-line outp "~Trk_valid_register = 4;")
		  (cond
		    ((zero? argno) (rp:emit-line outp "~Trk_eval_register[0] = RK_DUMMY_OBJ;"))
		    ((eqv? argno 1) ((cadar args) "rk_eval_register[0]" "rk_continuation[2]" 4))
		    (else
		     (let
		       ((last-arg
			 (letrec
			   ((load-args
			     (lambda (args more)
			       (if (null? (cdr args)) (car args)
				 (let ((last-arg (load-args (cdr args) #t)))
				   ((cadar args) "c" "rk_continuation[2]" 4)
				   (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[2]), c);")
				   (if more (rp:emit-line outp "~Trk_eval_register[2] = RP_CDR(rk_eval_register[2]);"))
				   last-arg)))))
			   (load-args args #f))))
		       ((cadr last-arg) "rk_eval_register[0]" "rk_continuation[2]" 4)
		       (rp:emit-line outp "~Trk_eval_register[2] = RK_MAKEINUM(~A);" argno))))
		  (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[3];")
		  (rp:emit-line outp "~Treturn rp_apply_object_proc;")
		  (rp:emit-line outp "}")
		  (list 'proc
		    (lambda ()
		      (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
		      (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
		      (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		      (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
		      (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)rk_continuation;")
		      (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		      (rp:procedure-epilogue-call (cadr fun-proc) outp))))))
	     (let ((argno (length args)))
	       (letrec
		 ((genconti
		   (lambda (args)
		     (let ((cproc (rp:non-direct args)))
		       (if (null? cproc)
			 (if (eq? (car fun-proc) 'closure-call)
			   (let ((frame-no (cadr fun-proc))
				 (restp (car (caddr fun-proc)))
				 (vno (car (cadr (caddr fun-proc))))
				 (body-pno (caddr (caddr fun-proc))))
			     (if (or (< argno (if restp (- vno 1) vno)) (and (not restp) (> argno vno)))
			       (rp:cause-error "Argument count mismatch" #f))
			     (let ((flen (if (even? vno) vno (+ vno 1))) (pno (rp:procedure-prologue outp)))
			       (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ flen 2))
			       (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(~A, 0);" (+ flen 2))
			       (rp:emit-line outp "~T((rk_object *)c)[1] = ~V;"
				 (lambda ()
				   (letrec
				     ((get-frame
				       (lambda (n)
					 (if (zero? n)
					   (rp:emit-code outp "rk_continuation[2]")
					   (rp:emit-code outp "((rk_object *)~V)[1]"
					     (lambda () (get-frame (- n 1))))))))
				     (get-frame frame-no))))
			       (let ((nval (- argno (+ (length args) 1))))
				 (if (< (+ nval 1) (if restp (- vno 1) vno))
				   (do ((i (+ nval 1) (+ i 1))) ((eqv? i flen))
				     (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ i 2)))
				   (begin
				     (if (and restp (>= argno vno))
				       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ vno 1)))
				     (if (< vno flen)
				       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ flen 1)))))
				 (if (and restp (>= (+ nval 1) vno))
				   (begin
				     (do ((i 0 (+ i 1))) ((eqv? i (- vno 1)))
				       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ i 2)))
				     (rp:emit-line outp "~Trk_eval_register[1] = c;")
				     (rp:emit-line outp "~Trk_valid_register = 2;")
				     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* (+ (- argno vno) 1) 2))
				     (do ((i 0 (+ i 1)))
				       ((eqv? i (- argno vno))
					(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 1)))
				       (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
					 (+ (* i 2) 1) (* (+ i 1) 2)))
				     (do ((i (+ (- nval vno) 2) (+ i 1))) ((eqv? i (+ (- argno vno) 1)))
				       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2)))
				     (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];"
				       (* (+ (- nval vno) 1) 2))
				     (rp:emit-line outp "~Trk_eval_register[2] = c;")
				     (rp:emit-line outp "~Tc = rk_continuation[3];")
				     (do ((i (- nval vno) (- i 1))) ((negative? i))
				       (rp:emit-line outp
					 "~T((rk_object *)rk_eval_register[2])[~A] = RP_CAR(c);" (* i 2))
				       (rp:emit-line outp "~Tc = RP_CDR(c);"))
				     (rp:emit-line outp "~Trk_eval_register[0] = c;")
				     (rp:emit-line outp "~Tc = rk_eval_register[2];")
				     (if (not (null? args))
				       (begin
					 (rp:emit-line outp "~Trk_eval_register[2] = (rk_object)&((rk_object *)c)[~A];"
					   (* (+ (- nval vno) 2) 2))
					 (rp:emit-line outp "~Trk_valid_register = 3;")))
				     (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], c);"
				       (+ vno 1))
				     (do ((i (- vno 2) (- i 1))) ((negative? i))
				       (rp:emit-line outp
			"~TRkWriteCell(&((rk_object *)rk_eval_register[1])[~A], RP_CAR(rk_eval_register[0]));" (+ i 2))
				       (if (positive? i)
					 (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))
				     (do ((args args (cdr args))) ((null? args))
				       ((cadar args) "c" "rk_continuation[2]" 3)
				       (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[2]), c);")
				       (if (not (null? (cdr args)))
					 (rp:emit-line outp "~Trk_eval_register[2] = RP_CDR(rk_eval_register[2]);")))
				     (rp:emit-line outp "~Trk_eval_register[0] = rk_eval_register[1];")
				     (rp:emit-line outp "~Trk_valid_register = 1;"))
				   (begin
				     (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];" (+ nval 2))
				     (rp:emit-line outp "~Trk_eval_register[0] = c;")
				     (rp:emit-line outp "~Tc = rk_continuation[3];")
				     (do ((i (- nval 1) (- i 1))) ((negative? i))
				       (rp:emit-line outp
					 "~T((rk_object *)rk_eval_register[0])[~A] = RP_CAR(c);" (+ i 2))
				       (if (not (zero? i)) (rp:emit-line outp "~Tc = RP_CDR(c);")))
				     (do ((i (+ nval 1) (+ i 1)) (args args (cdr args)))
				       ((eqv? i (if restp (- vno 1) vno))
					(if restp
					  (if (null? args)
					    (rp:emit-line outp
					      "~T((rk_object *)rk_eval_register[0])[~A] = RK_SOBJ_NIL;" (+ i 2))
					    (let ((nrest (length args)))
					      (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* nrest 2))
					      (do ((i 0 (+ i 1))) ((eqv? i nrest))
						(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
						(if (eqv? i (- nrest 1))
						  (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;"
						    (+ (* i 2) 1))
						  (rp:emit-line outp
			"~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];" (+ (* i 2) 1) (* (+ i 1) 2))))
					      (rp:emit-line outp "~Trk_eval_register[1] = c;")
					      (rp:emit-line outp "~Trk_valid_register = 2;")
					      (rp:emit-line outp
						"~TRkWriteCell(&((rk_object *)rk_eval_register[0])[~A], c);" (+ vno 1))
					      (do ((args args (cdr args))) ((null? args))
						((cadar args) "c" "rk_continuation[2]" 2)
						(rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[1]), c);")
						(if (not (null? (cdr args)))
						  (rp:emit-line outp
						    "~Trk_eval_register[1] = RP_CDR(rk_eval_register[1]);")))
					      (rp:emit-line outp "~Trk_valid_register = 1;")))))
				       ((cadar args) "c" "rk_continuation[2]" 1)
				       (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rk_eval_register[0])[~A], c);"
					 (+ i 2)))))
				 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[4];")
				 (rp:procedure-epilogue-call-no body-pno outp))
			       pno))
			   (let ((pno (rp:procedure-prologue outp)))
			     (if (null? args)
			       (rp:emit-line outp "~Trk_eval_register[1] = rk_continuation[3];")
			       (let ((argno (length args)))
				 (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (* argno 2))
				 (do ((i 0 (+ i 1)))
				   ((eqv? i (- argno 1))
				    (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];" (* i 2))
				    (rp:emit-line outp "~T((rk_object *)c)[~A] = rk_continuation[3];" (+ (* i 2) 1)))
				   (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (* i 2))
				   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
				     (+ (* i 2) 1) (* (+ i 1) 2)))
				 (rp:emit-line outp "~Trk_eval_register[1] = c;")))
			     (rp:emit-line outp "~Trk_eval_register[2] = RK_MAKEINUM(~A);" argno)
			     (rp:emit-line outp "~Trk_eval_register[3] = rk_continuation[4];")
			     (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
			       (rp:emit-line outp "~Trk_valid_register = 4;"))
			     (cond
			       ((null? args))
			       ((null? (cdr args))
				((cadar args) "rk_eval_register[0]" "rk_continuation[2]" 4))
			       ((null? (cddr args))
				((cadar args) "c" "rk_continuation[2]" 4)
				(rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[1]), c);")
				((cadadr args) "rk_eval_register[0]" "rk_continuation[2]" 4))
			       (else
				(rp:emit-line outp "~Trk_eval_register[4] = rk_eval_register[1];")
				(rp:emit-line outp "~Trk_valid_register = 5;")
				(let
				  ((last-arg
				    (letrec
				      ((load-args
					(lambda (args more)
					  (if (null? (cdr args)) (car args)
					    (let ((last-arg (load-args (cdr args) #t)))
					      ((cadar args) "c" "rk_continuation[2]" 5)
					      (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[4]), c);")
					      (if more
						(rp:emit-line outp
						  "~Trk_eval_register[4] = RP_CDR(rk_eval_register[4]);"))
					      last-arg)))))
				      (load-args args #f))))
				  (rp:emit-line outp "~Trk_valid_register = 4;")
				  ((cadr last-arg) "rk_eval_register[0]" "rk_continuation[2]" 4))))
			     (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[5];")
			     (rp:emit-line outp "~Treturn rp_apply_object_proc;")
			     (rp:emit-line outp "}")
			     pno))
			 (let ((conti (genconti (cdr cproc))))
			   (let ((argno (+ (- (length args) (length cproc)) 1)) (pno (rp:procedure-prologue outp)))
			     (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ 6 (* argno 2)))
			     (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
			     (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
			     (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
			     (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];")
			     (rp:emit-line outp "~T((rk_object *)c)[4] = rk_continuation[4];")
			     (rp:emit-line outp "~T((rk_object *)c)[5] = rk_continuation[5];")
			     (do ((i 0 (+ i 1)))
			       ((eqv? i (- argno 1))
				(rp:emit-line outp "~T((rk_object *)c)[~A] = rk_eval_register[0];" (+ (* i 2) 6))
				(rp:emit-line outp "~T((rk_object *)c)[~A] = rk_continuation[3];" (+ (* i 2) 7)))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ (* i 2) 6))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
				 (+ (* i 2) 7) (+ (* i 2) 8)))
			     (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
			     (if (not (eq? args cproc))
			       (begin
				 (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];")
				 (letrec
				   ((load-args
				     (lambda (args more)
				       (if (not (eq? (cdr args) cproc)) (load-args (cdr args) #t))
				       ((cadar args) "c" "rk_continuation[2]" 1)
				       (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[0]), c);")
				       (if more
					 (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))))
				   (load-args args #f))))
			     (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
			     (rp:procedure-epilogue-call (car cproc) outp)
			     pno)))))))
		 (let ((conti (genconti (cdr cproc))))
		   (if (or (eq? (car fun-proc) 'closure-call) (eq? (car (cadr fun-proc)) 'direct))
		     (let ((argno (- (length args) (length cproc))))
		       (list 'proc
			 (lambda ()
			   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ 6 (* argno 2)))
			   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
			   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
			   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
			   (if (zero? argno)
			     (rp:emit-line outp "~T((rk_object *)c)[3] = RK_SOBJ_NIL;")
			     (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];"))
			   (if (eq? (car fun-proc) 'closure-call)
			     (begin
			       (rp:emit-line outp "~T((rk_object *)c)[4] = (rk_object)rk_continuation;")
			       (rp:emit-line outp "~T((rk_object *)c)[5] = RK_DUMMY_OBJ;"))
			     (begin
			       (rp:emit-line outp "~T((rk_object *)c)[4] = RK_DUMMY_OBJ;")
			       (rp:emit-line outp "~T((rk_object *)c)[5] = (rk_object)rk_continuation;")))
			   (if (not (zero? argno))
			     (do ((i 0 (+ i 1)))
			       ((eqv? i (- argno 1))
				(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ (* i 2) 6))
				(rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 7)))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ (* i 2) 6))
			       (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
				 (+ (* i 2) 7) (+ (* i 2) 8))))
			   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
			   (if (not (zero? argno))
			     (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];"))
			   (if (not (eq? (car fun-proc) 'closure-call))
			     (begin
			       ((cadr (cadr fun-proc)) "c" "rk_continuation[2]" 1)
			       (rp:emit-line outp "~TRkWriteCell(&rk_continuation[4], c);")))
			   (if (not (zero? argno))
			     (letrec
			       ((load-args
				 (lambda (args more)
				   (if (not (eq? (cdr args) cproc)) (load-args (cdr args) #t))
				   ((cadar args) "c" "rk_continuation[2]" 1)
				   (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[0]), c);")
				   (if more
				     (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))))
			       (load-args args #f)
			       (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")))
			   (rp:procedure-epilogue-call (car cproc) outp))))
		     (let ((argno (- (length args) (length cproc))) (fconti (rp:procedure-prologue outp)))
		       (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(~A);" (+ 6 (* argno 2)))
		       (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(6, 0);")
		       (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		       (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
		       (if (zero? argno)
			 (rp:emit-line outp "~T((rk_object *)c)[3] = RK_SOBJ_NIL;")
			 (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)&((rk_object *)c)[6];"))
		       (rp:emit-line outp "~T((rk_object *)c)[4] = rk_eval_register[0];")
		       (rp:emit-line outp "~T((rk_object *)c)[5] = rk_continuation[3];")
		       (if (not (zero? argno))
			 (do ((i 0 (+ i 1)))
			   ((eqv? i (- argno 1))
			    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ (* i 2) 6))
			    (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_SOBJ_NIL;" (+ (* i 2) 7)))
			   (rp:emit-line outp "~T((rk_object *)c)[~A] = RK_DUMMY_OBJ;" (+ (* i 2) 6))
			   (rp:emit-line outp "~T((rk_object *)c)[~A] = (rk_object)&((rk_object *)c)[~A];"
			     (+ (* i 2) 7) (+ (* i 2) 8))))
		       (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		       (if (not (zero? argno))
			 (begin
			   (rp:emit-line outp "~Trk_eval_register[0] = (rk_object)&((rk_object *)c)[6];")
			   (letrec
			     ((load-args
			       (lambda (args more)
				 (if (not (eq? (cdr args) cproc)) (load-args (cdr args) #t))
				 ((cadar args) "c" "rk_continuation[2]" 1)
				 (rp:emit-line outp "~TRkWriteCell(&RP_CAR(rk_eval_register[0]), c);")
				 (if more
				   (rp:emit-line outp "~Trk_eval_register[0] = RP_CDR(rk_eval_register[0]);")))))
			     (load-args args #f))))
		       (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
		       (rp:procedure-epilogue-call (car cproc) outp)
		       (list 'proc
			 (lambda ()
			   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
			   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
			   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" fconti)
			   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
			   (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)rk_continuation;")
			   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
			   (rp:procedure-epilogue-call (cadr fun-proc) outp)))))))))))))
    ((sequence)
     (letrec
       ((genconti
	 (lambda (procs)
	   (if (null? (cdr procs))
	     (if (eq? (caar procs) 'direct)
	       (let ((pno (rp:procedure-prologue outp)))
		 ((cadar procs) "rk_eval_register[0]" "rk_continuation[2]" 1)
		 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[3];")
		 (rp:procedure-epilogue-return outp)
		 pno)
	       (let ((pno (rp:procedure-prologue outp)))
		 (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
		 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[3];")
		 (rp:procedure-epilogue-call (car procs) outp)
		 pno))
	     (let ((head-pno (rp:realize-direct-proc (car procs) outp)))
	       (let ((conti (genconti (cdr procs))))
		 (let ((pno (rp:procedure-prologue outp)))
		   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
		   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
		   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
		   (rp:emit-line outp "~T((rk_object *)c)[3] = rk_continuation[3];")
		   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		   (rp:emit-line outp "~Trk_eval_register[0] = rk_continuation[2];")
		   (rp:procedure-epilogue-call-no head-pno outp)
		   pno)))))))
       (let loop ((head-proc (rp:code-to-proc-delayed (car (cadr code)) outp))
		  (next-codes (cdr (cadr code))))
	 (if (null? next-codes) head-proc
	   (if (and (eq? (car head-proc) 'direct) (not (null? (cddr head-proc))))
	     (loop (rp:code-to-proc-delayed (car next-codes) outp) (cdr next-codes))
	     (let ((procs (cons (if (eq? (car head-proc) 'direct) head-proc
				  (let ((pno (rp:procedure-prologue outp))) ((cadr head-proc)) (list 'proc pno)))
				(map (lambda (code) (rp:code-to-proc code outp)) next-codes))))
	       (let ((head-pno (rp:realize-direct-proc (car procs) outp)))
		 (let ((conti (genconti (cdr procs))))
		   (list 'proc
		     (lambda ()
		       (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
		       (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
		       (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		       (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
		       (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)rk_continuation;")
		       (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		       (rp:procedure-epilogue-call-no head-pno outp)))))))))))
    ((if)
     (let ((test-proc (rp:code-to-proc (cadr code) outp))
	   (then-proc (rp:code-to-proc (caddr code) outp))
	   (else-proc (rp:code-to-proc (cadddr code) outp)))
       (if (eq? (car test-proc) 'direct)
	 (if (and (eq? (car then-proc) 'direct) (eq? (car else-proc) 'direct))
	   (list 'direct
	     (lambda (store env vreg)
	       ((cadr test-proc) "c" env vreg)
	       (rp:emit-line outp "~Tif (c == RK_SOBJ_FALSE) {")
	       ((cadr else-proc) store env vreg)
	       (rp:emit-line outp "~T} else {")
	       ((cadr then-proc) store env vreg)
	       (rp:emit-line outp "~T}")))
	   (list 'proc
	     (lambda ()
	       ((cadr test-proc) "c" "rk_eval_register[0]" 1)
	       (rp:emit-line outp "~Tif (c == RK_SOBJ_FALSE) {")
	       (if (eq? (car else-proc) 'direct)
		 (begin
		   ((cadr else-proc) "rk_eval_register[0]" "rk_eval_register[0]" 1)
		   (rp:emit-line outp "~T~TRP_RETURN();"))
		 (rp:emit-line outp "~T~Treturn rp_c_~A;" (cadr else-proc)))
	       (rp:emit-line outp "~T} else {")
	       (if (eq? (car then-proc) 'direct)
		 (begin
		   ((cadr then-proc) "rk_eval_register[0]" "rk_eval_register[0]" 1)
		   (rp:emit-line outp "~T~TRP_RETURN();"))
		 (rp:emit-line outp "~T~Treturn rp_c_~A;" (cadr then-proc)))
	       (rp:emit-line outp "~T}")
	       (rp:emit-line outp "}"))))
	 (let ((conti (rp:procedure-prologue outp)))
	   (rp:emit-line outp "~Tif (rk_eval_register[0] == RK_SOBJ_FALSE) {")
	   (if (eq? (car else-proc) 'direct)
	     (begin
	       ((cadr else-proc) "rk_eval_register[0]" "rk_continuation[2]" 1)
	       (rp:emit-line outp "~T~Trk_continuation = (rk_object *)rk_continuation[3];")
	       (rp:emit-line outp "~T~TRP_RETURN();"))
	     (begin
	       (rp:emit-line outp "~T~Trk_eval_register[0] = rk_continuation[2];")
	       (rp:emit-line outp "~T~Trk_continuation = (rk_object *)rk_continuation[3];")
	       (rp:emit-line outp "~T~Treturn rp_c_~A;" (cadr else-proc))))
	   (rp:emit-line outp "~T} else {")
	   (if (eq? (car then-proc) 'direct)
	     (begin
	       ((cadr then-proc) "rk_eval_register[0]" "rk_continuation[2]" 1)
	       (rp:emit-line outp "~T~Trk_continuation = (rk_object *)rk_continuation[3];")
	       (rp:emit-line outp "~T~TRP_RETURN();"))
	     (begin
	       (rp:emit-line outp "~T~Trk_eval_register[0] = rk_continuation[2];")
	       (rp:emit-line outp "~T~Trk_continuation = (rk_object *)rk_continuation[3];")
	       (rp:emit-line outp "~T~Treturn rp_c_~A;" (cadr then-proc))))
	   (rp:emit-line outp "~T}")
	   (rp:emit-line outp "}")
	   (list 'proc
	     (lambda ()
	       (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
	       (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
	       (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
	       (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
	       (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)rk_continuation;")
	       (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
	       (rp:procedure-epilogue-call test-proc outp)))))))
    ((setbang)
     (let* ((vcell (cadr code))
	    (value-proc
	     (if (not (and (eq? (car vcell) 'local) (not (eq? (cadr (caddr vcell)) 'var))))
	       (rp:code-to-proc (caddr code) outp))))
       (let
	 ((assign-value
	   (lambda (val env)
	     (if (eq? (car vcell) 'global)
	       (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rp_refarray[~A])[1], ~A);" (caddr (cadr vcell)) val)
	       (letrec
		 ((access-env
		   (lambda (d)
		     (rp:emit-code outp "((rk_object *)~V)"
		       (lambda ()
			 (if (zero? d)
			   (rp:emit-code outp "~A" env)
			   (rp:emit-code outp "~V[1]" (lambda () (access-env (- d 1))))))))))
		 (rp:emit-line outp "~TRkWriteCell(&~V[~A], ~A);"
		   (lambda () (access-env (cadr vcell))) (+ (caddr (caddr vcell)) 2) val))))))
	 (cond
	   ((and (eq? (car vcell) 'local) (not (eq? (cadr (caddr vcell)) 'var)))
	    (list 'direct (lambda (store env vreg) (rp:emit-line outp "~T~A = RK_SOBJ_UNSPEC;" store)) 'void))
	   ((eq? (car value-proc) 'direct)
	    (list 'proc
	      (lambda ()
		((cadr value-proc) "c" "rk_eval_register[0]" 1)
		(assign-value "c" "rk_eval_register[0]")
		(rp:emit-line outp "~Trk_eval_register[0] = RK_SOBJ_UNSPEC;")
		(rp:procedure-epilogue-return outp))))
	   (else
	    (let ((conti (rp:procedure-prologue outp)))
	      (assign-value "rk_eval_register[0]" "rk_continuation[2]")
	      (rp:emit-line outp "~Trk_eval_register[0] = RK_SOBJ_UNSPEC;")
	      (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[3];")
	      (rp:procedure-epilogue-return outp)
	      (list 'proc
		(lambda ()
		  (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
		  (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
		  (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		  (rp:emit-line outp "~T((rk_object *)c)[2] = rk_eval_register[0];")
		  (rp:emit-line outp "~T((rk_object *)c)[3] = (rk_object)rk_continuation;")
		  (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		  (rp:procedure-epilogue-call value-proc outp)))))))))))

(define (rp:generate-proc exp env outp)
  (rp:code-to-proc (rp:exp-to-objcode exp env) outp))

(define (rp:compile-expression exp forms outp file)
  (rp:catch-error (rp:error-reporter file exp)
    (let ((exp (rp:expand-macro exp)))
      (cond ((and (pair? exp) (eq? (car exp) 'begin))
	     (let loop ((exp-list (cdr exp)) (forms forms))
	       (if (null? exp-list) forms
		 (loop (cdr exp-list) (rp:compile-expression (car exp-list) forms outp file)))))
	    ((and (pair? exp) (eq? (car exp) 'rp:define))
	     (rp:primitive-syntax (and (symbol? (cadr exp)) (null? (cdddr exp))) exp)
	     (set-cdr! forms
	       (cons (list 'define (rp:register-literal (cadr exp)) (rp:generate-proc (caddr exp) '() outp)) '()))
	     (cdr forms))
	    ((and (pair? exp) (eq? (rp:expand-macro (car exp)) 'rp:eval-in-compiler-environment))
	     (rp:primitive-syntax (null? (cddr exp)) exp)
	     (rp:call-evaluator (cons (cadr exp) '()) (rp:top-level-environment))
	     forms)
	    (else
	     (set-cdr! forms (cons (list 'eval (rp:generate-proc exp '() outp)) '()))
	     (cdr forms))))))

(define (rp:compile file forms outp)
  (let ((inp (open-input-file file)))
    (rp:catch-error (rp:error-reporter file #f)
      (let loop ((forms forms))
	(let ((exp (read inp)))
	  (if (eof-object? exp) forms
	    (loop (rp:compile-expression (rp:expand-syntax exp) forms outp file))))))))

(define (rp:module-code forms outp)
  (letrec
    ((do-directs
      (lambda (head cproc def)
	(if def (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rp_refarray[~A])[1], rk_eval_register[0]);" def))
	(do ((proc head (cdr proc))) ((eq? proc cproc))
	  (if (eq? (caar proc) 'define)
	    (begin
	      ((cadr (caddar proc)) "c" "RK_DUMMY_OBJ" 0)
	      (rp:emit-line outp "~TRkWriteCell(&((rk_object *)rp_refarray[~A])[1], c);" (caddr (cadar proc))))
	    ((cadr (cadar proc)) "c" "RK_DUMMY_OBJ" 0)))))
     (gen-proc
      (lambda (head cproc def)
	(cond ((null? cproc)
	       (let ((pno (rp:procedure-prologue outp)))
		 (do-directs head cproc def)
		 (rp:emit-line outp "~Trk_continuation = (rk_object *)rk_continuation[2];")
		 (rp:procedure-epilogue-return outp)
		 pno))
	      ((eq? (car (if (eq? (caar cproc) 'define) (caddar cproc) (cadar cproc))) 'direct)
	       (gen-proc head (cdr cproc) def))
	      (else
	       (let ((conti
		      (gen-proc (cdr cproc) (cdr cproc) (if (eq? (caar cproc) 'define) (caddr (cadar cproc)) #f))))
		 (let ((pno (rp:procedure-prologue outp)))
		   (do-directs head cproc def)
		   (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
		   (rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
		   (rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
		   (rp:emit-line outp "~T((rk_object *)c)[2] = rk_continuation[2];")
		   (rp:emit-line outp "~T((rk_object *)c)[3] = RK_DUMMY_OBJ;")
		   (rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
		   (rp:emit-line outp "~Trk_eval_register[0] = RK_DUMMY_OBJ;")
		   (rp:emit-line outp "~Trk_valid_register = 1;")
		   (rp:procedure-epilogue-call (if (eq? (caar cproc) 'define) (caddar cproc) (cadar cproc)) outp)
		   pno)))))))
    (let ((conti (gen-proc forms forms #f)))
      (let ((pno (rp:procedure-prologue outp)))
	(rp:emit-line outp "~Tc = (rk_object)RkAllocCells(4);")
	(rp:emit-line outp "~T((rk_object *)c)[0] = RK_VECTOR_TAG(4, 0);")
	(rp:emit-line outp "~T((rk_object *)c)[1] = rp_c_~A;" conti)
	(rp:emit-line outp "~T((rk_object *)c)[2] = (rk_object)rk_continuation;")
	(rp:emit-line outp "~T((rk_object *)c)[3] = RK_DUMMY_OBJ;")
	(rp:emit-line outp "~Trk_continuation = (rk_object *)c;")
	(rp:procedure-epilogue-return outp)
	(newline outp)
	(rp:emit-line outp "rk_object RP_MODEXPORT RpC~ARun(void) { return rp_c_~A; }" rp:*module-id* pno)))))

(define rp:*odesc-index* 0)

(define (rp:new-object-no)
  (let ((ono rp:*odesc-index*))
    (set! rp:*odesc-index* (+ ono 1))
    ono))

(define (rp:dump-string str outp)
  (let ((n (string-length str)))
    (do ((i 0 (+ i 1))) ((eqv? i n))
      (let ((c (char->integer (string-ref str i))))
	(if (and (<= #o40 c #o176) (not (memv c '(#o42 #o77 #o134)))) (display (integer->char c) outp)
	  (rp:emit-code outp "\\~A~A~A" (quotient c 64) (remainder (quotient c 8) 8) (remainder c 8)))))))

(define (rp:make-object-data obj aux outp)
  (cond ((symbol? obj)
	 (if (eq? obj (string->symbol (symbol->string obj)))
	   (let ((ono (rp:new-object-no)))
	     (newline outp)
	     (rp:emit-line outp "static struct RP_STRING_DESC const rp_c_o_~A[] = {{~A, \"~V\"}};" ono
	       (string-length (symbol->string obj)) (lambda () (rp:dump-string (symbol->string obj) outp)))
	     (list 'symbol ono))
	   (list 'gensym)))
	((pair? obj)
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static int const rp_c_o_~A[] = {~A, ~A};" ono (car aux) (cadr aux))
	   (list 'pair ono)))
	((null? obj) (list 'null))
	((string? obj)
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static struct RP_STRING_DESC const rp_c_o_~A[] = {{~A, \"~V\"}};" ono
	     (string-length obj) (lambda () (rp:dump-string obj outp)))
	   (list 'string ono)))
	((boolean? obj) (list 'boolean (if obj 1 0)))
	((char? obj) (list 'character (char->integer obj)))
	((vector? obj)
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static int const rp_c_o_~A[] = {~A," ono (vector-length obj))
	   (for-each (lambda (n) (rp:emit-line outp "~T~A," n)) aux)
	   (rp:emit-line outp "};")
	   (list 'vector ono)))
	((and (number? obj) (real? obj) (exact? obj) (integer? obj) (<= #x-20000000 obj #x1fffffff))
	 (list 'shortint obj))
	((and (number? obj) (real? obj) (exact? obj) (integer? obj) (not (<= #x-20000000 obj #x1fffffff)))
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static int const rp_c_o_~A[] = {" ono)
	   (do ((n (if (positive? obj)
		     (begin (rp:emit-line outp "~T1,") obj)
		     (begin (rp:emit-line outp "~T0,") (- obj)))
		   (quotient n #x40000000)))
	       ((zero? n))
	     (rp:emit-line outp "~T~A," (remainder n #x40000000)))
	   (rp:emit-line outp "~T-1};")
	   (list 'bigint ono)))
	((and (number? obj) (real? obj) (exact? obj) (not (integer? obj)))
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static int const rp_c_o_~A[] = {~A, ~A, ~A};" ono (car aux) (cadr aux) (caddr aux))
	   (list 'fraction ono)))
	((and (number? obj) (real? obj) (inexact? obj))
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static double const rp_c_o_~A[] = {~A};" ono obj)
	   (list 'dblfloat ono)))
	((and (number? obj) (not (real? obj)))
	 (let ((ono (rp:new-object-no)))
	   (newline outp)
	   (rp:emit-line outp "static int const rp_c_o_~A[] = {~A, ~A};" ono (car aux) (cadr aux))
	   (list 'complex ono)))
	((eq? obj (rp:syntax-mark)) (list 'synmark))
	(else (rp:cause-error "Illegal literal object" #f))))

(define (rp:output-object-data data outp)
  (case (car data)
    ((symbol) (rp:emit-line outp "~TRP_OTAG_SYMBOL, rp_c_o_~A," (cadr data)))
    ((gensym) (rp:emit-line outp "~TRP_OTAG_GENSYM, NULL,"))
    ((pair) (rp:emit-line outp "~TRP_OTAG_PAIR, rp_c_o_~A," (cadr data)))
    ((null) (rp:emit-line outp "~TRP_OTAG_NULL, NULL,"))
    ((string) (rp:emit-line outp "~TRP_OTAG_STRING, rp_c_o_~A," (cadr data)))
    ((boolean) (rp:emit-line outp "~TRP_OTAG_BOOLEAN, (void const *)~A," (cadr data)))
    ((character) (rp:emit-line outp "~TRP_OTAG_CHARACTER, (void const *)~A," (cadr data)))
    ((vector) (rp:emit-line outp "~TRP_OTAG_VECTOR, rp_c_o_~A," (cadr data)))
    ((shortint) (rp:emit-line outp "~TRP_OTAG_SHORTINT, (void const *)~A," (cadr data)))
    ((bigint) (rp:emit-line outp "~TRP_OTAG_BIGINT, rp_c_o_~A," (cadr data)))
    ((fraction) (rp:emit-line outp "~TRP_OTAG_FRACTION, rp_c_o_~A," (cadr data)))
    ((dblfloat) (rp:emit-line outp "~TRP_OTAG_DBLFLOAT, rp_c_o_~A," (cadr data)))
    ((complex) (rp:emit-line outp "~TRP_OTAG_COMPLEX, rp_c_o_~A," (cadr data)))
    ((synmark) (rp:emit-line outp "~TRP_OTAG_SYNMARK, NULL,"))))

(define (rp:initializor outp)
  (newline outp)
  (rp:emit-line outp "static struct RP_PROCEDURE_REC const rp_c_procedure_rec[] = {")
  (do ((i 0 (+ i 1))) ((eqv? i rp:*procedure-index*)) (rp:emit-line outp "~TRpC~A, &rp_c_~A," i i))
  (rp:emit-line outp "~TNULL, NULL};")
  (let ((object-data
	 (map (lambda (desc) (rp:make-object-data (cadr desc) (cdddr desc) outp)) (cdr rp:*toplevel-environment*))))
    (newline outp)
    (rp:emit-line outp "static struct RP_OBJECT_DESC const rp_c_object_desc[] = {")
    (for-each (lambda (data) (rp:output-object-data data outp)) object-data)
    (rp:emit-line outp "~T-1, NULL};")
    (newline outp)
    (rp:emit-line outp "static rk_object rp_refarray[~A];" (length object-data)))
  (newline outp)
  (rp:emit-line outp "static int loaded = 0;")
  (newline outp)
  (rp:emit-line outp "void RP_MODEXPORT RpC~AInit(struct RP_MODULE_INIT *p) {" rp:*module-id*)
  (rp:emit-line outp "~Tp->rp_nprocs = sizeof(rp_c_procedure_rec)/sizeof(rp_c_procedure_rec[0]) - 1;")
  (rp:emit-line outp "~Tp->rp_procs = rp_c_procedure_rec;")
  (rp:emit-line outp "~Tp->rp_odesc = rp_c_object_desc;")
  (rp:emit-line outp "~Tp->rp_oarray = rp_refarray;")
  (rp:emit-line outp "~Tp->rp_version = 1;")
  (rp:emit-line outp "~Tp->rp_loaded = &loaded;")
  (rp:emit-line outp "~Tp->rp_mname = \"~A\";" rp:*module-id*)
  (rp:emit-line outp "}"))

(rp:catch-error (rp:error-reporter '(command line) #f)
  (begin
    (do ((pf (reverse rp:*preload-files*) (cdr pf))) ((null? pf))
      (if (string? (car pf)) (load (car pf))
	(do ((exp (read) (read))) ((eof-object? exp)) (eval exp))))
    (call-with-output-file rp:*output-file*
      (lambda (outp)
	(rp:emit-line outp "/* Generated by $Id: pisc.scm,v 1.8 2004/08/06 05:48:06 qfwfq Exp $ */")
	(rp:emit-line outp "#include \"rhiz_pi.h\"")
	(rp:emit-line outp "RP_ARRAY_FORWARD_DECL rk_object rp_refarray[];")
	(let ((forms (cons #t '())))
	  (rp:compile rp:*input-file* forms outp)
	  (rp:module-code (cdr forms) outp)
	  (rp:initializor outp))))))

(exit 0)
