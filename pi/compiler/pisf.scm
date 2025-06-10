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

;;;; @(#)$Id: pisf.scm,v 1.4 2004/08/06 05:48:06 qfwfq Exp $
; $Log: pisf.scm,v $
; Revision 1.4  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.3  1999/06/29 07:49:05  qfwfq
; Check duplicate module loading.
;
; Revision 1.2  1999/06/15 07:36:00  qfwfq
; Various shared library arrangement
;
; Revision 1.1  1998/07/31 11:17:46  qfwfq
; Fasloader and WW library
;

(define (rp:usage)
  (display "$Id: pisf.scm,v 1.4 2004/08/06 05:48:06 qfwfq Exp $") (newline)
  (display "usage: ")
  (display (rp:basename (car *invocation-arg*) #f))
  (display " [-exec <interpreter>|-module <identifier>] [-output <filename>]")
  (display " {-mpath <dir>}... {-load <filename>}... filename")
  (newline)
  (exit 2))

(define rp:*prefix* #t)
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
	    ((string=? arg "-exec")
	     (set! rp:*prefix* (car args)) (loop (cdr args)))
	    ((string=? arg "-module")
	     (set! rp:*prefix* #f) (set! rp:*module-id* (car args)) (loop (cdr args)))
	    ((string=? arg "-output")
	     (set! rp:*output-file* (car args)) (loop (cdr args)))
	    ((string=? arg "-mpath")
	     (set! rp:*additional-macro-path* (cons (car args) rp:*additional-macro-path*))
	     (loop (cdr args)))
	    ((string=? arg "-load")
	     (set! rp:*preload-files* (cons (if (string=? (car args) "-") 'stdin (car args)) rp:*preload-files*))
	     (loop (cdr args)))
	    (else (rp:usage))))))

(if (not rp:*output-file*) (set! rp:*output-file*
			     (cond ((eq? rp:*prefix* #t) (string-append (rp:basename rp:*input-file* ".scm") ".scf"))
				   ((not rp:*prefix*) (string-append rp:*module-id* ".c"))
				   (else (let ((base (rp:basename rp:*input-file* ".scm"))
					       (file (rp:basename rp:*input-file* #f)))
					   (if (string=? file base) (string-append base ".sh") base))))))
(set! rp:*macro-search-list*
  (cons (car rp:*macro-search-list*) (append (reverse rp:*additional-macro-path*) (cdr rp:*macro-search-list*))))

(define (rp:error-reporter file)
  (lambda (err obj)
    (display (rp:basename (car *invocation-arg*) #f))
    (display ": in ") (write file) (display #\:) (newline)
    (rp:print-error-message err obj) (newline) (exit 1)))

(define (rp:primitive-syntax check exp)
  (if (not check) (rp:cause-error "Error in primitive syntax" exp)))

(define (rp:expand-macro exp)
  (if (and (pair? exp) (eq? (car exp) 'rp:resolvant))
    (rp:expand-macro (rp:resolve-syntax (cdr exp)))
    exp))

(define rp:*quote-expressions* '())
(define rp:*constant-elements* '())
(define rp:*n-constant-roots* 0)
(define rp:*constant-roots* '())
(define rp:*n-constant-brahches* 0)
(define rp:*constant-brahches* '())
(define rp:*n-collected-consts* 0)
(define rp:*collected-consts* '())

(define (rp:register-constant obj)
  (define (new-element type . elts)
    (let ((c `(,obj 0 ,type ,@elts)))
      (set! rp:*constant-elements* (cons c rp:*constant-elements*))
      c))
  (cond ((assoc obj rp:*constant-elements*) =>
	 (lambda (c) (set-car! (cdr c) (+ (cadr c) 1)) c))
	((pair? obj)
	 (new-element 'pair (rp:register-constant (car obj)) (rp:register-constant (cdr obj))))
	((vector? obj)
	 (let ((n (vector-length obj)))
	   (let ((e (make-vector n)))
	     (do ((i 0 (+ i 1))) ((= i n))
	       (vector-set! e i (rp:register-constant (vector-ref obj i))))
	     (new-element 'vector e))))
	(else (new-element 'atomic))))

(define rp:collect-constant
  (letrec ((no-common?
	    (lambda (vald)
	      (cond ((memv (caddr vald) '(atomic regatom)) #t)
		    ((< 0 (cadr vald)) #f)
		    (else (no-common-sub? vald)))))
	   (no-common-sub?
	    (lambda (vald)
	      (case (caddr vald)
		((pair) (and (no-common? (cadddr vald)) (no-common? (car (cddddr vald)))))
		((vector)
		 (let ((v (cadddr vald)))
		   (let ((n (vector-length v)))
		     (let loop ((i 0)) (if (= i n) #t (and (no-common? (vector-ref v i)) (loop (+ i 1))))))))
		(else #f))))
	   (register-root
	    (lambda (vald tag)
	      (let ((j rp:*n-constant-roots*))
		(set! rp:*constant-roots* (cons (car vald) rp:*constant-roots*))
		(set! rp:*n-constant-roots* (+ j 1))
		(set-cdr! (cdr vald) `(,tag ,j))
		j)))
	   (register-branch
	    (lambda (vald elt)
	      (let ((j rp:*n-constant-brahches*))
		(set! rp:*constant-brahches* (cons elt rp:*constant-brahches*))
		(set! rp:*n-constant-brahches* (+ j 1))
		(let ((j (- -1 j)))
		  (set-cdr! (cdr vald) `(registered ,j))
		  j))))
	   (create-entry
	    (lambda (vald)
	      (cond ((eq? (caddr vald) 'atomic) (register-root vald 'regatom))
		    ((no-common-sub? vald) (register-root vald 'registered))
		    ((eq? (caddr vald) 'pair)
		     (do ((ja (create-entry (cadddr vald)) (create-entry (cadddr d)))
			  (d (car (cddddr vald)) (car (cddddr d)))
			  (l '() (cons ja l)))
			 ((not (and (zero? (cadr d)) (eq? (caddr d) 'pair)))
			  (register-branch vald `(l ,(create-entry d) ,ja ,@l)))))
		    ((eq? (caddr vald) 'vector)
		     (let ((v (cadddr vald)))
		       (let ((n (vector-length v)))
			 (do ((i (- n 1) (- i 1)) (l '() (cons (create-entry (vector-ref v i)) l)))
			     ((< i 0) (register-branch vald (cons 'v l)))))))
		    (else (cadddr vald)))))
	   (collect
	    (lambda (vald)
	      (if (no-common? vald) #f
		(let ((j (create-entry vald)))
		  (cond ((memv j rp:*collected-consts*) => (lambda (l) (length (cdr l))))
			(else (let ((k rp:*n-collected-consts*))
				(set! rp:*collected-consts* (cons j rp:*collected-consts*))
				(set! rp:*n-collected-consts* (+ k 1))
				k))))))))
    (lambda (mname expcell valdesc)
      (cond ((collect valdesc) =>
	     (lambda (k) (set-car! expcell mname)
			 (set-cdr! expcell (cons k '()))))))))

(define (rp:make-constant-descs)
  (let ((adjust-ref (lambda (k) (if (<= 0 k) k (- rp:*n-constant-roots* k 1))))
	(v (make-vector (+ rp:*n-constant-roots* rp:*n-constant-brahches*))))
    (do ((i (- rp:*n-constant-roots* 1) (- i 1)) (l rp:*constant-roots* (cdr l))) ((null? l))
      (vector-set! v i (car l)))
    (do ((i (+ rp:*n-constant-roots* rp:*n-constant-brahches* -1) (- i 1))
	 (l rp:*constant-brahches* (cdr l)))
	((null? l))
      (vector-set! v i
	(let ((desc (car l)))
	  (case (car desc)
	    ((l) (cons 'l (map adjust-ref (cdr desc))))
	    ((v) (cons 'v (list->vector (map adjust-ref (cdr desc)))))))))
    v))

(define (rp:make-constant-leaves)
  (let ((v (make-vector rp:*n-collected-consts*)))
    (do ((i (- rp:*n-collected-consts* 1) (- i 1)) (l rp:*collected-consts* (cdr l))) ((null? l))
      (vector-set! v i (let ((k (car l))) (if (<= 0 k) k (- rp:*n-constant-roots* k 1)))))
    v))

(define (rp:expand-expression exp)
  (let ((exp (rp:expand-macro exp)))
    (if (not (pair? exp)) exp
      (case (car exp)
	((quote)
	 (rp:primitive-syntax (null? (cddr exp)) exp)
	 (let ((val (cadr exp)))
	   (let ((rcell `(quote ,val)))
	     (if (or (pair? val) (vector? val))
	       (set! rp:*quote-expressions* (cons (cons rcell (rp:register-constant val)) rp:*quote-expressions*)))
	     rcell)))
	((rp:lambda) `(rp:lambda ,(cadr exp) ,@(map rp:expand-expression (cddr exp))))
	((if) `(if ,@(map rp:expand-expression (cdr exp))))
	((set! rp:define)
	 (rp:primitive-syntax (null? (cdddr exp)) exp)
	 `(,(car exp) ,(cadr exp) ,(rp:expand-expression (caddr exp))))
	((begin)
	 (if (null? (cddr exp))
	   (rp:expand-expression (cadr exp))
	   `(begin ,@(map rp:expand-expression (cdr exp)))))
	(else (map rp:expand-expression exp))))))

(define (rp:load-expression exp forms)
  (let ((exp (rp:expand-macro exp)))
    (cond ((and (pair? exp) (eq? (car exp) 'begin))
	   (let loop ((exp-list (cdr exp)) (forms forms))
	     (if (null? exp-list) forms
	       (loop (cdr exp-list) (rp:load-expression (car exp-list) forms)))))
	  ((and (pair? exp) (eq? (rp:expand-macro (car exp)) 'rp:eval-in-compiler-environment))
	   (rp:primitive-syntax (null? (cddr exp)) exp)
	   (rp:call-evaluator (cons (cadr exp) '()) (rp:top-level-environment))
	   forms)
	  (else
	   (set-cdr! forms (cons (rp:expand-expression exp) '()))
	   (cdr forms)))))

(define (rp:load-source file forms)
  (let ((inp (open-input-file file)))
    (rp:catch-error (rp:error-reporter file)
      (let loop ((forms forms))
	(let ((exp (read inp)))
	  (if (not (eof-object? exp))
	    (loop (rp:load-expression (rp:expand-syntax exp) forms)))))))
  (if (null? (cdr forms)) (rp:cause-error (string-append "No forms in source file " file) #f)))

(define (rp:emit-c-prologue outp)
  (rp:emit-line outp "/* Generated by $Id: pisf.scm,v 1.4 2004/08/06 05:48:06 qfwfq Exp $ */")
  (rp:emit-line outp "#include \"rhiz_pi.h\"")
  (newline outp)
  (rp:emit-line outp "static char const rp_f_str[] =")
  (write-char #\" outp))

(define (rp:emit-c-epilogue outp)
  (rp:emit-line outp "\";")
  (newline outp)
  (rp:emit-line outp "static rk_object rp_c_0;")
  (rp:emit-line outp "static rk_object RpC0(void) {")
  (rp:emit-line outp "~Trk_object c;")
  (rp:emit-line outp "~Tc = (rk_object)RkAllocCells(2);")
  (rp:emit-line outp "~T((rk_object *)c)[0] = RK_MAKEINUM((unsigned long)rp_f_str & 0xffff);")
  (rp:emit-line outp "~T((rk_object *)c)[1] = RK_MAKEINUM((unsigned long)rp_f_str >> 16);")
  (rp:emit-line outp "~Trk_eval_register[0] = c;")
  (rp:emit-line outp "~Trk_valid_register = 1;")
  (rp:emit-line outp "~Treturn rp_run_faslcode_proc;")
  (rp:emit-line outp "}")
  (newline outp)
  (rp:emit-line outp "rk_object RP_MODEXPORT RpC~ARun(void) { return rp_c_0; }" rp:*module-id*)
  (newline outp)
  (rp:emit-line outp "static struct RP_PROCEDURE_REC const rp_c_procedure_rec[] = {RpC0, &rp_c_0, NULL, NULL};")
  (newline outp)
  (rp:emit-line outp "static struct RP_OBJECT_DESC const rp_c_object_desc[] = {-1, NULL};")
  (newline outp)
  (rp:emit-line outp "static int loaded = 0;")
  (newline outp)
  (rp:emit-line outp "void RP_MODEXPORT RpC~AInit(struct RP_MODULE_INIT *p) {" rp:*module-id*)
  (rp:emit-line outp "~Tp->rp_nprocs = sizeof(rp_c_procedure_rec)/sizeof(rp_c_procedure_rec[0]) - 1;")
  (rp:emit-line outp "~Tp->rp_procs = rp_c_procedure_rec;")
  (rp:emit-line outp "~Tp->rp_odesc = rp_c_object_desc;")
  (rp:emit-line outp "~Tp->rp_oarray = NULL;")
  (rp:emit-line outp "~Tp->rp_version = 1;")
  (rp:emit-line outp "~Tp->rp_loaded = &loaded;")
  (rp:emit-line outp "~Tp->rp_mname = \"~A\";" rp:*module-id*)
  (rp:emit-line outp "}"))

(rp:catch-error (rp:error-reporter '(command-line))
  (begin
    (do ((pf (reverse rp:*preload-files*) (cdr pf))) ((null? pf))
      (if (string? (car pf)) (load (car pf))
	(do ((exp (read) (read))) ((eof-object? exp)) (eval exp))))
    (let ((forms (cons 'rp:raw-expressions '())))
      (rp:load-source rp:*input-file* forms)
      (let ((mname (gensym)))
	(do ((qes rp:*quote-expressions* (cdr qes))) ((null? qes))
	  (rp:collect-constant mname (caar qes) (cdar qes)))
	(if (< 0 rp:*n-collected-consts*)
	  (set-cdr! forms
	    (cons `(rp:define ,mname
		     (rp:fas-constants ',(rp:make-constant-descs) ,rp:*n-constant-roots* ',(rp:make-constant-leaves)))
		  (cdr forms)))))
      (call-with-output-file rp:*output-file*
	(lambda (outp)
	  (if (not rp:*prefix*) (rp:emit-c-prologue outp))
	  (rp:fas-write forms (lambda (c) (write-char c outp)) rp:*prefix*
	    (if rp:*prefix* #f (lambda () (display "\\n\"" outp) (newline outp) (write-char #\" outp))))
	  (if rp:*prefix* (newline outp) (rp:emit-c-epilogue outp)))))))

(exit 0)
