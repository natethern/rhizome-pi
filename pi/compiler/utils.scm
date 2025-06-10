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

;;;; @(#)$Id: utils.scm,v 1.4 2004/08/06 05:48:06 qfwfq Exp $
; $Log: utils.scm,v $
; Revision 1.4  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.3  1997/05/12 07:21:29  qfwfq
; version 0.31 - some enhancements on error handling etc.
;
; Revision 1.2  1996/10/10 08:27:10  qfwfq
; Ported to Win32 environment.
;
; Revision 1.1  1996/09/06 06:12:41  qfwfq
; Version 0.20 unix revision is up.
;

(define (rp:basename name suffix)
  (let
    ((rindex
       (lambda (string char)
	 (let loop ((i (- (string-length string) 1)))
	   (cond ((< i 0) #f)
		 ((char=? (string-ref string i) char) i)
		 (else (loop (- i 1))))))))
    (let ((slash (rindex name (cm-path-separate-char))) (len (string-length name)))
      (let ((blen (if suffix (- len (string-length suffix)) len)))
	(substring name
	  (if slash (+ slash 1) 0)
	  (if (and suffix (< 0 blen) (string=? suffix (substring name blen len)))
	    blen len))))))

(define (rp:write-error-exp exp . port)
  (define (wrt exp) (apply write exp port))
  (define (dpl exp) (apply display exp port))
  (define (plist c l)
    (cond ((null? l) (dpl #\)))
	  ((pair? l) (dpl c) (pobj (car l)) (plist #\space (cdr l)))
	  (else (dpl " . ") (wrt l) (dpl #\)))))
  (define (pobj exp)
    (cond ((and (pair? exp) (eq? (car exp) 'rp:resolvant))
	   (dpl #\[) (wrt (syntax-object->datum (cadr exp))) (dpl #\]))
	  ((pair? exp) (plist #\( exp))
	  (else (wrt exp))))
  (pobj exp))

(define (rp:cause-error mes exp)
  (rp:error 109
    (list 'compile
      (lambda (p mes exp)
	(display mes p) (if exp (begin (display ": " p) (rp:write-error-exp exp p))))
      mes exp)))

(define rp:emit-code
  (let ((tab-char (integer->char 9)))
    (lambda (port format . args)
      (let ((n (string-length format))
	    (format-error (lambda () (rp:cause-error "Internal: format string error" format)))
	    (get-arg
	      (lambda ()
		(if (null? args) (rp:cause-error "Internal: to few arguments for format string" format))
		(let ((val (car args))) (set! args (cdr args)) val))))
	(do ((i 0 (+ i 1)))
	  ((eqv? i n)
	   (if (not (null? args)) (rp:cause-error "Internal: to many arguments for fomat string" format)))
	  (let ((c (string-ref format i)))
	    (if (not (eqv? c #\~)) (display c port)
	      (begin
		(set! i (+ i 1))
		(if (eqv? i n) (format-error))
		(case (string-ref format i)
		  ((#\T) (display tab-char port))
		  ((#\~) (display #\~ port))
		  ((#\A) (display (get-arg) port))
		  ((#\V) ((get-arg)))
		  (else (format-error)))))))))))

(define (rp:emit-line port format . args)
  (apply rp:emit-code port format args) (newline port))
