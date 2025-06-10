; Copyright (c) 1998-99 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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

;;;; @(#)$Id: wwutils.scm,v 1.3 2004/08/06 05:48:06 qfwfq Exp $
; $Log: wwutils.scm,v $
; Revision 1.3  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.2  1999/02/15 08:14:39  qfwfq
; API call argument check
;
; Revision 1.1  1998/07/31 11:34:38  qfwfq
; First release of ww library
;

(define (rp:ww-dispatch-make-table size)
  (let ((s (do ((i 1 (rp:* i 2))) ((rp:<= size i) i))))
    (rp:vector s 0 0 (rp:make-vector s '()))))

(define (rp:ww-dispatch-install-handler table key proc)
  (define (update-stat n)
    (rp:vector-set! table 1 (rp:+ (rp:vector-ref table 1) 1))
    (if (rp:< (rp:vector-ref table 2) n)
      (rp:vector-set! table 2 n)))
  (let ((index (rp:remainder key (rp:vector-ref table 0)))
	(v (rp:vector-ref table 3)))
    (let ((l (rp:vector-ref v index)))
      (if (rp:null? l)
	(begin (rp:vector-set! v index (rp:cons (rp:cons key proc) '()))
	       (update-stat 1))
	(let ((p (rp:assv key l)))
	  (if p (rp:set-cdr! p proc)
	    (do ((n 2 (rp:+ n 1)) (l l (rp:cdr l)))
	      ((rp:null? (rp:cdr l))
	       (rp:set-cdr! l (rp:cons (rp:cons key proc) '()))
	       (update-stat n)))))))))

(define (rp:ww-dispatch-copy-table src dst)
  (let ((v (rp:vector-ref src 3)) (s (rp:vector-ref src 0)))
    (do ((i 0 (rp:+ i 1))) ((rp:= i s))
      (do ((l (rp:vector-ref v i) (rp:cdr l))) ((rp:null? l))
	(let ((p (rp:car l)))
	  (if (rp:cdr p) (rp:ww-dispatch-install-handler dst (rp:car p) (rp:cdr p))))))))

(define (rp:ww-dispatch-get-handler table key default)
  (let ((p (rp:assv key (rp:vector-ref (rp:vector-ref table 3) (rp:remainder key (rp:vector-ref table 0))))))
    (if p (let ((proc (rp:cdr p))) (if proc proc default)) default)))

(rp:define-generic (rp:ww-register-get-obj this index))
(rp:define-generic (rp:ww-register-insert-obj this obj))
(rp:define-generic (rp:ww-register-delete-obj this index))

(define (rp:ww-register-create)
  (let ((table (rp:make-vector 1 -1))
	(table-size 1) (expand-size 1) (next-size 2) (free-entry 0))
    (rp:object-constructor ()
      ((rp:ww-register-get-obj this index) (rp:vector-ref table index))
      ((rp:ww-register-insert-obj this obj)
       (if (rp:negative? free-entry)
	 (let ((new-table (rp:make-vector next-size)))
	   (do ((i 0 (rp:+ i 1))) ((rp:= i table-size))
	     (rp:vector-set! new-table i (rp:vector-ref table i)))
	   (do ((i (rp:- next-size 1) (rp:- i 1)) (n -1 i))
	     ((rp:< i table-size))
	     (rp:vector-set! new-table i n))
	   (set! table new-table)
	   (set! free-entry table-size)
	   (set! table-size next-size)
	   (set! expand-size free-entry)
	   (set! next-size (rp:+ table-size expand-size))))
       (let ((next-free (rp:vector-ref table free-entry)) (index free-entry))
	 (set! free-entry next-free)
	 (rp:vector-set! table index obj)
	 index))
      ((rp:ww-register-delete-obj this index)
       (rp:vector-set! table index free-entry)
       (set! free-entry index)))))

(define (rp:ww-api-arg-error api-name)
  (lambda (i type val)
    ((rp:exception (rp:ww-api-arg-error api-name index type value)
       (lambda (port) (rp:display "Win32 API " port) (rp:display api-name)
		      (rp:display " argument ") (rp:display index)
		      (rp:display " must be of type ") (rp:display type)
		      (rp:display " but got ") (rp:write value)))
     api-name i type val)))

(define rp:ww-postponed-api-entry
  (let ((mkentry (let ((eval eval))
		   (lambda (name module fun ret arg)
		     (let ((eproc (eval `(rp:external-procedure ,module ,fun ,ret ,arg))))
		       (if (rp:dbg-debugging? 'rp:ww-api-arg)
			 (let ((err (rp:ww-api-arg-error name)))
			   (lambda args (rp:apply rp:dbg-type-checker err arg args)
					(rp:apply eproc args)))
			 eproc))))))
    (lambda (name module fun ret arg)
      (lambda args
	(let ((entry (mkentry name module fun ret arg)))
	  (rp:symbol-value-set! name entry)
	  (rp:apply entry args))))))
