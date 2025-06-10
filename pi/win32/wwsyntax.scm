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

;;;; @(#)$Id: wwsyntax.scm,v 1.3 2004/08/06 05:48:06 qfwfq Exp $
; $Log: wwsyntax.scm,v $
; Revision 1.3  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.2  1999/02/15 08:14:38  qfwfq
; API call argument check
;
; Revision 1.1  1998/07/31 11:34:36  qfwfq
; First release of ww library
;

(define-syntax rp:ww-load-system-dll
  (lambda (x)
    (syntax-case x ()
      ((_ id name)
       (with-syntax ((idsym (implicit-identifier (syntax id)
			      (rp:string->symbol
				(rp:string-append "rp:ww-win32-sysdll-"
						  (rp:symbol->string (syntax-object->datum (syntax id))))))))
	 (syntax (define idsym (rp:load-external-object name))))))))

(define-syntax rp:ww-book-api-entry
  (lambda (x)
    (define (mksym prefix name)
      (implicit-identifier name
	(rp:string->symbol (rp:string-append prefix (rp:symbol->string (syntax-object->datum name))))))
    (syntax-case x ()
      ((_ scm dll fun ret arg)
       (with-syntax ((scm-name (mksym "rp:win32api-" (syntax scm)))
		     (module (mksym "rp:ww-win32-sysdll-" (syntax dll))))
	 (syntax (define scm-name (rp:ww-postponed-api-entry 'scm-name 'module 'fun 'ret 'arg))))))))

(define-syntax rp:ww-load-api-entry
  (lambda (x)
    (define (mksym prefix name)
      (implicit-identifier name
	(rp:string->symbol (rp:string-append prefix (rp:symbol->string (syntax-object->datum name))))))
    (syntax-case x ()
      ((_ scm dll fun ret arg)
       (with-syntax ((scm-name (mksym "rp:win32api-" (syntax scm)))
		     (module (mksym "rp:ww-win32-sysdll-" (syntax dll))))
	 (if (rp:dbg-debugging? 'rp:ww-api-arg)
	   (syntax (define scm-name
		     (let ((eproc (rp:external-procedure module fun ret arg))
			   (err (rp:ww-api-arg-error 'scm-name)))
		       (lambda args (rp:apply rp:dbg-type-checker err 'arg args)
				    (rp:apply eproc args)))))
	   (syntax (define scm-name (rp:external-procedure module fun ret arg)))))))))

(define-syntax rp:ww-message-dispatcher
  (lambda (x)
    (syntax-case x ()
      ((_ (wnd message wparam lparam) (msg exp1 exp2 ...) ...)
       (with-syntax ((n (rp:length (syntax (msg ...))))
		     (setudat (implicit-identifier (syntax _) 'rp:ww-set-user-data))
		     (getudat (implicit-identifier (syntax _) 'rp:ww-get-user-data))
		     (deludat (implicit-identifier (syntax _) 'rp:ww-delete-user-data)))
	 (syntax (lambda (udat-tbl)
		   (let ((tbl (rp:ww-dispatch-make-table n))
			 (setudat (lambda (wnd lparam)
				    (rp:win32api-set-window-long wnd (win:gwl userdata)
				      (rp:cast-buffer->integer
					(win:createstruct-load-create-params (rp:cast-integer->buffer lparam))))))
			 (getudat (let ((uget (rp:proxy udat-tbl rp:ww-register-get-obj)))
				    (lambda (wnd)
				      (uget udat-tbl (rp:win32api-get-window-long wnd (win:gwl userdata))))))
			 (deludat (lambda (wnd)
				    (rp:ww-register-delete-obj udat-tbl
				      (rp:win32api-get-window-long wnd (win:gwl userdata))))))
		     (rp:ww-dispatch-install-handler tbl msg
		       (lambda (wnd message wparam lparam) exp1 exp2 ...))
		     ...
		     (lambda (wnd message wparam lparam)
		       ((rp:ww-dispatch-get-handler tbl message rp:win32api-def-window-proc)
			wnd message wparam lparam))))))))))

(define-syntax rp:ww-subclass-dispatcher
  (lambda (x)
    (syntax-case x ()
      ((_ (wnd message wparam lparam) (msg exp1 exp2 ...) ...)
       (with-syntax ((n (rp:length (syntax (msg ...))))
		     (baseproc (implicit-identifier (syntax _) 'rp:ww-call-base-procedure)))
	 (syntax (lambda (oproc)
		   (let ((tbl (rp:ww-dispatch-make-table n))
			 (baseproc (lambda (wnd message wparam lparam)
				     (rp:win32api-call-window-proc oproc wnd message wparam lparam))))
		     (rp:ww-dispatch-install-handler tbl msg
		       (lambda (wnd message wparam lparam) exp1 exp2 ...))
		     ...
		     (lambda (wnd message wparam lparam)
		       ((rp:ww-dispatch-get-handler tbl message baseproc) wnd message wparam lparam))))))))))

(define-syntax rp:ww-dialog-dispatcher
  (lambda (x)
    (syntax-case x ()
      ((_ (wnd message wparam lparam) (msg exp1 exp2 ...) ...)
       (with-syntax ((n (rp:length (syntax (msg ...)))))
	 (syntax (let ((tbl (rp:ww-dispatch-make-table n))
		       (defproc (lambda (wnd message wparam lparam) 0)))
		   (rp:ww-dispatch-install-handler tbl msg
		     (lambda (wnd message wparam lparam) exp1 exp2 ...))
		   ...
		   (lambda (wnd message wparam lparam)
		     ((rp:ww-dispatch-get-handler tbl message defproc) wnd message wparam lparam)))))))))

(define-syntax rp:ww-command-dispatcher
  (lambda (x)
    (define (idlist->cond idlist id)
      (with-syntax ((ids id))
	(syntax-case idlist (-)
	  ((n1 - n2) (syntax ((else (rp:<= n1 ids n2)))))
	  ((n) (syntax ((else (rp:= ids n)))))
	  ((n1 - n2 m1 m2 ...) (with-syntax (((next ...) (idlist->cond (syntax (m1 m2 ...)) id)))
				 (syntax (((rp:<= n1 ids n2) #t) next ...))))
	  ((n m1 m2 ...) (with-syntax (((next ...) (idlist->cond (syntax (m1 m2 ...)) id)))
			   (syntax (((rp:= ids n) #t) next ...)))))))
    (define (spec->criteria spec id cmd)
      (with-syntax ((cmds cmd))
	(syntax-case spec (*)
	  ((idl *) (with-syntax (((idc ...) (idlist->cond (syntax idl) id)))
		     (syntax (cond idc ...))))
	  ((idl k) (with-syntax (((idc ...) (idlist->cond (syntax idl) id)))
		     (syntax (if (cond idc ...) (rp:= cmds k) #f)))))))
    (syntax-case x ()
      ((_ (wnd id cmd ctl udat) (spec exp1 exp2 ...) ...)
       (with-syntax (((crit ...)
		      (rp:map (lambda (s) (spec->criteria s (syntax id) (syntax cmd))) (syntax (spec ...)))))
	 (syntax (lambda (wnd wparam lparam udat)
		   (let ((ctl (rp:cast-integer->buffer lparam)))
		     (rp:ww-split-wparam wparam #f #f
		       (lambda (id cmd) (cond (crit exp1 exp2 ...) ...)))))))))))
