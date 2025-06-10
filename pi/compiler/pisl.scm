; Copyright (c) 1996-99,2002 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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

;;;; @(#)$Id: pisl.scm,v 1.11 2004/08/06 05:48:06 qfwfq Exp $
; $Log: pisl.scm,v $
; Revision 1.11  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.10  2002/09/27 12:07:57  qfwfq
; Add support of linux, lcc-win32 and recent version of win compilers.
;
; Revision 1.9  1999/06/15 07:36:01  qfwfq
; Various shared library arrangement
;
; Revision 1.8  1999/03/15 12:57:39  qfwfq
; enable -loadable in Win32 Visual C++ environment
;
; Revision 1.7  1999/02/15 08:28:44  qfwfq
; port to Microsoft C compiler
;
; Revision 1.6  1998/07/31 11:16:51  qfwfq
; Adoption to Win32 GUI environment
;
; Revision 1.5  1997/10/16 06:25:05  qfwfq
; Release version 0.40
;
; Revision 1.4  1997/05/12 07:21:27  qfwfq
; version 0.31 - some enhancements on error handling etc.
;
; Revision 1.3  1997/04/26 13:29:14  qfwfq
; Version 0.30 - hygienic macro system with syntax-case
;
; Revision 1.2  1996/10/10 08:27:10  qfwfq
; Ported to Win32 environment.
;
; Revision 1.1  1996/09/06 06:12:39  qfwfq
; Version 0.20 unix revision is up.
;

(define *rhizome-lib* (getenv (cm-lib-environment-var)))
(define *def-cc-command* (cm-cc-command unshare *rhizome-lib*))
(define *share-cc-command* (cm-cc-command share *rhizome-lib*))
(define *static-cc-command* (cm-cc-command static *rhizome-lib*))
(define *def-ld-command* (cm-ld-command con *rhizome-lib*))
(define *win-ld-command* (cm-ld-command gui *rhizome-lib*))
(define *share-ld-command* (cm-ld-command share *rhizome-lib*))
(define *dynamic-ld-lib* (cm-ld-lib dynamic *rhizome-lib*))
(define *static-ld-lib* (cm-ld-lib static *rhizome-lib*))
(define *loadable-ld-lib* (cm-ld-lib loadable *rhizome-lib*))

(define (usage)
  (display "$Id: pisl.scm,v 1.11 2004/08/06 05:48:06 qfwfq Exp $") (newline)
  (display "usage: ")
  (display (rp:basename (car *invocation-arg*) #f))
  (display " [-cc <cc command>] [-ld <ld command>|-nold] [-nolib] [-loadable|-static|-modlib]")
  (display " [-windows] [-o <output>] [-s <startup>] [-base <address>]")
  (display " {-xm stdmacro|expand|debugger|stdproc|extcall|saccess}... {-aux <file>}... module[:file] ...")
  (newline)
  (display "default is <cc command> = ") (write *def-cc-command*) (newline)
  (display "        (for -loadable)   ") (write *share-cc-command*) (newline)
  (display "           <ld command> = ") (write *def-ld-command*) (newline)
  (display "        (for -loadable)   ") (write *share-ld-command*) (newline)
  (display "        (for -windows)    ") (write *win-ld-command*) (newline)
  (display "link library is ") (write *dynamic-ld-lib*) (newline)
  (display "  (for -static) ") (write *static-ld-lib*) (newline)
  (display "(for -loadable) ") (write (or *loadable-ld-lib* '<none>)) (newline)
  (exit 2))

(define *cc-command* #f)
(define *ld-command* #f)
(define *no-load* #f)
(define *make-loadable* #f)
(define *make-static* (cm-always-static))
(define *make-modlib* #f)
(define *make-winapp* #f)
(define *output* #f)
(define *startup* #f)
(define *base-addr* #f)
(define *aux-inputs* (cons #t '()))
(define *modules* (cons #t '()))
(define *no-library* #f)
(define *link-stdmacro* #t)
(define *link-expand* #t)
(define *link-debugger* #t)
(define *link-stdproc* #t)
(define *link-extcall* #t)
(define *link-saccess* #t)

(define (make-cc-line input) (cm-cc-line *cc-command* input))
(define (make-ld-line)
  (let ((result *ld-command*))
    (if *base-addr* (set! result (cm-add-base-option result *base-addr*)))
    (if *output* (set! result (cm-add-output-option result *output*)))
    (if (not *make-modlib*) (set! result (cm-add-module result *startup*)))
    (do ((module (cdr *modules*) (cdr module))) ((null? module))
      (if (not (string=? (cadar module) ""))
	(set! result (cm-add-module result (cadar module)))))
    (do ((aux (cdr *aux-inputs*) (cdr aux))) ((null? aux))
      (set! result (string-append result " " (car aux))))
    (if (not *no-library*)
      (cond ((or *make-loadable* *make-modlib*)
	     (if *loadable-ld-lib* (set! result (string-append result " " *loadable-ld-lib*))))
	    (*make-static* (set! result (string-append result " " *static-ld-lib*)))
	    (else (set! result (string-append result " " *dynamic-ld-lib*)))))
    result))

(let loop ((args (cdr *invocation-arg*)) (aux *aux-inputs*) (mod *modules*))
  (if (null? args)
    (if (and *link-stdproc* (not (or *make-loadable* *make-modlib*))) (set-cdr! mod '(("rp_topl" ""))))
    (let ((arg (car args)) (get-option (lambda () (if (null? (cdr args)) (usage) (cadr args)))))
      (cond ((string=? arg "-cc") (set! *cc-command* (get-option)) (loop (cddr args) aux mod))
	    ((string=? arg "-ld") (set! *ld-command* (get-option)) (loop (cddr args) aux mod))
	    ((string=? arg "-nold") (set! *no-load* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-nolib") (set! *no-library* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-loadable") (set! *make-loadable* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-static") (set! *make-static* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-modlib") (set! *make-modlib* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-windows") (set! *make-winapp* #t) (loop (cdr args) aux mod))
	    ((string=? arg "-o") (set! *output* (get-option)) (loop (cddr args) aux mod))
	    ((string=? arg "-s") (set! *startup* (get-option)) (loop (cddr args) aux mod))
	    ((string=? arg "-base") (set! *base-addr* (get-option)) (loop (cddr args) aux mod))
	    ((string=? arg "-aux") (set-cdr! aux (cons (get-option) '())) (loop (cddr args) (cdr aux) mod))
	    ((string=? arg "-xm")
	     (let ((mod (get-option)))
	       (cond ((string=? mod "stdmacro") (set! *link-stdmacro* #f))
		     ((string=? mod "expand") (set! *link-expand* #f))
		     ((string=? mod "debugger") (set! *link-debugger* #f))
		     ((string=? mod "stdproc") (set! *link-stdproc* #f))
		     ((string=? mod "extcall") (set! *link-extcall* #f))
		     ((string=? mod "saccess") (set! *link-saccess* #f))
		     (else (usage))))
	     (loop (cddr args) aux mod))
	    ((or (string=? arg "-h") (string=? arg "-help")) (usage))
	    (else
	     (set-cdr! mod
	       (let loop ((i 0))
		 (cond ((= i (string-length arg)) (cons (list arg (string-append arg ".c")) '()))
		       ((char=? (string-ref arg i) #\:)
			(cons (list (substring arg 0 i) (substring arg (+ i 1) (string-length arg))) '()))
		       (else (loop (+ i 1))))))
	     (loop (cdr args) aux (cdr mod)))))))

(if (not *cc-command*) (set! *cc-command* (cond (*make-loadable* *share-cc-command*)
						(*make-modlib* (string-append *share-cc-command* " -DRP_MODLIB"))
						(*make-static* *static-cc-command*)
						(else *def-cc-command*))))
(if (not *ld-command*) (set! *ld-command* (cond ((or *make-loadable* *make-modlib*) *share-ld-command*)
						(*make-winapp* *win-ld-command*)
						(else *def-ld-command*))))

(if (not *link-stdproc*) (set! *link-debugger* #f))
(if (not (and *link-stdmacro* *link-stdproc*)) (set! *link-expand* #f))
(if (not *link-expand*) (set! *link-saccess* #f))
(if (not *link-saccess*) (set! *link-extcall* #f))
(if (not (or *make-loadable* *make-modlib*))
  (set! *modules*
    (append '(#t ("rp_fasr" ""))
      (if (or *link-stdmacro* *link-stdproc*) '(("rp_priv" "")) '())
      (if *link-stdmacro* (if *link-expand* '(("rp_expn" "") ("rp_stds" "")) '(("rp_stdm" ""))) '())
      (if *link-debugger* '(("rp_debg" "")) '())
      (if *link-debugger* (if *link-expand* '(("rp_dbgs" "")) '(("rp_dbgm" ""))) '())
      (if *link-stdproc* '(("rp_schm" "")) '())
      (if *link-saccess* '(("rp_sacc" "")) '())
      (if *link-extcall* '(("rp_extc" "")) '())
      (cdr *modules*))))

(if (not *startup*)
  (let ((isuffix (if (or *make-loadable* *make-modlib*)
		   (cm-default-exe-suffix share)
		   (cm-default-exe-suffix unshare))))
    (set! *startup*
      (string-append (if *output* (string-append "s_" (rp:basename *output* isuffix)) "a") ".c"))))

(if (not (or *no-load* *make-modlib*))
  (call-with-output-file *startup*
    (lambda (outp)
      (rp:emit-line outp "/* Generated by $Id: pisl.scm,v 1.11 2004/08/06 05:48:06 qfwfq Exp $ */")
      (rp:emit-line outp "#include \"rhiz_pi.h\"")
      (newline outp)
      (for-each
	(lambda (m)
	  (rp:emit-line outp "extern rk_object RpC~ARun(void);" (car m))
	  (rp:emit-line outp "extern void RpC~AInit(struct RP_MODULE_INIT *);" (car m)))
	(cdr *modules*))
      (newline outp)
      (rp:emit-line outp "RP_DLLEXPORT int const rp_in_windows_subsystem = ~A;" (if *make-winapp* 1 0))
      (newline outp)
      (rp:emit-line outp "static rk_object (* const run[])(void) = {")
      (for-each (lambda (m) (rp:emit-line outp "~TRpC~ARun," (car m))) (cdr *modules*))
      (rp:emit-line outp "};")
      (newline outp)
      (rp:emit-line outp "static void (* const init[])(struct RP_MODULE_INIT *) = {")
      (for-each (lambda (m) (rp:emit-line outp "~TRpC~AInit," (car m))) (cdr *modules*))
      (rp:emit-line outp "};")
      (newline outp)
      (rp:emit-line outp "RP_DLLEXPORT struct RP_PROGRAM_DESC const *RpProgramDesc(void) {")
      (rp:emit-line outp "~Tstatic struct RP_PROGRAM_DESC const prog_d = {~A, run, init};" (length (cdr *modules*)))
      (rp:emit-line outp "~Treturn &prog_d;")
      (rp:emit-line outp "}"))))

(define (invoke-compiler c)
  (display c) (newline)
  (let ((r (system c))) (if (not (zero? r)) (exit (cm-exit-status r)))))
;;; for Cygnus win32 environment beta16, use this
;(define (invoke-compiler c)
;  (display c) (newline)
;  (call-with-output-file ">/tmp/pisl_cmd" (lambda (p) (display "exec " p) (display c p) (newline p)))
;  (let ((r (system "/tmp/pisl_cmd"))) (if (not (zero? r)) (exit (cm-exit-status r)))))

(for-each (lambda (m) (if (not (string=? (cadr m) "")) (invoke-compiler (make-cc-line (cadr m))))) (cdr *modules*))
(if (not (or *no-load* *make-modlib*)) (invoke-compiler (make-cc-line *startup*)))
(if (not *no-load*) (invoke-compiler (make-ld-line)))
(exit 0)
