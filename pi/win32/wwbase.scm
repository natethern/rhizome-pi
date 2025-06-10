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

;;;; @(#)$Id: wwbase.scm,v 1.3 2004/08/06 05:48:06 qfwfq Exp $
; $Log: wwbase.scm,v $
; Revision 1.3  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.2  1999/02/15 08:12:55  qfwfq
; rp:ww-windows-error permit specification of error code
;
; Revision 1.1  1998/07/31 11:34:34  qfwfq
; First release of ww library
;

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_misc.scm")
(rp:use-macro-package "w_res.scm")
(rp:use-macro-package "w_class.scm")
(rp:use-macro-package "w_wstyle.scm")
(rp:use-macro-package "w_wmove.scm")
(rp:use-macro-package "w_offset.scm")
(rp:use-macro-package "w_msg.scm")

(define rp:ww-windows-error
  (let ((ptbuf (rp:make-external-buffer 1)))
    (lambda opt
      (let ((code (if (rp:null? opt) (rp:win32api-get-last-error) (rp:car opt))))
	(rp:win32api-format-message
	  (win:format-message allocate-buffer from-system)
	  #f code 0 ptbuf 0 #f)
	(let ((desc (rp:load-external-chars (rp:buffer-array-load ptbuf 0) 0 #f)))
	  (rp:win32api-local-free (rp:buffer-array-load ptbuf 0))
	  ((rp:exception (rp:ww-windows-error errcode errdesc)
	     (lambda (port) (rp:display "Error from Win32 API - " port)
			    (rp:display errdesc port)))
	   code desc))))))

(define rp:ww-make-window-class
  (let ((cnt 0))
    (rp:lambda-with-options
      (dispatcher)
      ((style 0) (cls-extra 0) (wnd-extra 0)
       (instance (rp:win32api-get-module-handle #f))
       (icon (rp:win32api-load-icon #f (rp:cast-integer->buffer (win:idi application))))
       (cursor (rp:win32api-load-cursor #f (rp:cast-integer->buffer (win:idc arrow))))
       (background (rp:cast-integer->buffer (rp:+ 1 (win:color window))))
       (menu-name #f) (class-name #f) (icon-sm #f))
      (if (rp:not class-name)
	(begin (set! class-name (rp:string-append "RP_GT_" (rp:object->string cnt)))
	       (set! cnt (rp:+ cnt 1))))
      (let ((buf-clsname (rp:export-string class-name))
	    (udat-tbl (rp:ww-register-create)))
	(rp:ww-register-insert-obj udat-tbl #f)
	(let ((wndproc (dispatcher udat-tbl)))
	  (let ((wc (win:wndclassex-create
		      style
		      (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
			(wndproc wnd message wparam lparam))
		      menu-name #f
		      (cls-extra cls-extra) (wnd-extra wnd-extra)
		      (instance instance)
		      (icon icon) (cursor cursor) (background background)
		      (class-name buf-clsname)
		      (icon-sm icon-sm))))
	    (let ((cls-atom (rp:win32api-register-class-ex wc)))
	      (rp:destroy-external-buffer wc)
	      (if (rp:zero? cls-atom)
		(begin (rp:destroy-external-buffer buf-clsname)
		       (rp:ww-windows-error))
		(rp:vector class-name buf-clsname cls-atom udat-tbl)))))))))

(define rp:ww-create-window
  (rp:lambda-with-options
    (class title data)
    ((exstyle 0) (style (win:ws overlappedwindow))
     (x (win:cw usedefault)) (y 0) (width (win:cw usedefault)) (height 0)
     (parent #f) (menu #f) (instance (rp:win32api-get-module-handle #f))
     (show (win:sw showdefault)))
    (let ((uind (rp:ww-register-insert-obj (rp:vector-ref class 3) data))
	  (buf-title (rp:export-string title)))
      (let ((wnd (rp:win32api-create-window-ex
		   exstyle (rp:vector-ref class 1) buf-title style
		   x y width height parent menu instance (rp:cast-integer->buffer uind))))
	(rp:destroy-external-buffer buf-title)
	(if wnd
	  (begin (if show
		   (begin (rp:win32api-show-window wnd show)
			  (rp:win32api-update-window wnd)))
		 wnd)
	  (begin (rp:ww-register-delete-obj (rp:vector-ref class 3) uind)
		 (rp:ww-windows-error)))))))

(define rp:ww-create-child-window
  (let ((nullstr (rp:export-string "")))
    (rp:lambda-with-options
      (class style x y width height parent id)
      ((exstyle 0) (text #f) (instance (rp:win32api-get-module-handle #f)) (param 0))
      (rp:locally ((class-buf (if (rp:string? class) (rp:export-string class) (rp:cast-integer->buffer class)))
		   (text-buf (if text (rp:export-string text) nullstr)))
		  ((if (rp:string? class) (rp:destroy-external-buffer class-buf))
		   (if text (rp:destroy-external-buffer text-buf)))
	(let ((wnd (rp:win32api-create-window-ex
		     exstyle class-buf text-buf (rp:bitwise-or style (win:ws child)) x y width height parent
		     (rp:cast-integer->buffer id) instance (rp:cast-integer->buffer param))))
	  (if wnd wnd (rp:ww-windows-error)))))))

(define (rp:ww-replace-window-procedure wnd dispatcher)
  (let ((oproc (rp:win32api-get-window-long wnd (win:gwl wndproc))))
    (let ((wndproc (dispatcher (rp:cast-integer->procedure oproc))))
      (let ((nproc (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
		     (wndproc wnd message wparam lparam))))
	(rp:win32api-set-window-long wnd (win:gwl wndproc) (rp:cast-procedure->integer nproc))
	(lambda () (rp:win32api-set-window-long wnd (win:gwl wndproc) oproc)
		   (rp:destroy-exported-procedure nproc))))))

(define rp:ww-replace-class-procedure
  (let ((nullstr (rp:export-string ""))
	(instance (rp:win32api-get-module-handle #f)))
    (lambda (class dispatcher)
      (rp:locally ((class-buf (rp:export-string class)))
		  ((rp:destroy-external-buffer class-buf))
	(let ((wnd (rp:win32api-create-window-ex 0 class-buf nullstr (win:ws popup) 0 0 10 10 #f #f instance #f)))
	  (if (rp:not wnd) (rp:ww-windows-error))
	  (let ((oproc (rp:win32api-get-class-long wnd (win:gcl wndproc))))
	    (let ((wndproc (dispatcher (rp:cast-integer->procedure oproc))))
	      (let ((nproc (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
			     (wndproc wnd message wparam lparam))))
		(rp:win32api-set-class-long wnd (win:gcl wndproc) (rp:cast-procedure->integer nproc))
		(lambda () (rp:win32api-set-class-long wnd (win:gcl wndproc) oproc)
			   (rp:destroy-exported-procedure nproc)
			   (rp:win32api-destroy-window wnd))))))))))

(define rp:ww-make-derived-class
  (let ((class-buf (win:wndclassex-allocate 1))
	(ninst (rp:win32api-get-module-handle #f)))
    (win:wndclassex-store-size class-buf (win:wndclassex-size))
    (rp:lambda-with-options
      (oclass nclass dispatcher)
      ((instance #f) (modify-class (lambda (buf) #t)))
      (rp:locally ((oclass-buf (rp:export-string oclass))
		   (nclass-buf (rp:export-string nclass)))
		  ((rp:destroy-external-buffer oclass-buf)
		   (rp:destroy-external-buffer nclass-buf))
	(if (rp:zero? (rp:win32api-get-class-info-ex instance oclass-buf class-buf)) (rp:ww-windows-error))
	(win:wndclassex-set-values class-buf
	  (wnd-proc (let ((wndproc (dispatcher (win:wndclassex-load-wnd-proc class-buf))))
		      (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
			(wndproc wnd message wparam lparam))))
	  (instance ninst) (menu-name #f) (class-name nclass-buf))
	(modify-class class-buf)
	(let ((atom (rp:win32api-register-class-ex class-buf)))
	  (if (rp:zero? atom) (rp:ww-windows-error))
	  atom)))))

(rp:define-generic (rp:ww-message-loop-add-dialog this dlg))
(rp:define-generic (rp:ww-message-loop-remove-dialog this dlg))
(rp:define-generic (rp:ww-message-loop-set-accelerator this accel wnd))
(rp:define-generic (rp:ww-message-loop-process-one-message this msg))
(rp:define-generic (rp:ww-message-loop-run this))
(rp:define-generic (rp:ww-message-loop-process-pending-messages this))

(define (rp:ww-message-loop-create . body)
  (let ((dialogs '())
	(accel-tbl #f)
	(accel-hwnd #f)
	(bodyf (if (rp:null? body)
		 (lambda (msg)
		   (rp:win32api-translate-message msg)
		   (rp:win32api-dispatch-message msg))
		 (rp:car body))))
    (rp:object-constructor ()
      ((rp:ww-message-loop-add-dialog this dlg)
       (set! dialogs (rp:cons dlg dialogs)))
      ((rp:ww-message-loop-remove-dialog this dlg)
       (set! dialogs
	 (let loop ((l dialogs))
	   (cond ((rp:eq? (rp:car l) dlg) (rp:cdr l))
		 (else (rp:cons (rp:car l) (loop (rp:cdr l))))))))
      ((rp:ww-message-loop-set-accelerator this accel wnd)
       (set! accel-tbl accel)
       (set! accel-hwnd wnd))
      ((rp:ww-message-loop-process-one-message this msg)
       (let loop ((l dialogs))
	 (cond ((rp:null? l)
		(if accel-tbl (if (rp:zero? (rp:win32api-translate-accelerator accel-hwnd accel-tbl msg)) (bodyf msg))
			      (bodyf msg)))
	       ((rp:zero? (rp:win32api-is-dialog-message (rp:car l) msg)) (loop (rp:cdr l))))))
      ((rp:ww-message-loop-run this)
       (let ((msg (win:msg-create))
	     (proc (rp:proxy this rp:ww-message-loop-process-one-message)))
	 (let ((return (lambda (v) (rp:destroy-external-buffer msg) v)))
	   (do () ((rp:zero? (rp:win32api-get-message msg #f 0 0)) (return (win:msg-load-wparam msg)))
	     (proc this msg)))))
      ((rp:ww-message-loop-process-pending-messages this)
       (let ((msg (win:msg-create))
	     (proc (rp:proxy this rp:ww-message-loop-process-one-message)))
	 (let ((return (lambda (v) (rp:destroy-external-buffer msg) v)))
	   (let loop () (cond ((rp:zero? (rp:win32api-peek-message msg #f 0 0 (win:pm remove))) (return #f))
			      ((rp:= (win:msg-load-message msg) (win:wm quit)) (return (win:msg-load-wparam msg)))
			      (else (proc this msg) (loop))))))))))

(define rp:ww-split-lparam
  (let ((buf (rp:make-external-buffer 1)))
    (lambda (lparam lo-signed? hi-signed? proc)
      (rp:integer-array-store buf 0 lparam)
      (proc (rp:load-external-halfword buf 0 lo-signed?)
	    (rp:load-external-halfword buf 2 hi-signed?)))))

(define rp:ww-split-wparam
  (let ((buf (rp:make-external-buffer 1)))
    (lambda (wparam lo-signed? hi-signed? proc)
      (rp:cardinal-array-store buf 0 wparam)
      (proc (rp:load-external-halfword buf 0 lo-signed?)
	    (rp:load-external-halfword buf 2 hi-signed?)))))

(define (rp:ww-do-paint wnd ps proc)
  (rp:locally ((dc (rp:win32api-begin-paint wnd ps)))
	      ((rp:win32api-end-paint wnd ps))
    (proc dc)))

(define rp:ww-windows-version #f)

(let ((vinfo (win:osversioninfo-create)))
  (rp:win32api-get-version-ex vinfo)
  (win:osversioninfo-let-values vinfo
    ((major major-version)
     (minor minor-version)
     (build build-number)
     (platform platform-id))
    (set! rp:ww-windows-version (lambda () major)))
  (rp:destroy-external-buffer vinfo))
