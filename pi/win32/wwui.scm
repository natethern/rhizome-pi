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

;;;; @(#)$Id: wwui.scm,v 1.3 2004/08/06 05:48:06 qfwfq Exp $
; $Log: wwui.scm,v $
; Revision 1.3  2004/08/06 05:48:06  qfwfq
; change license, using OpenBSD:/usr/share/misc/license.template
;
; Revision 1.2  1999/06/15 07:24:25  qfwfq
; Add packed buffer
;
; Revision 1.1  1998/07/31 11:34:37  qfwfq
; First release of ww library
;

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_menu.scm")
(rp:use-macro-package "w_mb.scm")
(rp:use-macro-package "w_wstyle.scm")

(define rp:ww-make-menu
  (let ((item-info (win:menuiteminfo-create))
	(mask-bits (rp:vector (win:miim state) (win:miim id) (win:miim submenu)
			      (win:miim checkmarks) (win:miim data) (win:miim bitmap)))
	(type-options `((menubarbreak . ,(win:mft menubarbreak))
			(menubreak . ,(win:mft menubreak))
			(radiocheck . ,(win:mft radiocheck))
			(rightjustify . ,(win:mft rightjustify))
			(rightorder . ,(win:mft rightorder))))
	(state-options `((checked . (,(win:mfs checked) . ,(win:mfs unchecked)))
			 (enabled . (,(win:mfs enabled) . ,(win:mfs disabled)))
			 (hilite . (,(win:mfs hilite) . ,(win:mfs unhilite)))))
	(v5upper? (rp:<= 5 (rp:ww-windows-version))))
    (define (make-menu desc)
      (let ((menu (rp:win32api-create-menu)))
	(if (rp:not menu) (rp:ww-windows-error))
	(do ((i 0 (rp:+ i 1)) (l desc (rp:cdr l)))
	  ((rp:null? l) menu)
	  (make-item menu i (rp:car l)))))
    (define (make-item menu index item)
      (let ((label (rp:car item))
	    (content (rp:car (rp:cdr item)))
	    (optlist (rp:car (rp:cdr (rp:cdr item))))
	    (mask (rp:make-vector 6 #f)) (type 0) (state 0))
	(cond ((rp:number? content)
	       (rp:vector-set! mask 1 #t)
	       (win:menuiteminfo-store-id item-info content))
	      (else
	       (rp:vector-set! mask 2 #t)
	       (win:menuiteminfo-store-sub-menu item-info (make-menu content))))
	(cond ((rp:string? label)
	       (set! type (win:mft string))
	       (win:menuiteminfo-store-type-data item-info (rp:export-string label)))
	      ((rp:eq? label 'separator)
	       (set! type (win:mft separator)))
	      ((rp:eq? (rp:car label) 'bitmap)
	       (set! type (win:mft bitmap))
	       (win:menuiteminfo-store-type-data item-info (rp:car (rp:cdr label))))
	      (else
	       (set! type (win:mft ownerdraw))
	       (win:menuiteminfo-store-type-data item-info (rp:cast-integer->buffer (rp:car (rp:cdr label))))))
	(do ((optlist optlist (rp:cdr optlist))) ((rp:null? optlist))
	  (let ((opt (rp:car optlist)))
	    (cond ((rp:assq opt type-options) =>
		   (lambda (pair) (set! type (rp:bitwise-or type (rp:cdr pair)))))
		  ((rp:eq? opt 'default)
		   (rp:vector-set! mask 0 #t)
		   (set! state (rp:bitwise-or state (win:mfs default))))
		  (else
		   (let ((optid (rp:car opt)) (optdat (rp:cdr opt)))
		     (cond ((rp:assq optid state-options) =>
			    (lambda (pair)
			      (rp:vector-set! mask 0 #t)
			      (set! state (rp:bitwise-or state ((if (rp:car optdat) rp:car rp:cdr) (rp:cdr pair))))))
			   ((rp:eq? optid 'checkmarks)
			    (rp:vector-set! mask 3 #t)
			    (win:menuiteminfo-store-bmp-checked item-info (rp:car optdat))
			    (win:menuiteminfo-store-bmp-unchecked item-info (rp:car (rp:cdr optdat))))
			   ((rp:eq? optid 'data)
			    (rp:vector-set! mask 4 #t)
			    (win:menuiteminfo-store-item-data item-info (rp:car optdat)))
			   ((and (rp:eq? optid 'bitmap) v5upper?)
			    (rp:vector-set! mask 5 #t)
			    (win:menuiteminfo-store-bmp-item item-info (rp:car optdat)))))))))
	(win:menuiteminfo-store-type item-info type)
	(win:menuiteminfo-store-state item-info state)
	(do ((i 0 (+ i 1))
	     (mask-val (win:miim type)
	       (if (rp:vector-ref mask i) (rp:bitwise-or mask-val (rp:vector-ref mask-bits i)) mask-val)))
	  ((rp:= i 6)
	   (win:menuiteminfo-store-mask item-info mask-val)))
	(if (rp:zero? (rp:win32api-insert-menu-item menu index 1 item-info)) (rp:ww-windows-error))
	(if (rp:string? label) (rp:destroy-external-buffer (win:menuiteminfo-load-type-data item-info)))))
    (win:menuiteminfo-store-size item-info
      (rp:- (win:menuiteminfo-size) (if v5upper? 0 4)))
    make-menu))

(define rp:ww-make-accelerator-table
  (let ((flag-table `((virtkey . ,(win: fvirtkey)) (noinvert . ,(win: fnoinvert))
		      (shift . ,(win: fshift)) (control . ,(win: fcontrol)) (alt . ,(win: falt)))))
    (lambda (table)
      (let ((acct (win:accel-allocate (rp:length table))))
	(do ((table table (rp:cdr table))
	     (acct acct (rp:skip-buffer-element acct (win:accel-size))))
	  ((rp:null? table))
	  (let ((flags (rp:car (rp:car table)))
		(char (rp:car (rp:cdr (rp:car table))))
		(cmd (rp:car (rp:cdr (rp:cdr (rp:car table)))))
		(virt 0))
	    (rp:for-each (lambda (f) (cond ((rp:assq f flag-table) =>
					    (lambda (p) (set! virt (rp:bitwise-or virt (rp:cdr p)))))))
			 flags)
	    (win:accel-set-values acct (virt (rp:string (rp:integer->char virt))) (key char) (cmd cmd))))
	(let ((accel (rp:win32api-create-accelerator-table acct (rp:length table))))
	  (rp:destroy-external-buffer acct)
	  (if (rp:not accel) (rp:ww-windows-error))
	  accel)))))

(define (rp:ww-destroy-accelerator-table accel) (rp:win32api-destroy-accelerator-table accel))

(define (rp:ww-make-dialog-template desc)
  (define (roundup n) (rp:* (rp:quotient (rp:+ n 3) 4) 4))
  (define buffer-size 0)
  (define (increment-nhwords n) (set! buffer-size (rp:+ buffer-size n n)))
  (define (convert-string str)
    (let ((buf (rp:export-string str)))
      (increment-nhwords (rp:win32api-multi-byte-to-wide-char (win:cp acp) 0 buf -1 #f 0))
      (rp:cons 'string buf)))
  (define (convert-string-or-ordinal dat)
    (cond ((rp:not dat) (increment-nhwords 1) '(null))
	  ((rp:number? dat) (increment-nhwords 2) (rp:cons 'ordinal dat))
	  (else (convert-string dat))))
  (define store-point 0)
  (define (store-dword buffer n)
    (rp:cardinal-array-store buffer (rp:quotient store-point 4) n)
    (set! store-point (rp:+ store-point 4)))
  (define (store-unsigned buffer n)
    (rp:store-external-halfword buffer store-point #f n)
    (set! store-point (rp:+ store-point 2)))
  (define (store-signed buffer n)
    (rp:store-external-halfword buffer store-point #t n)
    (set! store-point (rp:+ store-point 2)))
  (define (store-string-or-ordinal buffer dat)
    (case (rp:car dat)
      ((null) (store-unsigned buffer 0))
      ((ordinal) (store-unsigned buffer #xffff) (store-unsigned buffer (rp:cdr dat)))
      ((string)
       (let ((nwc (rp:win32api-multi-byte-to-wide-char
		    (win:cp acp) 0
		    (rp:cdr dat) -1
		    (rp:skip-buffer-element buffer store-point) (rp:quotient (rp:- buffer-size store-point) 2))))
	 (set! store-point (rp:+ store-point (rp:* nwc 2)))
	 (rp:destroy-external-buffer (rp:cdr dat))))))
  (define (decode-item item-desc)
    (rp:apply
      (lambda (id class title x y cx cy opts)
	(let ((v (rp:vector 0 0 0 x y cx cy id
		   (cond ((rp:assq class '((button . #x80) (edit . #x81) (static . #x82)
					   (list-box . #x83) (scroll-bar . #x84) (combo-box . #x85))) =>
			  (lambda (p) (increment-nhwords 2) (rp:cons 'ordinal (rp:cdr p))))
			 (else (convert-string class)))
		   (convert-string-or-ordinal title) #f)))
	  (rp:for-each
	    (lambda (opt)
	      (rp:vector-set! v
		(rp:cdr (rp:assq (rp:car opt) '((help-id . 0) (ex-style . 1) (style . 2) (creation-data . 10))))
		(rp:car (rp:cdr opt)))) opts)
	  (increment-nhwords 13)
	  (rp:vector-set! v 2 (rp:bitwise-or (rp:vector-ref v 2) (win:ws child visible)))
	  (set! buffer-size (roundup (rp:+ buffer-size (cond ((rp:vector-ref v 10) => rp:string-length) (else 0)))))
	  v))
      item-desc))
  (define (store-item buffer item-data)
    (store-dword buffer (rp:vector-ref item-data 0))
    (store-dword buffer (rp:vector-ref item-data 1))
    (store-dword buffer (rp:vector-ref item-data 2))
    (store-signed buffer (rp:vector-ref item-data 3))
    (store-signed buffer (rp:vector-ref item-data 4))
    (store-signed buffer (rp:vector-ref item-data 5))
    (store-signed buffer (rp:vector-ref item-data 6))
    (store-dword buffer (rp:vector-ref item-data 7))
    (store-string-or-ordinal buffer (rp:vector-ref item-data 8))
    (store-string-or-ordinal buffer (rp:vector-ref item-data 9))
    (let ((creation-data (rp:vector-ref item-data 10)))
      (if creation-data
	(begin
	  (store-unsigned buffer (rp:string-length creation-data))
	  (rp:store-external-chars buffer store-point creation-data)
	  (set! store-point (rp:+ store-point (rp:string-length creation-data))))
	(store-unsigned buffer 0)))
    (set! store-point (roundup store-point)))
  (rp:apply
    (lambda (title x y cx cy opts . items)
      (let ((v (rp:vector 0 0 (rp:bitwise-or (win:ds modalframe) (win:ws caption sysmenu)) #f #f))
	    (t #f) (f #f) (m #f) (wc #f)
	    (i (rp:map decode-item items)))
	(increment-nhwords 13)
	(set! t (if title (convert-string title) (begin (increment-nhwords 1) '(null))))
	(rp:for-each
	  (lambda (opt)
	    (cond ((rp:eq? (rp:car opt) 'font)
		   (increment-nhwords 3)
		   (set! f (rp:apply (lambda (ptsize wght italic font)
				       (rp:vector ptsize wght italic (convert-string font)))
				     (rp:cdr opt))))
		  ((rp:assq (rp:car opt) '((help-id . 0) (ex-style . 1) (style . 2) (menu . 3) (window-class . 4))) =>
		   (lambda (p) (rp:vector-set! v (rp:cdr p) (rp:car (rp:cdr opt)))))))
	  opts)
	(rp:vector-set! v 2 (rp:bitwise-or (rp:vector-ref v 2) (win:ws popup)))
	(if f (rp:vector-set! v 2 (rp:bitwise-or (rp:vector-ref v 2) (win:ds setfont))))
	(set! m (convert-string-or-ordinal (rp:vector-ref v 3)))
	(set! wc (convert-string-or-ordinal (rp:vector-ref v 4)))
	(set! buffer-size (roundup buffer-size))
	(let ((template (rp:make-external-buffer (rp:quotient buffer-size 4))))
	  (store-unsigned template 1)
	  (store-unsigned template #xffff)
	  (store-dword template (rp:vector-ref v 0))
	  (store-dword template (rp:vector-ref v 1))
	  (store-dword template (rp:vector-ref v 2))
	  (store-unsigned template (rp:length items))
	  (store-signed template x)
	  (store-signed template y)
	  (store-signed template cx)
	  (store-signed template cy)
	  (store-string-or-ordinal template m)
	  (store-string-or-ordinal template wc)
	  (store-string-or-ordinal template t)
	  (if f (begin (store-signed template (rp:vector-ref f 0))
		       (store-signed template (rp:vector-ref f 1))
		       (store-signed template (rp:vector-ref f 2))
		       (store-string-or-ordinal template (rp:vector-ref f 3))))
	  (set! store-point (roundup store-point))
	  (rp:for-each (lambda (item) (store-item template item)) i)
	  template))) desc))

(rp:define-generic (rp:ww-dialog-destroy this))
(rp:define-generic (rp:ww-dialog-do-modal this parent param))
(rp:define-generic (rp:ww-dialog-create-modeless this parent param))

(define rp:ww-dialog-create
  (rp:lambda-with-options
    (template func)
    ((instance (rp:win32api-get-module-handle #f)))
    (let ((proc (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
		  (func wnd message wparam lparam))))
      (rp:object-constructor ()
	((rp:ww-dialog-destroy this)
	 (rp:destroy-exported-procedure proc))
	((rp:ww-dialog-do-modal this parent param)
	 (rp:win32api-dialog-box-indirect-param instance template parent proc param))
	((rp:ww-dialog-create-modeless this parent param)
	 (rp:win32api-create-dialog-indirect-param instance template parent proc param))))))

(define rp:ww-message-box
  (let ((button-types `((ok . ,(win:mb ok)) (ok-cancel . ,(win:mb okcancel))
			(abort-retry-ignore . ,(win:mb abortretryignore)) (yes-no-cancel . ,(win:mb yesnocancel))
			(yes-no . ,(win:mb yesno)) (retry-cancel . ,(win:mb retrycancel))))
	(icons `((#f . 0) (hand . ,(win:mb iconhand)) (question . ,(win:mb iconquestion))
		 (exclamation . ,(win:mb iconexclamation)) (asterisk . ,(win:mb iconasterisk))))
	(def-button `#(#f ,(win:mb defbutton1) ,(win:mb defbutton2) ,(win:mb defbutton3) ,(win:mb defbutton4)))
	(modal `((application . ,(win:mb applmodal)) (system . ,(win:mb systemmodal)) (task . ,(win:mb taskmodal))))
	(optlist `((help . ,(win:mb help)) (no-focus . ,(win:mb nofocus)) (set-foreground . ,(win:mb setforeground))
		   (default-desktop-only . ,(win:mb default-desktop-only)) (topmost . ,(win:mb topmost))
		   (right . ,(win:mb right)) (rtl-reading . ,(win:mb rtlreading))
		   (service-notification . ,(win:mb service-notification)))))
    (rp:lambda-with-options (wnd text)
      ((caption #f) (buttons 'ok) (icon #f) (default-button 1) (mode 'application) (options '()))
      (let ((txtbuf (rp:export-string text)) (capbuf #f) (type 0))
	(define (add-type n) (set! type (rp:bitwise-or type n)))
	(if caption (set! capbuf (rp:export-string caption)))
	(add-type (rp:cdr (rp:assq buttons button-types)))
	(add-type (rp:cdr (rp:assq icon icons)))
	(add-type (rp:vector-ref def-button default-button))
	(add-type (rp:cdr (rp:assq mode modal)))
	(rp:for-each (lambda (opt) (add-type (rp:cdr (rp:assq opt optlist)))) options)
	(let ((res (rp:win32api-message-box wnd txtbuf capbuf type)))
	  (rp:destroy-external-buffer txtbuf)
	  (if capbuf (rp:destroy-external-buffer capbuf))
	  res)))))
