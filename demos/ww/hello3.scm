;;;; @(#)$Id: hello3.scm,v 1.1 1998/07/31 11:40:36 qfwfq Exp $

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_class.scm")
(rp:use-macro-package "w_wstyle.scm")
(rp:use-macro-package "w_c_btn.scm")
(rp:use-macro-package "w_msg.scm")
(rp:use-macro-package "w_dc.scm")

(rp:eval-in-compiler-environment
  (rp:declare-constants idm (exit 100) (about 101)))

(define *message*
  (if (null? (cdr *invocation-arg*))
    "Hello world"
    (cadr *invocation-arg*)))

(define draw-window
  (let ((text-buffer (rp:export-string *message*))
	(text-length (string-length *message*))
	(ps (win:paintstruct-create))
	(cl-rect (win:rect-create)))
    (lambda (wnd)
      (rp:ww-do-paint wnd ps
	(lambda (dc) (rp:win32api-set-bk-mode dc (win: transparent))
		     (rp:win32api-set-text-align dc (win:ta noupdatecp center baseline))
		     (rp:win32api-get-client-rect wnd cl-rect)
		     (rp:win32api-text-out dc
		       (quotient (win:rect-load-right cl-rect) 2)
		       (quotient (win:rect-load-bottom cl-rect) 2)
		       text-buffer text-length)
		     0)))))

(define about-dlg
  (rp:ww-dialog-create
    (rp:ww-make-dialog-template
      `("About hello" 40 40 160 44 ()
	(0 static "Rhizome/pi Hello world demo" 4 4 152 8 ((style ,(win:ss center))))
	(0 static "$Id: hello3.scm,v 1.1 1998/07/31 11:40:36 qfwfq Exp $" 4 16 152 10 ((style ,(win:ss left sunken endellipsis))))
	(,(win: idok) button "OK" 70 30 20 10 ((style ,(win:bs defpushbutton center))))))
    (rp:ww-dialog-dispatcher (wnd message wparam lparam)
      ((win:wm initdialog) 1)
      ((win:wm command)
       ((rp:ww-command-dispatcher (wnd id cmd ctl udat)
	  ((((win: idok) (win: idcancel)) *) (rp:win32api-end-dialog wnd 0)))
	wnd wparam lparam #f)
       1))))

(define the-class
  (rp:ww-make-window-class
    (rp:ww-message-dispatcher (wnd message wparam lparam)
      ((win:wm paint) (draw-window wnd))
      ((win:wm destroy)
       (rp:win32api-post-quit-message 0)
       0)
      ((win:wm command)
       ((rp:ww-command-dispatcher (wnd id cmd ctl udat)
	  ((((idm about)) *) (rp:ww-dialog-do-modal about-dlg wnd 0))
	  ((((idm exit)) *) (rp:win32api-post-message wnd (win:wm close) 0 0)))
	wnd wparam lparam #f)
       0))
    'style (win:cs hredraw vredraw)))

(define the-menu
  (rp:ww-make-menu `(("&File"
		      (("E&xit" ,(idm exit) ()))
		      ())
		     ("&Help"
		      (("&About" ,(idm about) ()))
		      ()))))

(define main-win
  (rp:ww-create-window the-class "rhizome/pi \"Hello world\" application" #f 'menu the-menu))

(define main-loop (rp:ww-message-loop-create))

(exit (rp:ww-message-loop-run main-loop))
