;;;; @(#)$Id: hello2.scm,v 1.1 1998/07/31 11:40:35 qfwfq Exp $

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_class.scm")
(rp:use-macro-package "w_msg.scm")
(rp:use-macro-package "w_dc.scm")

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

(define the-class
  (rp:ww-make-window-class
    (rp:ww-message-dispatcher (wnd message wparam lparam)
      ((win:wm paint) (draw-window wnd))
      ((win:wm destroy)
       (rp:win32api-post-quit-message 0)
       0))
    'style (win:cs hredraw vredraw)))

(define main-win
  (rp:ww-create-window the-class "rhizome/pi \"Hello world\" application" #f))

(define main-loop (rp:ww-message-loop-create))

(exit (rp:ww-message-loop-run main-loop))
