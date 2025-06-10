;;;; @(#)$Id: hello1.scm,v 1.1 1998/07/31 11:40:35 qfwfq Exp $

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_msg.scm")
(rp:use-macro-package "w_dc.scm")

(define *message*
  (if (null? (cdr *invocation-arg*))
    "Hello world"
    (cadr *invocation-arg*)))

(define draw-window
  (let ((text-buffer (rp:export-string *message*))
	(text-length (string-length *message*))
	(ps (win:paintstruct-create)))
    (lambda (wnd)
      (rp:ww-do-paint wnd ps
	(lambda (dc) (rp:win32api-text-out dc 0 0 text-buffer text-length)
		     0)))))

(define the-class
  (rp:ww-make-window-class
    (rp:ww-message-dispatcher (wnd message wparam lparam)
      ((win:wm paint) (draw-window wnd))
      ((win:wm destroy)
       (rp:win32api-post-quit-message 0)
       0))))

(define main-win
  (rp:ww-create-window the-class "rhizome/pi \"Hello world\" application" #f))

(define main-loop (rp:ww-message-loop-create))

(exit (rp:ww-message-loop-run main-loop))
