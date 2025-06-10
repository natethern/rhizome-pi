;;;; @(#)$Id: minimal.scm,v 1.1 1998/07/31 11:40:37 qfwfq Exp $

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_msg.scm")

(define the-class
  (rp:ww-make-window-class
    (rp:ww-message-dispatcher (wnd message wparam lparam)
      ((win:wm destroy)
       (rp:win32api-post-quit-message 0)
       0))))

(define main-win
  (rp:ww-create-window the-class "rhizome/pi minimal application" #f))

(define main-loop (rp:ww-message-loop-create))

(exit (rp:ww-message-loop-run main-loop))
