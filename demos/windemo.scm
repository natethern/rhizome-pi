;;; $Id: windemo.scm,v 1.1 1997/10/16 06:23:39 qfwfq Exp $

;;; This is a step by step translation of typical win32 application

(rp:eval-in-compiler-environment
  (begin (define-syntax win:id
	   (syntax-rules ()
	     ((_ dom id) (rp:cast-integer->buffer (dom id)))))
	 (rp:declare-constants cs
	   (vredraw 1)
	   (hredraw 2))
	 (rp:declare-constants ws
	   (overlapped-window #xcf0000))
	 (rp:declare-constants cw
	   (use-default #x-80000000))
	 (rp:declare-constants sw
	   (show-default 10))
	 (rp:declare-constants wm
	   (destroy 2)
	   (paint 15))
	 (rp:declare-constants id:i
	   (application 32512))
	 (rp:declare-constants id:c
	   (arrow 32512))
	 (rp:declare-constants id:color
	   (window (+ 5 1)))
	 (rp:define-buffer-structure
	   (win:wndclass
	     style proc mname cname)
	   (style cardinal style)
	   (wnd-proc procedure proc)
	   (cls-extra integer 0)
	   (wnd-extra integer 0)
	   (instance buffer (get-module-handle #f))
	   (icon buffer (load-icon #f (win:id id:i application)))
	   (cursor buffer (load-cursor #f (win:id id:c arrow)))
	   (background buffer (win:id id:color window))
	   (menu-name buffer mname)
	   (class-name buffer (rp:export-string cname)))
	 (rp:define-buffer-structure win:point
	   (x integer)
	   (y integer))
	 (rp:define-buffer-structure win:rect
	   (left integer)
	   (top integer)
	   (right integer)
	   (bottom integer))
	 (rp:define-buffer-structure win:msg
	   (wnd buffer)
	   (message cardinal)
	   (wparam cardinal)
	   (lparam integer)
	   (time cardinal)
	   (pt structure win:point))
	 (rp:define-buffer-structure win:paintstruct
	   (dc buffer)
	   (erase integer)
	   (paint structure win:rect)
	   (restore integer)
	   (inc-update integer)
	   (rgb-reserved byte-array 32))))

(define m-kernel (rp:load-external-object "kernel32.dll"))
(define m-user (rp:load-external-object "user32.dll"))
(define m-gdi (rp:load-external-object "gdi32.dll"))

(define get-module-handle (rp:external-procedure m-kernel "GetModuleHandleA" buffer (buffer)))
(define load-icon (rp:external-procedure m-user "LoadIconA" buffer (buffer buffer)))
(define load-cursor (rp:external-procedure m-user "LoadCursorA" buffer (buffer buffer)))
(define register-class (rp:external-procedure m-user "RegisterClassA" cardinal (buffer)))
(define create-window-ex (rp:external-procedure m-user "CreateWindowExA" buffer
			   (cardinal buffer buffer cardinal integer integer integer integer
			    buffer buffer buffer buffer)))
(define show-window (rp:external-procedure m-user "ShowWindow" integer (buffer integer)))
(define update-window (rp:external-procedure m-user "UpdateWindow" integer (buffer)))
(define get-message (rp:external-procedure m-user "GetMessageA" integer (buffer buffer cardinal cardinal)))
(define translate-message (rp:external-procedure m-user "TranslateMessage" integer (buffer)))
(define dispatch-message (rp:external-procedure m-user "DispatchMessageA" integer (buffer)))
(define def-window-proc (rp:external-procedure m-user "DefWindowProcA" integer (buffer cardinal cardinal integer)))
(define post-quit-message (rp:external-procedure m-user "PostQuitMessage" effect (integer)))
(define begin-paint (rp:external-procedure m-user "BeginPaint" buffer (buffer buffer)))
(define end-paint (rp:external-procedure m-user "EndPaint" integer (buffer buffer)))
(define text-out (rp:external-procedure m-gdi "TextOutA" integer (buffer integer integer buffer integer)))

(define (init-application)
  (let ((wc (win:wndclass-create
	      (+ (cs vredraw) (cs hredraw))
	      (rp:make-entry integer ((wnd buffer) (message cardinal) (wparam cardinal) (lparam integer))
		(window-proc wnd message wparam lparam))
	      #f "Windemo")))
    (register-class wc)))

(define (init-instance)
  (let ((wnd (create-window-ex 0
			       (rp:export-string "Windemo")
			       (rp:export-string "Rhizome/pi Win32 API demo")
			       (ws overlapped-window)
			       (cw use-default) 0 (cw use-default) 0
			       #f #f (get-module-handle #f) #f)))
    (show-window wnd (sw show-default))
    (update-window wnd)))

(define (message-loop)
  (let ((msg (win:msg-create)))
    (do () ((zero? (get-message msg #f 0 0)) (win:msg-load-wparam msg))
      (translate-message msg)
      (dispatch-message msg))))

(define window-proc
  (let ((ps (win:paintstruct-create)))
    (lambda (wnd message wparam lparam)
      (cond ((= message (wm paint))
	     (let ((dc (begin-paint wnd ps)))
	       (draw dc ps)
	       (end-paint wnd ps))
	     0)
	    ((= message (wm destroy))
	     (post-quit-message 0) 0)
	    (else (def-window-proc wnd message wparam lparam))))))

(define draw
  (if (null? (cdr *invocation-arg*)) (lambda (dc ps) #f)
    (let ((s (rp:export-string (cadr *invocation-arg*)))
	  (l (string-length (cadr *invocation-arg*))))
      (lambda (dc ps)
	(text-out dc 0 0 s l)))))

(init-application)
(init-instance)
(message-loop)
