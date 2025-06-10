;;; $Id: windemo0.scm,v 1.1 2002/09/27 10:33:23 qfwfq Exp $

; Samples of rather simple windows API calls

(define m-user (rp:load-external-object "user32.dll"))
(define m-comdlg (rp:load-external-object "comdlg32.dll"))

; (message-box "Message") displays message box with text "Message"

(define api-message-box
  (rp:external-procedure m-user "MessageBoxA" integer (buffer buffer buffer cardinal)))

(define (message-box text)
  (let ((txtbuf (rp:export-string text)))
    (let ((res (api-message-box #f txtbuf #f 0)))	; hWnd=NULL lpCaption=NULL uType=MB_OK
      (rp:destroy-external-buffer txtbuf)
      res)))

; (enumerate-windows) returns list of lists of thread id, process id, text and position of client area
;  for all top-level windows

(define api-enum-windows
  (rp:external-procedure m-user "EnumWindows" integer (procedure integer)))
(define api-get-window-thread-process-id
  (rp:external-procedure m-user "GetWindowThreadProcessId" cardinal (buffer buffer)))
(define api-get-window-text
  (rp:external-procedure m-user "GetWindowTextA" integer (buffer buffer integer)))
(define api-get-client-rect
  (rp:external-procedure m-user "GetClientRect" integer (buffer buffer)))
(define api-client-to-screen
  (rp:external-procedure m-user "ClientToScreen" integer (buffer buffer)))

(rp:eval-in-compiler-environment	; structure declarations
  (begin (rp:define-buffer-structure s-point
	   (x integer)
	   (y integer))
	 (rp:define-buffer-structure s-rect
	   (left integer)
	   (top integer)
	   (right integer)
	   (bottom integer))))

(define (enumerate-windows)
  (let ((winlst '())
	(bprocid (rp:make-external-buffer 1)) (btext (rp:make-external-buffer 20))
	(brect (s-rect-create)) (bpoint (s-point-create)))
    (let ((enumproc
	   (rp:make-entry integer ((hwnd buffer) (lparam integer))
	     (let* ((tid (api-get-window-thread-process-id hwnd bprocid))
		    (pid (rp:cardinal-array-load bprocid 0))
		    (tlen (api-get-window-text hwnd btext 80))
		    (text (rp:load-external-chars btext 0 tlen)))
	       (api-get-client-rect hwnd brect)
	       (s-rect-let-values brect ((left left) (top top) (right right) (bottom bottom))
		 (s-point-set-values bpoint (x left) (y top))
		 (api-client-to-screen hwnd bpoint)
		 (s-point-let-values bpoint ((left x) (top y))
		   (s-point-set-values bpoint (x right) (y bottom))
		   (api-client-to-screen hwnd bpoint)
		   (s-point-let-values bpoint ((right x) (bottom y))
		     (set! winlst (cons `(,tid ,pid ,text ,left ,top ,right ,bottom) winlst))))))
	     1)))
      (api-enum-windows enumproc 0)
      (rp:destroy-exported-procedure enumproc)
      (rp:destroy-external-buffer bprocid) (rp:destroy-external-buffer btext)
      (rp:destroy-external-buffer brect) (rp:destroy-external-buffer bpoint)
      winlst)))

; (get-open-file-name) shows file open common dialog and returns file name

(define api-get-open-file-name
  (rp:external-procedure m-comdlg "GetOpenFileNameA" integer (buffer)))

(rp:eval-in-compiler-environment	; OPENFILENAME structure
  (rp:define-buffer-structure s-openfilename
    (struct-size cardinal (s-openfilename-size))
    (owner buffer)
    (instance buffer)
    (filter buffer)
    (custom-filter buffer)
    (max-cust-filter cardinal)
    (filter-index cardinal)
    (file buffer)
    (max-file cardinal)
    (file-title buffer)
    (max-file-title cardinal)
    (initial-dir buffer)
    (title buffer)
    (flags cardinal)
    (file-offset short-cardinal)
    (file-extension short-cardinal)
    (def-ext buffer)
    (cust-data integer)
    (hook procedure)
    (template-name buffer)))

(define (get-open-file-name)
  (let ((bof (s-openfilename-create
	       (owner #f)
	       (filter (rp:export-string
			 (rp:fancy-string "Scheme Programs (*.scm)" 0 "*.scm" 0 "All Files (*.*)" 0 "*.*" 0)))
	       (custom-filter #f)
	       (filter-index 1)
	       (file (rp:make-external-buffer 64))
	       (max-file 256)
	       (file-title #f)
	       (initial-dir #f)
	       (title #f)
	       (flags 0)
	       (def-ext #f))))
    (rp:store-external-chars (s-openfilename-load-file bof) 0 "" #f)
    (let ((path (if (zero? (api-get-open-file-name bof)) #f
		  (rp:load-external-chars (s-openfilename-load-file bof) 0 #f))))
      (rp:destroy-external-buffer (s-openfilename-load-filter bof))
      (rp:destroy-external-buffer (s-openfilename-load-file bof))
      (rp:destroy-external-buffer bof)
      path)))
