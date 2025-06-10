;;;; @(#)$Header: /u/master/rhizome/config/win32/lcc/config.scm,v 1.1 2002/09/27 08:52:35 qfwfq Exp $
; $Log: config.scm,v $
; Revision 1.1  2002/09/27 08:52:35  qfwfq
; Add support of linux and lcc-win32.
;

; character used for separate diractory names and file name
(define-syntax cm-path-separate-char (syntax-rules () ((_) #\\)))

; character used for separate directory lists in search paths
(define-syntax cm-list-separate-char (syntax-rules () ((_) #\;)))

; whether shared runtime works
(define-syntax cm-always-static (syntax-rules () ((_) #f)))

; C compile command
(define-syntax cc-command-str
  (syntax-rules (static)
    ((_ static) "lcc -O -nw -DWIN32 -DRK_BIND_STATIC")
    ((_ s) "lcc -O -nw -DWIN32")))

; expression to make C compile command
(define-syntax cm-cc-command
  (syntax-rules ()
    ((_ s libdir)
     (string-append (cc-command-str s) (if libdir (string-append " -I" libdir) "") " -c"))))

; command line for C compile
(define-syntax cm-cc-line
  (syntax-rules ()
    ((_ cmd input) (string-append cmd " " input))))

; link command
(define-syntax ld-command-str
  (syntax-rules (con gui share)
    ((_ con) "lcclnk -subsystem console")
    ((_ gui) "lcclnk -subsystem windows")
    ((_ share) "lcclnk -dll")))

; expression to make link command
(define-syntax cm-ld-command
  (syntax-rules ()
    ((_ s libdir) (ld-command-str s))))

; link library
(define-syntax ld-lib-str
  (syntax-rules (dynamic loadable)
    ((_ dynamic) (lambda (ap) (string-append (ap "statmain.obj") " " (ap "rhzpilc0.lib"))))
    ((_ loadable) (lambda (ap) (ap "rhzpilc0.lib")))
    ((_ s) (lambda (ap) (string-append (ap "rhzscm.lib") " " (ap "rhzpi.lib") " " (ap "rhizome.lib"))))))

; expression to make library option string
(define-syntax cm-ld-lib
  (syntax-rules ()
    ((_ s libdir)
     ((ld-lib-str s) (lambda (lb) (if libdir (string-append libdir "\\" lb) lb))))))

; option to specify base address
(define-syntax cm-add-base-option
  (syntax-rules ()
    ((_ result addr) result)))

; option to specify executable name
(define-syntax output-option-str (syntax-rules () ((_) " -o ")))

; add executable name option
(define-syntax cm-add-output-option
  (syntax-rules ()
    ((_ result out)
     (string-append result (output-option-str) out))))

; suffix for object file
(define-syntax obj-suffix-str (syntax-rules () ((_) ".obj")))

; add C source module
(define-syntax cm-add-module
  (syntax-rules ()
    ((_ result module)
     (string-append result " " (rp:basename module ".c") (obj-suffix-str)))))

; default excutable/loadable file suffix
(define-syntax cm-default-exe-suffix
  (syntax-rules (share)
    ((_ share) ".dll")
    ((_ s) ".exe")))

; value of system to exit status
(define-syntax cm-exit-status (syntax-rules () ((_ s) s)))

; platform idintification
(define-syntax cm-platform-id (syntax-rules () ((_) '(windows i386 "Win32"))))

; name of environment variable pointing library directory
(define-syntax cm-lib-environment-var (syntax-rules () ((_) "RHIZOME_LIB")))

; name of environment variable specifying macro search path
(define-syntax cm-macro-path-var (syntax-rules () ((_) "RHIZOME_MACRO_PATH")))

; name of environment variable containing startup command
(define-syntax cm-startup-cmd-var (syntax-rules () ((_) "RHIZOME_PI_RC")))

; SIGINT signal number
(define-syntax cm-sigint-no (syntax-rules () ((_) 2)))
