;;;; @(#)$Header: /u/master/rhizome/config/unix/linux/config.scm,v 1.3 2005/11/10 08:47:34 qfwfq Exp $
; $Log: config.scm,v $
; Revision 1.3  2005/11/10 08:47:34  qfwfq
; Option to distinguish pointers by function alignment.
;
; Revision 1.2  2004/07/23 04:46:31  qfwfq
; change -m option in CFLAGS
;
; Revision 1.1  2002/09/27 08:37:28  qfwfq
; Add support of linux and lcc-win32.
;

; character used for separate diractory names and file name
(define-syntax cm-path-separate-char (syntax-rules () ((_) #\/)))

; character used for separate directory lists in search paths
(define-syntax cm-list-separate-char (syntax-rules () ((_) #\:)))

; whether shared runtime works
(define-syntax cm-always-static (syntax-rules () ((_) #f)))

; C compile command
(define-syntax cc-command-str
  (syntax-rules (share static)
    ((_ share) "gcc -fpic -O2 -mtune=i686 -falign-functions")
    ((_ static) "gcc -O2 -mtune=i686 -falign-functions -DRK_BIND_STATIC")
    ((_ s) "gcc -O2 -mtune=i686 -falign-functions")))

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
  (syntax-rules (share)
    ((_ share) "ld -Bshareable")
    ((_ s) "gcc")))

; expression to make link command
(define-syntax cm-ld-command
  (syntax-rules (share)
    ((_ share libdir)
     (string-append (ld-command-str share) (if libdir (string-append " -L" libdir " -R" libdir) "")))
    ((_ s libdir)
     (string-append (ld-command-str s) (if libdir (string-append " -L" libdir " -Wl,-R" libdir) "")))))

; link library
(define-syntax ld-lib-str
  (syntax-rules (dynamic loadable)
    ((_ dynamic) "-lrhzpisys -lpthread -lm -ldl")
    ((_ loadable) #f)
    ((_ s) "-lrhzscm -lrhzpi -lrhizome -lpthread -lm -ldl")))

; expression to make library option string
(define-syntax cm-ld-lib
  (syntax-rules ()
    ((_ s libdir) (ld-lib-str s))))

; option to specify base address
(define-syntax cm-add-base-option
  (syntax-rules ()
    ((_ result addr)
     (string-append result " -T " addr))))

; option to specify executable name
(define-syntax output-option-str (syntax-rules () ((_) " -o ")))

; add executable name option
(define-syntax cm-add-output-option
  (syntax-rules ()
    ((_ result out)
     (string-append result (output-option-str) out))))

; suffix for object file
(define-syntax obj-suffix-str (syntax-rules () ((_) ".o")))

; add C source module
(define-syntax cm-add-module
  (syntax-rules ()
    ((_ result module)
     (string-append result " " (rp:basename module ".c") (obj-suffix-str)))))

; default excutable/loadable file suffix
(define-syntax cm-default-exe-suffix
  (syntax-rules (share)
    ((_ share) ".so")
    ((_ s) #f)))

; value of system to exit status
(define-syntax cm-exit-status (syntax-rules () ((_ s) (quotient s 256))))

; platform idintification
(define-syntax cm-platform-id
  (syntax-rules ()
    ((_) (list
	   'unix
	   (call-with-input-file "|uname -m" read)
	   (call-with-input-file "|echo \"\\\"`uname -sr`\\\"\"" read)))))

; name of environment variable pointing library directory
(define-syntax cm-lib-environment-var (syntax-rules () ((_) "RHIZOME_LIB")))

; name of environment variable specifying macro search path
(define-syntax cm-macro-path-var (syntax-rules () ((_) "RHIZOME_MACRO_PATH")))

; name of environment variable containing startup command
(define-syntax cm-startup-cmd-var (syntax-rules () ((_) "RHIZOME_PI_RC")))

; SIGINT signal number
(define-syntax cm-sigint-no (syntax-rules () ((_) 2)))
