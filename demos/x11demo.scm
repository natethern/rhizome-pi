;;;; $Id: x11demo.scm,v 1.3 2004/04/09 04:42:53 qfwfq Exp $

;;; This is a step by step translation of typical Xt application

(rp:eval-in-compiler-environment
  (begin
    (define prepend-underscore #f)	; change to #t if necessary
    (define-syntax libsym
      (lambda (x)
	(syntax-case x ()
	  ((_ s)
	   (if prepend-underscore
	     (string-append "_" (syntax-object->datum (syntax s)))
	     (syntax s))))))))

(rp:eval-in-compiler-environment
  (begin (rp:define-buffer-structure x:gc-values		; Xlib.h
	   (function integer)
	   (plane-mask cardinal)
	   (foreground cardinal)
	   (background cardinal)
	   (line-width integer)
	   (line-style integer)
	   (cap-style integer)
	   (join-style integer)
	   (fill-style integer)
	   (fill-rule integer)
	   (arc-mode integer)
	   (tile cardinal)
	   (stipple cardinal)
	   (ts-x-origin integer)
	   (ts-y-origin integer)
	   (font cardinal)
	   (subwindow-mode integer)
	   (graphics-exposures integer)
	   (clip-x-origin integer)
	   (clip-y-origin integer)
	   (clip-mask cardinal)
	   (dash-offset integer)
	   (dashes byte-array 1))
	 (rp:declare-constants gc				; X.h
	   (foreground (expt 2 2)))
	 (rp:define-buffer-structure
	   (xt:resource						; Intrinsic.h
	     name class type size offset def-type def-addr)
	   (resource-name buffer (rp:export-string name))
	   (resource-class buffer (rp:export-string class))
	   (resource-type buffer (rp:export-string type))
	   (resource-size cardinal size)
	   (resource-offset cardinal offset)
	   (default-type buffer (rp:export-string def-type))
	   (default-addr buffer def-addr))
	 (rp:define-buffer-structure
	   (xt:actions-rec string proc)
	   (string buffer (rp:export-string string))
	   (proc procedure proc))
	 (rp:declare-constants xt:
	   (default-foreground "XtDefaultForeground"))
	 (rp:define-buffer-structure xt:tm-rec			; IntrinsicP.h
	   (translations buffer)
	   (proc-table buffer)
	   (current-state buffer)
	   (last-event-time cardinal))
	 (rp:define-buffer-structure core:part			; CoreP.h
	   (self buffer)
	   (widget-class buffer)
	   (parent buffer)
	   (xrm-name integer)
	   (being-destroyed byte-array 1)
	   (destroy-callbacks buffer)
	   (constraints buffer)
	   (x short-integer) (y short-integer)
	   (width short-cardinal) (height short-cardinal)
	   (border-width short-cardinal)
	   (managed byte-array 1)
	   (sensitive byte-array 1)
	   (ancestor-sensitive byte-array 1)
	   (event-table buffer)
	   (tm structure xt:tm-rec)
	   (accelerators buffer)
	   (border-pixel cardinal)
	   (border-pixmap cardinal)
	   (popup-list buffer)
	   (num-popups cardinal)
	   (name buffer)
	   (screen buffer)
	   (colormap cardinal)
	   (window cardinal)
	   (depth cardinal)
	   (background-pixel cardinal)
	   (background-pixmap cardinal)
	   (visible byte-array 1)
	   (mapped-when-managed byte-array 1))
	 (rp:define-buffer-structure
	   (core:class-part
	     superclass name size initialize realize actions nactions
	     resources nresources destroy resize expose set-values)
	   (superclass buffer superclass)
	   (class-name buffer (rp:export-string name))
	   (widget-size cardinal size)
	   (class-initialize procedure #f)
	   (class-part-initialize procedure #f)
	   (class-inited byte-array 1 (rp:fancy-string 0))
	   (initialize procedure initialize)
	   (initialize-hook procedure #f)
	   (realize procedure realize)
	   (actions buffer actions)
	   (num-actions cardinal nactions)
	   (resources buffer resources)
	   (num-resources cardinal nresources)
	   (xrm-class integer 0)
	   (compress-motion byte-array 1 (rp:fancy-string 1))
	   (compress-exposure byte-array 1 (rp:fancy-string 1))
	   (compress-enterleave byte-array 1 (rp:fancy-string 1))
	   (visible-interest byte-array 1 (rp:fancy-string 0))
	   (destroy procedure destroy)
	   (resize procedure resize)
	   (expose procedure expose)
	   (set-values procedure set-values)
	   (set-values-hook procedure #f)
	   (set-values-almost procedure xt:inherit)
	   (get-values-hook procedure #f)
	   (accept-focus procedure #f)
	   (version cardinal 11006)
	   (callback-private buffer #f)
	   (tm-table buffer #f)
	   (query-geometry procedure xt:inherit)
	   (display-accelerator procedure xt:inherit)
	   (extension buffer #f))
	 (rp:declare-constants xt:n				; StringDefs.h
	   (foreground "foreground"))
	 (rp:declare-constants xt:c
	   (foreground "Foreground"))
	 (rp:declare-constants xt:r
	   (pixel "Pixel") (string "String"))
	 (rp:define-buffer-structure simple:part		; Xaw/SimpleP.h
	   (cursor cardinal)
	   (insensitive-border cardinal)
	   (cursor-name buffer)
	   (pointer-fg cardinal)
	   (pointer-bg cardinal)
	   (international byte-array 1))
	 (rp:define-buffer-structure simple:class-part
	   (change-sinsitive procedure xt:inherit))
	 (rp:define-buffer-structure logo:part			; LogoP.h
	   (fgpixel cardinal)
	   (fore-gc buffer)
	   (back-gc buffer))
	 (rp:define-buffer-structure logo:rec
	   (core structure core:part)
	   (simple structure simple:part)
	   (logo structure logo:part))
	 (rp:define-buffer-structure logo:class-part
	   (dummy integer 0))
	 (rp:define-buffer-structure
	   (logo:class-rec initialize realize destroy resize expose set-values)
	   (core-class structure core:class-part
	     (#t simple:class-rec "Logo" (logo:rec-size) initialize realize #f 0
		 (car resources) (cdr resources) destroy resize expose set-values))
	   (simple-class structure simple:class-part (#t))
	   (logo-class structure logo:class-part (#t)))))

(define m-x11 (rp:load-external-object "libX11.so"))
(define m-xt (rp:load-external-object "libXt.so"))
(define m-xmu (rp:load-external-object "libXmu.so"))
(define m-xaw (rp:load-external-object "libXaw.so"))

(define xt:inherit (rp:import-procedure m-xt (libsym "_XtInherit")))
(define application-shell:class-rec
  (rp:cast-procedure->buffer (rp:import-procedure m-xt (libsym "applicationShellClassRec"))))
(define simple:class-rec (rp:cast-procedure->buffer (rp:import-procedure m-xaw (libsym "simpleClassRec"))))

(define x:close-display (rp:external-procedure m-x11 (libsym "XCloseDisplay") integer (buffer)))
(define xt:display (rp:external-procedure m-xt (libsym "XtDisplay") buffer (buffer)))
(define xt:window (rp:external-procedure m-xt (libsym "XtWindow") cardinal (buffer)))
(define xt:get-gc (rp:external-procedure m-xt (libsym "XtGetGC") buffer (buffer cardinal buffer)))
(define xt:open-application (rp:external-procedure m-xt (libsym "XtOpenApplication") buffer
			      (buffer buffer buffer cardinal buffer buffer buffer buffer buffer cardinal)))
(define xt:app-add-actions (rp:external-procedure m-xt (libsym "XtAppAddActions") effect (buffer buffer cardinal)))
(define xt:widget-to-application-context (rp:external-procedure m-xt (libsym "XtWidgetToApplicationContext") buffer
						(buffer)))
(define xt:override-translations (rp:external-procedure m-xt (libsym "XtOverrideTranslations") effect (buffer buffer)))
(define xt:parse-translation-table (rp:external-procedure m-xt (libsym "XtParseTranslationTable") buffer (buffer)))
(define xt:create-managed-widget (rp:external-procedure m-xt (libsym "XtCreateManagedWidget") buffer
				   (buffer buffer buffer buffer cardinal)))
(define xt:realize-widget (rp:external-procedure m-xt (libsym "XtRealizeWidget") effect (buffer)))
(define xt:app-main-loop (rp:external-procedure m-xt (libsym "XtAppMainLoop") effect (buffer)))
(define xt:release-gc (rp:external-procedure m-xt (libsym "XtReleaseGC") effect (buffer buffer)))
(define xmu:draw-logo (rp:external-procedure m-xmu (libsym "XmuDrawLogo") effect
			(buffer cardinal buffer buffer integer integer cardinal cardinal)))

;;; Widget definition

(define resources
  (xt:resource-create-array
    ((xt:n foreground)
     (xt:c foreground)
     (xt:r pixel)
     4			; sizeof(Pixel)
     (+ (logo:rec-offsets logo) (logo:part-offsets fgpixel))
     (xt:r string)
     (rp:export-string (xt: default-foreground)))))

(define logo:class-rec
  (logo:class-rec-create
    (rp:make-entry effect ((request buffer) (new buffer) (args buffer) (num-args buffer))
      (initialize request new args num-args))
    (rp:make-entry effect ((gw buffer) (valuemaskp buffer) (attr buffer))
      (realize gw valuemaskp attr))
    (rp:make-entry effect ((gw buffer))
      (destroy gw))
    (rp:make-entry effect ((gw buffer)) #f)
    (rp:make-entry effect ((gw buffer) (event buffer) (region buffer))
      (re-display gw event region))
    (rp:make-entry integer ((gcurrent buffer) (grequest buffer) (gnew buffer) (args buffer) (num-args buffer))
      (set-values gcurrent grequest gnew args num-args))))

(define create-gcs
  (let ((v (x:gc-values-create)))
    (lambda (w)
      (x:gc-values-store-foreground v (logo:part-load-fgpixel (logo:rec-get-logo w)))
      (logo:part-store-fore-gc (logo:rec-get-logo w) (xt:get-gc w (gc foreground) v))
      (x:gc-values-store-foreground v (core:part-load-background-pixel (logo:rec-get-core w)))
      (logo:part-store-back-gc (logo:rec-get-logo w) (xt:get-gc w (gc foreground) v)))))

(define (initialize request w args num-args)
  (if (< (core:part-load-width (logo:rec-get-core w)) 1)
      (core:part-store-width (logo:rec-get-core w) 100))
  (if (< (core:part-load-height (logo:rec-get-core w)) 1)
      (core:part-store-height (logo:rec-get-core w) 100))
  (logo:part-set-values (logo:rec-get-logo w) (fore-gc #f) (back-gc #f)))

(define (destroy w)
  (logo:part-let-values (logo:rec-get-logo w) ((fore-gc fore-gc) (back-gc back-gc))
    (cond (fore-gc (xt:release-gc w fore-gc) (logo:part-store-fore-gc (logo:rec-get-logo w) #f)))
    (cond (back-gc (xt:release-gc w back-gc) (logo:part-store-back-gc (logo:rec-get-logo w) #f)))))

(define (realize w valuemaskp attr)
  (create-gcs w)
  ((rp:entry->procedure (core:class-part-load-realize
			  (core:class-part-load-superclass
			    (logo:class-rec-get-core-class logo:class-rec))) effect (buffer buffer buffer))
   w valuemaskp attr))

(define (re-display w event region)
  (logo:part-let-values (logo:rec-get-logo w) ((fore-gc fore-gc) (back-gc back-gc))
    (core:part-let-values (logo:rec-get-core w) ((width width) (height height))
      (xmu:draw-logo (xt:display w) (xt:window w) fore-gc back-gc 0 0 width height))))

(define (set-values current request new args num-args)
  (if (or (not (= (logo:part-load-fgpixel (logo:rec-get-logo new))
		  (logo:part-load-fgpixel (logo:rec-get-logo current))))
	  (not (= (core:part-load-background-pixel (logo:rec-get-core new))
		  (core:part-load-background-pixel (logo:rec-get-core current)))))
    (begin (destroy new) (create-gcs new) 1)
    0))

;;; Application code

(define actions
  (xt:actions-rec-create-array
    ("quit"
     (rp:make-entry effect ((w buffer) (event buffer) (params buffer) (num-params buffer))
       (quit w event params num-params)))))

(define (main args-list)
  (let ((n (length args-list)))
    (let ((app-con (rp:make-external-buffer 1))
	  (argc (rp:make-external-buffer 1))
	  (argv (rp:make-external-buffer (+ n 1))))
      (rp:integer-array-store argc 0 n)
      (do ((i 0 (+ i 1)) (a args-list (cdr a)))
	((= i n) (rp:buffer-array-store argv i #f))
	(rp:buffer-array-store argv i (rp:export-string (car a))))
      (let ((toplevel (xt:open-application app-con (rp:export-string "PiX11Demo")
					   #f 0 argc argv #f application-shell:class-rec #f 0)))
	(xt:app-add-actions (xt:widget-to-application-context toplevel) (car actions) (cdr actions))
	(let ((demo (xt:create-managed-widget (rp:export-string "piX11Demo") logo:class-rec toplevel #f 0)))
	  (xt:override-translations demo (xt:parse-translation-table (rp:export-string "<Key>q: quit()"))))
	(xt:realize-widget toplevel)
	(xt:app-main-loop (rp:buffer-array-load app-con 0))))))

(define (quit w event params num-params)
  (x:close-display (xt:display w))
  (exit 0))

(main *invocation-arg*)
