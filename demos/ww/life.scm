;;;; @(#)$Id: life.scm,v 1.2 1999/02/15 08:53:59 qfwfq Exp $

(rp:use-macro-package "w_base.scm")
(rp:use-macro-package "w_class.scm")
(rp:use-macro-package "w_wstyle.scm")
(rp:use-macro-package "w_c_btn.scm")
(rp:use-macro-package "w_c_edit.scm")
(rp:use-macro-package "w_msg.scm")
(rp:use-macro-package "w_dc.scm")
(rp:use-macro-package "w_scroll.scm")
(rp:use-macro-package "w_wmove.scm")
(rp:use-macro-package "w_menu.scm")
(rp:use-macro-package "w_key.scm")

(rp:eval-in-compiler-environment
  (rp:declare-constants idc
    (run 1000) (stop 1001)
    (forever 1010) (step 1011) (nsteps 1012)
    (nopause 1020) (pause 1021) (pauselen 1022)
    (fullscreen 1030)
    (edit 1100) (edit-done 1101) (edit-cancel 1102)
    (clear-view 1110) (clear-all 1111)
    (load 1120) (save 1121)
    (cellsize 1200) (setsize 1201)
    (lmost 1300) (rmost 1301) (tmost 1302) (bmost 1303)
    (centerx 1310) (centery 1311) (xygoto 1312)
    (about 1400) (exit 1401)))

(rp:eval-in-compiler-environment
  (rp:declare-constants appmsg
    (stop (+ (win:wm user) 0)) (play (+ (win:wm user) 1))))

(define *console-mode* 'ready)

(define *current-pattern* '())
(define *pattern-modified?* #f)
(define *nsteps* 0)

(define *pattern-leftmost* 0)
(define *pattern-topmost* 0)
(define *pattern-rightmost* 0)
(define *pattern-bottommost* 0)

(define *pattern-width* 0)
(define *pattern-height* 0)

(define (get-pattern-dims)
  (if (null? *current-pattern*)
    (begin (set! *pattern-leftmost* 0)
	   (set! *pattern-topmost* 0)
	   (set! *pattern-rightmost* 0)
	   (set! *pattern-bottommost* 0))
    (let ((last-elt (lambda (l) (do ((x (car l) (car t)) (t (cdr l) (cdr t))) ((null? t) x)))))
      (set! *pattern-leftmost* (caar *current-pattern*))
      (set! *pattern-topmost* (apply min (map caadr *current-pattern*)))
      (set! *pattern-rightmost* (+ (car (last-elt *current-pattern*)) 1))
      (set! *pattern-bottommost* (+ (apply max (map (lambda (l) (car (last-elt (cdr l)))) *current-pattern*)) 1))))
  (set! *pattern-width* (- *pattern-rightmost* *pattern-leftmost*))
  (set! *pattern-height* (- *pattern-bottommost* *pattern-topmost*)))

(define play-life-game
  (let ((rule-d '#(#f #f #f #t #f #f #f #f #f))
	(rule-l '#(#f #f #f #t #t #f #f #f #f #f))
	(dummyl '((#f . -1) . ()))
	(lesser (lambda (a b c) (let ((minor (lambda (x y) (if x (if y (min x y) x) y)))) (minor (minor a b) c)))))
    (letrec ((make-pat (lambda (p) (let ((x (caar p)) (c3 (cdar p)))
				     (add-col (- x 1) (make-col dummyl dummyl c3) (make-cols x dummyl c3 (cdr p))))))
	     (make-cols (lambda (x c1 c2 p)
			  (cond ((null? p) (add-col x (make-col c1 c2 dummyl)
						    (add-col (+ x 1) (make-col c2 dummyl dummyl) '())))
				((= (+ x 1) (caar p))
				 (let ((c3 (cdar p)))
				   (add-col x (make-col c1 c2 c3) (make-cols (+ x 1) c2 c3 (cdr p)))))
				((= (+ x 2) (caar p))
				 (let ((c4 (cdar p)))
				   (add-col x (make-col c1 c2 dummyl)
					    (add-col (+ x 1) (make-col c2 dummyl c4)
						     (make-cols (+ x 2) dummyl c4 (cdr p))))))
				(else (add-col x (make-col c1 c2 dummyl)
					       (add-col (+ x 1) (make-col c2 dummyl dummyl) (make-pat p)))))))
	     (add-col (lambda (x l r) (if (null? l) r (cons (cons x l) r))))
	     (nxt (lambda (l) (let ((t (cdr l))) (if (null? t) dummyl t))))
	     (make-col (lambda (c1 c2 c3)
			 (let ((y1 (caar c1)) (y2 (caar c2)) (y3 (caar c3)))
			   (make-cells (- (lesser y1 y2 y3) 1) 0 rule-d 0 0
				       y1 y2 y3 (nxt c1) (nxt c2) (nxt c3) (+ (cdar c2) 1)))))
	     (make-cells
	      (lambda (y g rule n1 n2 y1 y2 y3 t1 t2 t3 gn)
		(let ((y0 (+ y 1)) (n3 0) (rule0 rule-d) (g0 0))
		  (let ((count (lambda (test do) (if test (begin (set! n3 (+ n3 1)) (do))))))
		    (count (eqv? y0 y1) (lambda () (set! y1 (caar t1)) (set! t1 (nxt t1))))
		    (count (eqv? y0 y2) (lambda () (set! rule0 rule-l) (set! g0 gn) (set! gn (+ (cdar t2) 1))
						   (set! y2 (caar t2)) (set! t2 (nxt t2))))
		    (count (eqv? y0 y3) (lambda () (set! y3 (caar t3)) (set! t3 (nxt t3))))
		    (let ((b (if (zero? (+ n2 n3))
			       (let ((yn (lesser y1 y2 y3)))
				 (if yn (make-cells (- yn 1) 0 rule-d 0 0 y1 y2 y3 t1 t2 t3 gn) '()))
			       (make-cells y0 g0 rule0 n2 n3 y1 y2 y3 t1 t2 t3 gn))))
		      (if (vector-ref rule (+ n1 n2 n3)) (cons (cons y g) b) b)))))))
      (lambda ()
	(if (not (null? *current-pattern*))
	  (begin (set! *current-pattern* (make-pat *current-pattern*))
		 (set! *nsteps* (+ *nsteps* 1))))
	(get-pattern-dims)))))

(define (make-pattern lp)
  (set! *current-pattern* (map (lambda (l) (cons (car l) (map (lambda (n) (cons n 0)) (cdr l)))) lp))
  (get-pattern-dims))

(define (pattern-strip-gen)
  (map (lambda (l) (cons (car l) (map car (cdr l)))) *current-pattern*))

(define *pattern-document*
  (rp:object-constructor
    ((doc (rp:ww-document-create
	    'open-dialog-flags '(file-must-exist hide-readonly)
	    'dialog-option '(filters (("Life pattern files(*.lp)" "*.lp") ("All files(*.*)" "*.*"))
			     custom-filter-title "Other Types"
			     default-suffix "lp"))))
    ((rp:ww-document-load-from-port this port wnd filter-index extension-different?)
     (make-pattern (read port))
     (set! *pattern-modified?* #f) (set! *nsteps* 0))
    ((rp:ww-document-save-to-port this port wnd filter-index extension-different?)
     (write (pattern-strip-gen) port)
     (set! *pattern-modified?* #f) (set! *nsteps* 0))
    ((rp:ww-document-make-title this)
     (string-append "Life: "
		    (rp:ww-document-get-title this "Scratch")
		    (if *pattern-modified?* "*[" " [")
		    (number->string *nsteps*) "]"))))

(if (not (null? (cdr *invocation-arg*)))
  (rp:ww-document-load-from-file-by-port *pattern-document* (cadr *invocation-arg*)))

(define *edit-pattern* '())

(define *cell-size* 8)

(define *center-x* 0)
(define *center-y* 0)

(define *window-width* 0)
(define *window-height* 0)

(define *disp-left* 0)
(define *disp-top* 0)
(define *disp-right* 0)
(define *disp-bottom* 0)

(define *max-scroll-max* #x4000)

(define *screen-array* '())

(define (prepare-window-mapping-mode dc)
  (rp:win32api-set-map-mode dc (win:mm anisotropic))
  (rp:win32api-set-window-ext-ex dc 1 1 #f)
  (rp:win32api-set-viewport-ext-ex dc *cell-size* *cell-size* #f))

(define (acons-subrange min max l obj)
  (let collect ((l (do ((l l (cdr l))) ((or (null? l) (<= min (car l))) l))))
    (if (or (null? l) (< max (car l))) '()
      (cons (cons (- (car l) min) obj) (collect (cdr l))))))

(define (map-subrange min max l proc)
  (let collect ((l (do ((l l (cdr l))) ((or (null? l) (<= min (caar l))) l))))
    (if (or (null? l) (< max (caar l))) '()
      (cons (cons (- (caar l) min) (proc (cdar l))) (collect (cdr l))))))

(define (make-screen-array)
  (set! *screen-array* (if (eq? *console-mode* 'edit) (make-screen-array-edit) (make-screen-array-not-edit))))

(define make-screen-array-edit
  (let ((brush (rp:win32api-get-stock-object (win: white-brush))))
    (lambda ()
      (map-subrange *disp-left* *disp-right* *edit-pattern*
	(lambda (l) (acons-subrange *disp-top* *disp-bottom* l brush))))))

(define make-screen-array-not-edit
  (let ((brushes (make-vector 65 (rp:win32api-get-stock-object (win: white-brush)))))
    (let ((store-color
	    (lambda (i r g b)
	      (let ((br (rp:win32api-create-solid-brush (+ r (* g #x100) (* b #x10000)))))
		(if br (vector-set! brushes i br))))))
      (do ((i 0 (+ i 1)) (g 0 (+ g 16))) ((= i 16)) (store-color i 255 g 0))
      (store-color 16 255 255 0)
      (do ((i 17 (+ i 1)) (r 240 (- r 16))) ((= i 32)) (store-color i r 255 0))
      (do ((i 32 (+ i 1)) (b 0 (+ b 16))) ((= i 48)) (store-color i 0 255 b))
      (store-color 48 0 255 255)
      (do ((i 49 (+ i 1)) (g 240 (- g 16))) ((= i 65)) (store-color i 0 g 255)))
    (lambda ()
      (map-subrange *disp-left* *disp-right* *current-pattern*
	(lambda (l) (map-subrange *disp-top* *disp-bottom* l (lambda (g) (vector-ref brushes (min g 64)))))))))

(define draw-window
  (let ((ps (win:paintstruct-create))
	(rect (win:rect-create)))
    (lambda (wnd)
      (rp:ww-do-paint wnd ps
	(lambda (dc)
	  (prepare-window-mapping-mode dc)
	  (rp:locally ((old-pen (rp:win32api-select-object dc (rp:win32api-get-stock-object (win: null-pen))))
		       (old-brush (rp:win32api-select-object dc (rp:win32api-get-stock-object (win: null-brush)))))
		      ((rp:win32api-select-object dc old-pen)
		       (rp:win32api-select-object dc old-brush))
	    (for-each (lambda (l)
			(let ((x (car l)))
			  (for-each (lambda (c)
				      (let ((y (car c)))
					(win:rect-set-values rect (left x) (top y) (right (+ x 1)) (bottom (+ y 1)))
					(if (not (zero? (rp:win32api-rect-visible dc rect)))
					  (begin (rp:win32api-select-object dc (cdr c))
						 (rp:win32api-rectangle dc x y (+ x 1) (+ y 1))))))
				    (cdr l))))
		      *screen-array*)))))))

(define calc-display-area
  (let ((rect (win:rect-create))
	(sinfo (win:scrollinfo-create (mask (win:sif page pos range)))))
    (lambda (wnd)
      (rp:win32api-get-client-rect wnd rect)
      (let ((w (quotient (win:rect-load-right rect) *cell-size*))
	    (h (quotient (win:rect-load-bottom rect) *cell-size*)))
	(let ((hw (quotient w 2)) (hh (quotient h 2)))
	  (set! *window-width* w)
	  (set! *window-height* h)
	  (set! *disp-left* (- *center-x* hw))
	  (set! *disp-top* (- *center-y* hh))
	  (set! *disp-right* (+ *disp-left* w))
	  (set! *disp-bottom* (+ *disp-top* h))
	  (cond ((eq? *console-mode* 'edit)
		 (set! *pattern-leftmost* (min *pattern-leftmost* (- *disp-left* w)))
		 (set! *pattern-rightmost* (max *pattern-rightmost* (+ *disp-right* w)))
		 (set! *pattern-width* (- *pattern-rightmost* *pattern-leftmost*)))
		((and (< *disp-left* *pattern-leftmost*) (<= *disp-right* *pattern-rightmost*))
		 (set! *disp-left* (- *pattern-leftmost* 1))
		 (set! *disp-right* (+ *disp-left* w))
		 (set! *center-x* (+ *disp-left* hw)))
		((and (< *pattern-rightmost* *disp-right*) (<= *pattern-leftmost* *disp-left*))
		 (set! *disp-right* (+ *pattern-rightmost* 1))
		 (set! *disp-left* (- *disp-right* w))
		 (set! *center-x* (+ *disp-left* hw))))
	  (cond ((eq? *console-mode* 'edit)
		 (set! *pattern-topmost* (min *pattern-topmost* (- *disp-top* h)))
		 (set! *pattern-bottommost* (max *pattern-bottommost* (+ *disp-bottom* h)))
		 (set! *pattern-height* (- *pattern-bottommost* *pattern-topmost*)))
		((and (< *disp-top* *pattern-topmost*) (<= *disp-bottom* *pattern-bottommost*))
		 (set! *disp-top* (- *pattern-topmost* 1))
		 (set! *disp-bottom* (+ *disp-top* h))
		 (set! *center-y* (+ *disp-top* hh)))
		((and (< *pattern-bottommost* *disp-bottom*) (<= *pattern-topmost* *disp-top*))
		 (set! *disp-bottom* (+ *pattern-bottommost* 1))
		 (set! *disp-top* (- *disp-bottom* h))
		 (set! *center-y* (+ *disp-top* hh))))))
      (if (<= *pattern-width* *max-scroll-max*)
	(win:scrollinfo-set-values sinfo (min -1) (max *pattern-width*) (page *window-width*)
					 (pos (- *disp-left* *pattern-leftmost*)))
	(win:scrollinfo-set-values sinfo (min 0) (max *max-scroll-max*) (page 0)
					 (pos (quotient (* (- *disp-left* *pattern-leftmost* -1) *max-scroll-max*)
							(- *pattern-width* *window-width* -2)))))
      (rp:win32api-set-scroll-info wnd (win:sb horz) sinfo 1)
      (if (<= *pattern-height* *max-scroll-max*)
	(win:scrollinfo-set-values sinfo (min -1) (max *pattern-height*) (page *window-height*)
					 (pos (- *disp-top* *pattern-topmost*)))
	(win:scrollinfo-set-values sinfo (min 0) (max *max-scroll-max*) (page 0)
					 (pos (quotient (* (- *disp-top* *pattern-topmost* -1) *max-scroll-max*)
							(- *pattern-height* *window-height* -2)))))
      (rp:win32api-set-scroll-info wnd (win:sb vert) sinfo 1))))

(define about-dlg
  (rp:ww-dialog-create
    (rp:ww-make-dialog-template
      `("About life" 40 40 160 44 ()
	(0 static "Rhizome/pi Game of life demo" 4 4 152 8 ((style ,(win:ss center))))
	(0 static "$Id: life.scm,v 1.2 1999/02/15 08:53:59 qfwfq Exp $" 4 16 152 10 ((style ,(win:ss left sunken endellipsis))))
	(,(win: idok) button "OK" 70 30 20 10 ((style ,(win:bs defpushbutton center))))))
    (rp:ww-dialog-dispatcher (wnd message wparam lparam)
      ((win:wm initdialog) 1)
      ((win:wm command)
       ((rp:ww-command-dispatcher (wnd id cmd ctl udat)
	  ((((win: idok) (win: idcancel)) *) (rp:win32api-end-dialog wnd 0)))
	wnd wparam lparam #f)
       1))))

(define (get-editbox-string dlg id)
  (let ((n (rp:win32api-send-dlg-item-message dlg id (win:em linelength) 0 0)))
    (rp:locally ((buf (rp:make-external-buffer (quotient (+ n 4) 4))))
		((rp:destroy-external-buffer buf))
      (rp:store-external-halfword buf 0 #t n)
      (rp:win32api-send-dlg-item-message dlg id (win:em getline) 0 (rp:cast-buffer->integer buf))
      (rp:load-external-chars buf 0 n))))

(define (set-editbox-string dlg id str)
  (rp:locally ((buf (rp:export-string str)))
	      ((rp:destroy-external-buffer buf))
    (rp:win32api-send-dlg-item-message dlg id (win:wm settext) 0 (rp:cast-buffer->integer buf))))

(define (get-button-status dlg id)
  (= (rp:win32api-send-dlg-item-message dlg id (win:bm getcheck) 0 0) (win:bst checked)))

(define (set-button-status dlg id check?)
  (rp:win32api-send-dlg-item-message dlg id (win:bm setcheck) (if check? (win:bst checked) (win:bst unchecked)) 0))

(define (enable-control dlg id on?)
  (rp:win32api-enable-window (rp:win32api-get-dlg-item dlg id) (if on? 1 0)))

(define (display-coordinates dlg)
  (if (not (eq? *console-mode* 'edit))
    (begin (set-editbox-string dlg (idc lmost) (number->string *pattern-leftmost*))
	   (set-editbox-string dlg (idc rmost) (number->string (- *pattern-rightmost* 1)))
	   (set-editbox-string dlg (idc tmost) (number->string *pattern-topmost*))
	   (set-editbox-string dlg (idc bmost) (number->string (- *pattern-bottommost* 1)))))
  (set-editbox-string dlg (idc centerx) (number->string *center-x*))
  (set-editbox-string dlg (idc centery) (number->string *center-y*)))

(define (retrieve-coordinates dlg)
  (let ((getnum (lambda (id) (let ((x (string->number (get-editbox-string dlg id))))
			       (if x (round (inexact->exact (real-part x))) 0)))))
    (set! *center-x* (getnum (idc centerx)))
    (set! *center-y* (getnum (idc centery)))))

(define (retrieve-number dlg id minimum)
  (max (or (string->number (get-editbox-string dlg id)) 0) minimum))

(define (enable-nsteps dlg)
  (enable-control dlg (idc nsteps) (get-button-status dlg (idc step))))

(define (enable-pauselen dlg)
  (enable-control dlg (idc pauselen) (get-button-status dlg (idc pause))))

(define console-set-mode
  (let ((controls `(,(idc run) ,(idc stop) ,(idc forever) ,(idc step) ,(idc nopause) ,(idc pause) ,(idc fullscreen)
		    ,(idc edit) ,(idc edit-done) ,(idc edit-cancel) ,(idc clear-view) ,(idc clear-all)
		    ,(idc load) ,(idc save)))
	(modelist '((ready #t #f #t #t #t #t #t #t #f #f #f #f #t #t)
		    (edit #f #f #t #t #t #t #t #f #t #t #t #t #f #f)
		    (run #f #t #f #f #f #f #f #f #f #f #f #f #f #f)))
	(sv-leftmost 0) (sv-topmost 0) (sv-rightmost 0) (sv-bottommost 0))
    (lambda (dlg mode next-focus)
      (for-each (lambda (id on?) (enable-control dlg id on?)) controls (cdr (assq mode modelist)))
      (set-button-status dlg (idc run) (eq? mode 'run))
      (set-button-status dlg (idc edit) (eq? mode 'edit))
      (if (eq? mode 'run)
	(begin (enable-control dlg (idc nsteps) #f)
	       (enable-control dlg (idc pauselen) #f))
	(begin (enable-nsteps dlg)
	       (enable-pauselen dlg)))
      (cond ((eq? mode 'edit)
	     (set! sv-leftmost *pattern-leftmost*)
	     (set! sv-topmost *pattern-topmost*)
	     (set! sv-rightmost *pattern-rightmost*)
	     (set! sv-bottommost *pattern-bottommost*))
	    ((eq? *console-mode* 'edit)
	     (set! *pattern-leftmost* sv-leftmost)
	     (set! *pattern-topmost* sv-topmost)
	     (set! *pattern-rightmost* sv-rightmost)
	     (set! *pattern-bottommost* sv-bottommost)
	     (set! *pattern-width* (- *pattern-rightmost* *pattern-leftmost*))
	     (set! *pattern-height* (- *pattern-bottommost* *pattern-topmost*))))
      (set! *console-mode* mode)
      (rp:win32api-set-focus (rp:win32api-get-dlg-item dlg next-focus)))))

(define (full-redisplay wnd dlg)
  (calc-display-area wnd)
  (display-coordinates dlg)
  (make-screen-array)
  (rp:win32api-invalidate-rect wnd #f 1))

(define make-fullscreen
  (let ((restore? #t))
    (lambda (wnd dlg set?)
      (if set? (begin (let ((style (rp:win32api-get-window-long wnd (win:gwl style))))
			(set! restore? (even? (quotient style (win:ws maximize))))
			(rp:win32api-set-window-long wnd (win:gwl style) (- style (win:ws caption))))
		      (rp:win32api-show-window wnd (win:sw showmaximized))
		      (rp:win32api-show-window dlg (win:sw hide)))
	       (begin (rp:win32api-set-window-long wnd (win:gwl style)
			(rp:bitwise-or (rp:win32api-get-window-long wnd (win:gwl style)) (win:ws caption)))
		      (if restore? (rp:win32api-show-window wnd (win:sw restore)))
		      (rp:win32api-show-window dlg (win:sw show))))
      (rp:win32api-set-window-pos wnd #f 0 0 0 0 (win:swp framechanged nomove nosize nozorder))
      (full-redisplay wnd dlg)
      (rp:win32api-update-window wnd))))

(define *steps-remain* #f)
(define *pause-timer* #f)

(define *timer-triggered* #f)
(define timerproc (rp:make-entry effect ((wnd buffer) (msg cardinal) (id cardinal) (time cardinal))
		    (set! *timer-triggered* #t)))

(define console-dlg
  (rp:ww-dialog-create
    (rp:ww-make-dialog-template
      `("Game of life" 10 10 238 200
	 ((style ,(rp:bitwise-or (win:ws caption visible sysmenu minimizebox) (win:ds absalign))))
	(0 button "Execution" 6 5 72 129 ((style ,(rp:bitwise-or (win:ws group) (win:bs groupbox)))))
	(,(idc run) button "&Run" 12 15 30 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs checkbox pushlike center)))))
	(,(idc stop) button "St&op" 42 15 30 12
	 ((style ,(rp:bitwise-or (win:ws tabstop disabled) (win:bs pushbutton center)))))
	(0 static "" 10 30 64 2 ((style ,(win:ss etchedhorz))))
	(,(idc forever) button "&Infinite" 12 35 60 10
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs autoradiobutton left)))))
	(,(idc step) button "R&un" 12 47 24 10 ((style ,(win:bs autoradiobutton left))))
	(0 static "t&hrough" 37 48 34 10 ((style ,(win:ss left))))
	(,(idc nsteps) edit "1" 24 59 24 10
	 ((style ,(rp:bitwise-or (win:ws tabstop border) (win:es left number autohscroll)))))
	(0 static "steps" 52 59 20 10 ((style ,(win:ss left))))
	(0 static "" 10 73 64 2 ((style ,(win:ss etchedhorz))))
	(,(idc nopause) button "&Full speed" 12 78 60 10
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs autoradiobutton left)))))
	(,(idc pause) button "&Pause" 12 90 60 10 ((style ,(win:bs autoradiobutton left))))
	(0 static "&ms" 52 102 20 10 ((style ,(win:ss left))))
	(,(idc pauselen) edit "500" 24 102 24 10
	 ((style ,(rp:bitwise-or (win:ws tabstop border) (win:es left number autohscroll)))))
	(0 static "" 10 116 64 2 ((style ,(win:ss etchedhorz))))
	(,(idc fullscreen) button "Fullscree&n" 12 121 60 10
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs autocheckbox left)))))
	(0 button "Pattern" 84 5 102 80 ((style ,(rp:bitwise-or (win:ws group) (win:bs groupbox)))))
	(,(idc edit) button "&Edit" 90 15 30 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs checkbox pushlike center)))))
	(,(idc edit-done) button "&Done" 120 15 30 12
	 ((style ,(rp:bitwise-or (win:ws tabstop disabled) (win:bs pushbutton center)))))
	(,(idc edit-cancel) button "Re&vert" 150 15 30 12
	 ((style ,(rp:bitwise-or (win:ws tabstop disabled) (win:bs pushbutton center)))))
	(,(idc clear-view) button "Clear vie&w" 120 29 60 12
	 ((style ,(rp:bitwise-or (win:ws tabstop disabled) (win:bs pushbutton center)))))
	(,(idc clear-all) button "&Clear all" 120 43 60 12
	 ((style ,(rp:bitwise-or (win:ws tabstop disabled) (win:bs pushbutton center)))))
	(0 static "" 88 60 94 2 ((style ,(win:ss etchedhorz))))
	(,(idc load) button "&Load ..." 90 67 44 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs pushbutton center)))))
	(,(idc save) button "&Save as ..." 136 67 44 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs pushbutton center)))))
	(0 button "Zoom" 84 90 102 44 ((style ,(rp:bitwise-or (win:ws group) (win:bs groupbox)))))
	(0 static "Cell si&ze:" 90 101 48 10 ((style ,(win:ss left))))
	(,(idc cellsize) edit "" 140 101 24 10
	 ((style ,(rp:bitwise-or (win:ws tabstop border) (win:es left number autohscroll)))))
	(,(idc setsize) button "Chan&ge" 140 116 40 12
	 ((style ,(rp:bitwise-or (win:ws tabstop) (win:bs pushbutton center)))))
	(0 button "Coordinates" 6 137 180 58 ((style ,(rp:bitwise-or (win:ws group) (win:bs groupbox)))))
	(0 static "Leftmost:" 12 148 48 10 ((style ,(win:ss left))))
	(,(idc lmost) edit "" 66 148 24 10 ((style ,(rp:bitwise-or (win:ws border) (win:es left readonly)))))
	(0 static "Rightmost:" 102 148 48 10 ((style ,(win:ss left))))
	(,(idc rmost) edit "" 156 148 24 10 ((style ,(rp:bitwise-or (win:ws border) (win:es left readonly)))))
	(0 static "Topmost:" 12 162 48 10 ((style ,(win:ss left))))
	(,(idc tmost) edit "" 66 162 24 10 ((style ,(rp:bitwise-or (win:ws border) (win:es left readonly)))))
	(0 static "Bottommost:" 102 162 48 10 ((style ,(win:ss left))))
	(,(idc bmost) edit "" 156 162 24 10 ((style ,(rp:bitwise-or (win:ws border) (win:es left readonly)))))
	(0 static "" 10 176 172 2 ((style ,(win:ss etchedhorz))))
	(0 static "Center" 12 181 32 10 ((style ,(rp:bitwise-or (win:ws group) (win:ss left)))))
	(0 static "&x:" 46 181 10 10 ((style ,(win:ss left))))
	(,(idc centerx) edit "" 58 181 24 10
	 ((style ,(rp:bitwise-or (win:ws tabstop border) (win:es left autohscroll)))))
	(0 static "&y:" 90 181 10 10 ((style ,(win:ss left))))
	(,(idc centery) edit "" 102 181 24 10
	 ((style ,(rp:bitwise-or (win:ws tabstop border) (win:es left autohscroll)))))
	(,(idc xygoto) button "&Jump" 150 180 30 12
	 ((style ,(rp:bitwise-or (win:ws tabstop) (win:bs pushbutton center)))))
	(,(idc about) button "&About ..." 192 168 40 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs pushbutton center)))))
	(,(idc exit) button "Exi&t" 192 184 40 12
	 ((style ,(rp:bitwise-or (win:ws group tabstop) (win:bs pushbutton center)))))))
    (rp:ww-dialog-dispatcher (wnd message wparam lparam)
      ((win:wm initdialog)
       (rp:win32api-enable-menu-item (rp:win32api-get-system-menu wnd 0) (win:sc close) (win:mf bycommand grayed))
       (rp:ww-message-loop-add-dialog main-loop wnd)
       (set-button-status wnd (idc step) #t)
       (set-button-status wnd (idc pause) #t)
       (set-editbox-string wnd (idc cellsize) (number->string *cell-size*))
       1)
      ((win:wm command)
       ((rp:ww-command-dispatcher (wnd id cmd ctl udat)
	  ((((idc forever) - (idc step)) *) (enable-nsteps wnd))
	  ((((idc nopause) - (idc pause)) *) (enable-pauselen wnd))
	  ((((idc run)) *)
	   (if (get-button-status wnd (idc fullscreen)) (make-fullscreen (rp:win32api-get-parent wnd) wnd #t))
	   (set! *steps-remain* (and (get-button-status wnd (idc step)) (retrieve-number wnd (idc nsteps) 1)))
	   (set! *pause-timer* (and (get-button-status wnd (idc pause))
				    (rp:win32api-set-timer #f 0 (retrieve-number wnd (idc pauselen) 1) timerproc)))
	   (set! *timer-triggered* #t)
	   (console-set-mode wnd 'run (idc stop)))
	  ((((idc stop)) *)
	   (if (eq? *console-mode* 'run)
	     (begin (if *pause-timer* (rp:win32api-kill-timer #f *pause-timer*))
		    (if (get-button-status wnd (idc fullscreen)) (make-fullscreen (rp:win32api-get-parent wnd) wnd #f))
		    (console-set-mode wnd 'ready (idc run)))))
	  ((((idc edit)) *)
	   (console-set-mode wnd 'edit (idc edit-cancel))
	   (set! *edit-pattern* (pattern-strip-gen))
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc edit-done)) *)
	   (console-set-mode wnd 'ready (idc edit))
	   (make-pattern *edit-pattern*)
	   (set! *pattern-modified?* #t)
	   (set! *edit-pattern* '())
	   (let ((main-wnd (rp:win32api-get-parent wnd)))
	     (rp:ww-document-update-title *pattern-document* main-wnd)
	     (full-redisplay main-wnd wnd)))
	  ((((idc edit-cancel)) *)
	   (console-set-mode wnd 'ready (idc edit))
	   (set! *edit-pattern* '())
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc clear-view)) *)
	   (set! *edit-pattern*
	     (let loop ((pat *edit-pattern*))
	       (cond ((or (null? pat) (>= (caar pat) *disp-right*)) pat)
		     ((< (caar pat) *disp-left*) (cons (car pat) (loop (cdr pat))))
		     (else (let ((l (let loop ((l (cdar pat)))
				      (cond ((or (null? l) (>= (car l) *disp-bottom*)) l)
					    ((< (car l) *disp-top*) (cons (car l) (loop (cdr l))))
					    (else (loop (cdr l))))))
				 (t (loop (cdr pat))))
			     (if (null? l) t (cons (cons (caar pat) l) t)))))))
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc clear-all)) *)
	   (set! *edit-pattern* '())
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc load)) *)
	   (let ((main-wnd (rp:win32api-get-parent wnd)))
	     (rp:ww-document-open-by-port *pattern-document* wnd)
	     (rp:ww-document-update-title *pattern-document* main-wnd)
	     (full-redisplay main-wnd wnd)))
	  ((((idc save)) *)
	   (rp:ww-document-save-as-by-port *pattern-document* wnd)
	   (rp:ww-document-update-title *pattern-document* (rp:win32api-get-parent wnd)))
	  ((((idc setsize)) *)
	   (set! *cell-size* (retrieve-number wnd (idc cellsize) 2))
	   (set-editbox-string wnd (idc cellsize) (number->string *cell-size*))
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc xygoto)) *)
	   (retrieve-coordinates wnd)
	   (full-redisplay (rp:win32api-get-parent wnd) wnd))
	  ((((idc about)) *) (rp:ww-dialog-do-modal about-dlg wnd 0))
	  ((((idc exit)) *) (rp:win32api-post-message (rp:win32api-get-parent wnd) (win:wm close) 0 0)))
	wnd wparam lparam #f)
       1))))

(define the-class
  (rp:ww-make-window-class
    (rp:ww-message-dispatcher (wnd message wparam lparam)
      ((win:wm create)
       (rp:ww-set-user-data wnd lparam)
       (set-car! (rp:ww-get-user-data wnd) (rp:ww-dialog-create-modeless console-dlg wnd 0))
       (rp:ww-document-update-title *pattern-document* wnd)
       0)
      ((win:wm destroy) (rp:win32api-post-quit-message 0) 0)
      ((win:wm paint) (draw-window wnd) 0)
      ((win:wm size)
       (if (or (= wparam (win:size restored)) (= wparam (win:size maximized)))
	 (begin (calc-display-area wnd)
		(display-coordinates (car (rp:ww-get-user-data wnd)))
		(make-screen-array)))
       0)
      ((win:wm lbuttondown)
       (cond ((eq? *console-mode* 'run) (rp:win32api-send-message wnd (appmsg stop) 0 0))
	     ((eq? *console-mode* 'edit)
	      (rp:ww-split-lparam lparam #t #t
		(lambda (dx dy)
		  (let ((lx (quotient dx *cell-size*)) (ly (quotient dy *cell-size*))
			(wbrush (rp:win32api-get-stock-object (win: white-brush)))
			(bbrush (rp:win32api-get-stock-object (win: black-brush))))
		    (let ((x (+ lx *disp-left*)) (y (+ ly *disp-top*))
			  (toggle-disp
			    (lambda (brush)
			      (rp:locally ((dc (rp:win32api-get-dc wnd)))
					  ((rp:win32api-release-dc wnd dc))
				(prepare-window-mapping-mode dc)
				(rp:locally ((old-pen (rp:win32api-select-object dc
							(rp:win32api-get-stock-object (win: null-pen))))
					     (old-brush (rp:win32api-select-object dc brush)))
					    ((rp:win32api-select-object dc old-pen)
					     (rp:win32api-select-object dc old-brush))
				  (rp:win32api-rectangle dc lx ly (+ lx 1) (+ ly 1)))))))
		      (set! *edit-pattern*
			(let loop ((pat *edit-pattern*))
			  (cond ((or (null? pat) (< x (caar pat))) (toggle-disp wbrush) `((,x ,y) ,@pat))
				((= x (caar pat))
				 (let ((l (let loop ((l (cdar pat)))
					    (cond ((or (null? l) (< y (car l))) (toggle-disp wbrush) (cons y l))
						  ((= y (car l)) (toggle-disp bbrush) (cdr l))
						  (else (cons (car l) (loop (cdr l))))))))
				   (if (null? l) (cdr pat) `((,x ,@l) ,@(cdr pat)))))
				(else (cons (car pat) (loop (cdr pat))))))))
		    (set! *screen-array*
		      (let loop ((pat *screen-array*))
			(cond ((or (null? pat) (< lx (caar pat))) `((,lx (,ly . ,wbrush)) ,@pat))
			      ((= lx (caar pat))
			       (let ((l (let loop ((l (cdar pat)))
					  (cond ((or (null? l) (< ly (caar l))) `((,ly . ,wbrush) ,@l))
						((= ly (caar l)) (cdr l))
						(else (cons (car l) (loop (cdr l))))))))
				 (if (null? l) (cdr pat) `((,lx ,@l) ,@(cdr pat)))))
			      (else (cons (car pat) (loop (cdr pat))))))))))))
       0)
      ((win:wm hscroll)
       (let ((orgx *center-x*)
	     (l-scroll (lambda (d) (if (>= *disp-left* *pattern-leftmost*) (set! *center-x* (- *center-x* d)))))
	     (r-scroll (lambda (d) (if (<= *disp-right* *pattern-rightmost*) (set! *center-x* (+ *center-x* d))))))
	 (rp:ww-split-wparam wparam #t #t
	   (lambda (code pos)
	     (cond ((= code (win:sb lineleft)) (l-scroll 1))
		   ((= code (win:sb lineright)) (r-scroll 1))
		   ((= code (win:sb pageleft)) (l-scroll *window-width*))
		   ((= code (win:sb pageright)) (r-scroll *window-width*))
		   ((= code (win:sb left)) (l-scroll *pattern-width*))
		   ((= code (win:sb right)) (r-scroll *pattern-width*))
		   ((= code (win:sb thumbposition))
		    (let ((newleft (if (<= *pattern-width* *max-scroll-max*)
				     (+ *pattern-leftmost* pos)
				     (+ *pattern-leftmost* -1
					(quotient (* (- *pattern-width* *window-width* -2) pos) *max-scroll-max*)))))
		      (if (< newleft *disp-left*)
			(l-scroll (- *disp-left* newleft))
			(r-scroll (- newleft *disp-left*))))))))
	 (calc-display-area wnd)
	 (display-coordinates (car (rp:ww-get-user-data wnd)))
	 (if (not (= orgx *center-x*))
	   (begin (make-screen-array)
		  (rp:win32api-scroll-window-ex wnd (* (- orgx *center-x*) *cell-size*) 0
						#f #f #f #f (win:sw invalidate erase)))))
       0)
      ((win:wm vscroll)
       (let ((orgy *center-y*)
	     (u-scroll (lambda (d) (if (>= *disp-top* *pattern-topmost*) (set! *center-y* (- *center-y* d)))))
	     (d-scroll (lambda (d) (if (<= *disp-bottom* *pattern-bottommost*) (set! *center-y* (+ *center-y* d))))))
	 (rp:ww-split-wparam wparam #t #t
	   (lambda (code pos)
	     (cond ((= code (win:sb lineup)) (u-scroll 1))
		   ((= code (win:sb linedown)) (d-scroll 1))
		   ((= code (win:sb pageup)) (u-scroll *window-height*))
		   ((= code (win:sb pagedown)) (d-scroll *window-height*))
		   ((= code (win:sb top)) (u-scroll *pattern-height*))
		   ((= code (win:sb bottom)) (d-scroll *pattern-height*))
		   ((= code (win:sb thumbposition))
		    (let ((newtop (if (<= *pattern-height* *max-scroll-max*)
				    (+ *pattern-topmost* pos)
				    (+ *pattern-topmost* -1
				       (quotient (* (- *pattern-height* *window-height* -2) pos) *max-scroll-max*)))))
		      (if (< newtop *disp-top*)
			(u-scroll (- *disp-top* newtop))
			(d-scroll (- newtop *disp-top*))))))))
	 (calc-display-area wnd)
	 (display-coordinates (car (rp:ww-get-user-data wnd)))
	 (if (not (= orgy *center-y*))
	   (begin (make-screen-array)
		  (rp:win32api-scroll-window-ex wnd 0 (* (- orgy *center-y*) *cell-size*)
						#f #f #f #f (win:sw invalidate erase)))))
       0)
      ((win:wm keydown)
       (let ((ctrl? (lambda () (< (rp:win32api-get-key-state (win:vk control)) 0)))
	     (hsend (lambda (code) (rp:win32api-send-message wnd (win:wm hscroll) code 0)))
	     (vsend (lambda (code) (rp:win32api-send-message wnd (win:wm vscroll) code 0))))
	 (cond ((= wparam (win:vk left)) (hsend (win:sb lineleft)))
	       ((= wparam (win:vk up)) (vsend (win:sb lineup)))
	       ((= wparam (win:vk right)) (hsend (win:sb lineright)))
	       ((= wparam (win:vk down)) (vsend (win:sb linedown)))
	       ((= wparam (win:vk prior)) (if (ctrl?) (hsend (win:sb pageleft)) (vsend (win:sb pageup))))
	       ((= wparam (win:vk next)) (if (ctrl?) (hsend (win:sb pageright)) (vsend (win:sb pagedown))))
	       ((= wparam (win:vk home)) (if (ctrl?) (hsend (win:sb left)) (vsend (win:sb top))))
	       ((= wparam (win:vk end)) (if (ctrl?) (hsend (win:sb right)) (vsend (win:sb bottom))))))
       0)
      ((appmsg stop) (rp:win32api-send-message (car (rp:ww-get-user-data wnd)) (win:wm command) (idc stop) 0) 0)
      ((appmsg play)
       (if *pause-timer* (set! *timer-triggered* #f))
       (play-life-game)
       (rp:ww-document-update-title *pattern-document* wnd)
       (let ((dlg (car (rp:ww-get-user-data wnd))))
	 (full-redisplay wnd dlg)
	 (rp:win32api-update-window wnd)
	 (if *steps-remain*
	   (begin (set! *steps-remain* (- *steps-remain* 1))
		  (if (not (zero? *steps-remain*))
		    (set-editbox-string dlg (idc nsteps) (number->string *steps-remain*)))))
	 (if (or (null? *current-pattern*) (and *steps-remain* (zero? *steps-remain*)))
	   (rp:win32api-send-message dlg (win:wm command) (idc stop) 0)))
       0))
    'style (win:cs hredraw vredraw)
    'background (rp:win32api-get-stock-object (win: black-brush))))

(define main-loop (rp:ww-message-loop-create))

(define main-win (rp:ww-create-window the-class "" (cons #f '())
				      'style (win:ws overlappedwindow hscroll vscroll)))

(let loop () (cond ((rp:ww-message-loop-process-pending-messages main-loop) => exit)
		   ((and (eq? *console-mode* 'run) *timer-triggered*)
		    (rp:win32api-post-message main-win (appmsg play) 0 0))
		   (else (rp:win32api-wait-message)))
	     (loop))
