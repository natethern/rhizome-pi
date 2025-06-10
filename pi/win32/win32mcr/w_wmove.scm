;;;; @(#)$Id: w_wmove.scm,v 1.1 1998/07/31 11:35:17 qfwfq Exp $

(rp:declare-flag-set win:sw
  (hide 0)			; ShowWindow Command
  (shownormal 1)
  (normal 1)
  (showminimized 2)
  (showmaximized 3)
  (maximize 3)
  (shownoactivate 4)
  (show 5)
  (minimize 6)
  (showminnoactive 7)
  (showna 8)
  (restore 9)
  (showdefault 10)
  (forceminimize 11)
  (max 11)
  (scrollchildren #x1)		; ScrollWindowEx flags
  (invalidate #x2)
  (erase #x4)
  (smoothscroll #x10))

(rp:declare-constants win:hwnd	; pseudo HWND's
  (broadcast #xffff)
  (message -3)
  (desktop 0)
  (top 0)
  (bottom 1)
  (topmost -1)
  (notopmost -2))

(rp:declare-flag-set win:swp	; SetWindowPos flags
  (nosize #x1)
  (nomove #x2)
  (nozorder #x4)
  (noredraw #x8)
  (noactivate #x10)
  (framechanged #x20)
  (showwindow #x40)
  (hidewindow #x80)
  (nocopybits #x100)
  (noownerzorder #x200)
  (nosendchanging #x400)
  (drawframe #x20)
  (noreposition #x200)
  (defererase #x2000)
  (asyncwindowpos #x4000))
