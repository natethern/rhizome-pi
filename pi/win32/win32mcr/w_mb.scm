;;;; @(#)$Id: w_mb.scm,v 1.1 1998/07/31 11:35:12 qfwfq Exp $

(rp:declare-constants win:cp	; standard CodePage
  (acp 0)
  (oemcp 1)
  (maccp 2)
  (thread-acp 3)
  (symbol 42)
  (utf7 65000)
  (utf8 65001))

(rp:declare-flag-set win:mb
  (precomposed #x1)		; MultiByteToWideChar dwFlags
  (composite #x2)
  (useglyphchars #x4)
  (err-invalid-chars #x8)
  (ok #x0)			; MessageBox uType
  (okcancel #x1)
  (abortretryignore #x2)
  (yesnocancel #x3)
  (yesno #x4)
  (retrycancel #x5)
  (iconhand #x10)
  (iconquestion #x20)
  (iconexclamation #x30)
  (iconasterisk #x40)
  (usericon #x80)
  (iconwarning #x30)
  (iconerror #x10)
  (iconinformation #x40)
  (iconstop #x10)
  (defbutton1 #x0)
  (defbutton2 #x100)
  (defbutton3 #x200)
  (defbutton4 #x300)
  (applmodal #x0)
  (systemmodal #x1000)
  (taskmodal #x2000)
  (help #x4000)
  (nofocus #x8000)
  (setforeground #x10000)
  (default-desktop-only #x20000)
  (topmost #x40000)
  (right #x80000)
  (rtlreading #x100000)
  (service-notification #x200000)
  (service-notification-nt3x #x40000)
  (typemask #xf)
  (iconmask #xf0)
  (defmask #xf00)
  (modemask #x3000)
  (miscmask #xc000))
