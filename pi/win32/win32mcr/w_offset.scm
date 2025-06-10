;;;; @(#)$Id: w_offset.scm,v 1.1 1998/07/31 11:35:15 qfwfq Exp $

(rp:declare-constants win:gcl	; GetClassLong offsets
  (menuname -8)
  (hbrbackground -10)
  (hcursor -12)
  (hicon -14)
  (hmodule -16)
  (cbwndextra -18)
  (cbclsextra -20)
  (wndproc -24)
  (style -26)
  (hiconsm -34))

(rp:declare-constants win:gcw	; GetClassWord offsets
  (atom -32))
