;;;; @(#)$Id: w_scroll.scm,v 1.1 1998/07/31 11:35:16 qfwfq Exp $

(rp:declare-flag-set win:sb
  (horz 0)			; Scroll bar flags
  (vert 1)
  (ctl 2)
  (both 3)
  (lineup 0)			; Scroll bar notification codes
  (lineleft 0)
  (linedown 1)
  (lineright 1)
  (pageup 2)
  (pageleft 2)
  (pagedown 3)
  (pageright 3)
  (thumbposition 4)
  (thumbtrack 5)
  (top 6)
  (left 6)
  (bottom 7)
  (right 7)
  (endscroll 8)
  (none #x0)			; GetDeviceCaps SHADEBLENDCAPS value mask
  (const-alpha #x1)
  (pixel-alpha #x2)
  (premult-alpha #x4)
  (grad-rect #x10)
  (grad-tri #x20))

(rp:declare-flag-set win:sif	; SCROLLINFO Mask
  (range #x1)
  (page #x2)
  (pos #x4)
  (disablenoscroll #x8)
  (trackpos #x10)
  (all #x17))

(rp:declare-constants win:esb	; EnableScrollBar flags
  (enable-both #x0)
  (disable-both #x3)
  (disable-left #x1)
  (disable-right #x2)
  (disable-up #x1)
  (disable-down #x2)
  (disable-ltup #x1)
  (disable-rtdn #x2))

(rp:define-buffer-structure win:scrollinfo
  (size cardinal (win:scrollinfo-size))
  (mask cardinal)
  (min integer)
  (max integer)
  (page cardinal)
  (pos integer)
  (track-pos integer))
