;;;; @(#)$Id: w_devcap.scm,v 1.1 1998/07/31 11:35:09 qfwfq Exp $

(rp:declare-flag-set win:cc	; GetDeviceCaps CURVECAPS value mask
  (none 0)
  (circles 1)
  (pie 2)
  (chord 4)
  (ellipses 8)
  (wide 16)
  (styled 32)
  (widestyled 64)
  (interiors 128)
  (roundrect 256))

(rp:declare-flag-set win:lc	; GetDeviceCaps LINECAPS value mask
  (none 0)
  (polyline 2)
  (marker 4)
  (polymarker 8)
  (wide 16)
  (styled 32)
  (widestyled 64)
  (interiors 128))

(rp:declare-flag-set win:pc	; GetDeviceCaps POLYGONALCAPS value mask
  (none 0)
  (polygon 1)
  (rectangle 2)
  (windpolygon 4)
  (trapezoid 4)
  (scanline 8)
  (wide 16)
  (styled 32)
  (widestyled 64)
  (interiors 128)
  (polypolygon 256)
  (paths 512))

(rp:declare-flag-set win:tc	; GetDeviceCaps TEXTCAPS value mask
  (op-character #x1)
  (op-stroke #x2)
  (cp-stroke #x4)
  (cr-90 #x8)
  (cr-any #x10)
  (sf-x-yindep #x20)
  (sa-double #x40)
  (sa-integer #x80)
  (sa-contin #x100)
  (ea-double #x200)
  (ia-able #x400)
  (ua-able #x800)
  (so-able #x1000)
  (ra-able #x2000)
  (va-able #x4000)
  (reserved #x8000)
  (scrollblt #x10000))

(rp:declare-flag-set win:rc	; GetDeviceCaps RASTERCAPS value mask
  (none 0)
  (bitblt 1)
  (banding 2)
  (scaling 4)
  (bitmap64 8)
  (gdi20-output #x10)
  (gdi20-state #x20)
  (savebitmap #x40)
  (di-bitmap #x80)
  (palette #x100)
  (dibtodev #x200)
  (bigfont #x400)
  (stretchblt #x800)
  (floodfill #x1000)
  (stretchdib #x2000)
  (op-dx-output #x4000)
  (devbits #x8000))
