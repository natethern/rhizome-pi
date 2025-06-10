;;;; @(#)$Id: w_misc.scm,v 1.1 1998/07/31 11:35:14 qfwfq Exp $

(rp:declare-constants win:ver-platform	; GetVersionEx dwPlatformId
  (win32s 0)
  (win32-windows 1)
  (win32-nt 2))

(rp:declare-flag-set win:format-message	; FormatMessage dwFlags
  (allocate-buffer #x100)
  (ignore-inserts #x200)
  (from-string #x400)
  (from-hmodule #x800)
  (from-system #x1000)
  (argument-array #x2000)
  (max-width-mask #xff))

(rp:define-buffer-structure win:osversioninfo
  (os-version-info-size cardinal (win:osversioninfo-size))
  (major-version cardinal)
  (minor-version cardinal)
  (build-number cardinal)
  (platform-id cardinal)
  (csd-version byte-array 128))
