;;;; @(#)$Id: w_file.scm,v 1.1 1999/02/15 08:16:25 qfwfq Exp $

(rp:declare-flag-set win:generic	; handle access modes
  (read #x80000000)
  (write #x40000000)
  (execute #x20000000)
  (all #x10000000))

(rp:declare-flag-set win:file-share	; file share modes
  (read #x1)
  (write #x2)
  (delete #x4))

(rp:declare-constants win:create	; file creation modes
  (new 1)
  (always 2))

(rp:declare-constants win:open		; file creation modes
  (existing 3)
  (always 4))

(rp:declare-constants win:truncate	; file creation modes
  (existing 5))

(rp:declare-flag-set win:file-attribute	; file attributes
  (readonly #x1)
  (hidden #x2)
  (system #x4)
  (directory #x10)
  (archive #x20)
  (encrypted #x40)
  (normal #x80)
  (temporary #x100)
  (sparse-file #x200)
  (reparse-point #x400)
  (compressed #x800)
  (offline #x1000))

(rp:declare-flag-set win:file-flag	; additional file open flags
  (write-through #x80000000)
  (overlapped #x40000000)
  (no-buffering #x20000000)
  (random-access #x10000000)
  (sequential-scan #x8000000)
  (delete-on-close #x4000000)
  (backup-semantics #x2000000)
  (posix-semantics #x1000000)
  (open-reparse-point #x200000)
  (open-no-recall #x100000))

(rp:declare-constants win:invalid	; misc constants
  (handle-value -1)
  (file-size #xffffffff))
