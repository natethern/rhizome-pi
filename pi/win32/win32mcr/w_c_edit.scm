;;;; @(#)$Id: w_c_edit.scm,v 1.1 1998/07/31 11:35:07 qfwfq Exp $

(rp:declare-flag-set win:es	; edit style
  (left #x0)
  (center #x1)
  (right #x2)
  (multiline #x4)
  (uppercase #x8)
  (lowercase #x10)
  (password #x20)
  (autovscroll #x40)
  (autohscroll #x80)
  (nohidesel #x100)
  (oemconvert #x400)
  (readonly #x800)
  (wantreturn #x1000)
  (number #x2000))

(rp:declare-constants win:em	; Edit control messages
  (getsel #xb0)
  (setsel #xb1)
  (getrect #xb2)
  (setrect #xb3)
  (setrectnp #xb4)
  (scroll #xb5)
  (linescroll #xb6)
  (scrollcaret #xb7)
  (getmodify #xb8)
  (setmodify #xb9)
  (getlinecount #xba)
  (lineindex #xbb)
  (sethandle #xbc)
  (gethandle #xbd)
  (getthumb #xbe)
  (linelength #xc1)
  (replacesel #xc2)
  (getline #xc4)
  (limittext #xc5)
  (canundo #xc6)
  (undo #xc7)
  (fmtlines #xc8)
  (linefromchar #xc9)
  (settabstops #xcb)
  (setpasswordchar #xcc)
  (emptyundobuffer #xcd)
  (getfirstvisibleline #xce)
  (setreadonly #xcf)
  (setwordbreakproc #xd0)
  (getwordbreakproc #xd1)
  (getpasswordchar #xd2)
  (setmargins #xd3)
  (getmargins #xd4)
  (setlimittext #xc5)
  (getlimittext #xd5)
  (posfromchar #xd6)
  (charfrompos #xd7)
  (setimestatus #xd8)
  (getimestatus #xd9))

(rp:declare-constants win:en	; Edit control notification
  (setfocus #x100)
  (killfocus #x200)
  (change #x300)
  (update #x400)
  (errspace #x500)
  (maxtext #x501)
  (hscroll #x601)
  (vscroll #x602))
