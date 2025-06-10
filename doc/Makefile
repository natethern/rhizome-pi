# @(#)$Header: /u/master/rhizome/doc/Makefile,v 1.3 1998/08/03 04:46:15 qfwfq Exp $

# $Log: Makefile,v $
# Revision 1.3  1998/08/03 04:46:15  qfwfq
# Fix typo
#
# Revision 1.2  1998/07/31 11:49:03  qfwfq
# Features addition for release 0.45
#
# Revision 1.1  1996/12/16 06:50:39  qfwfq
# Created documents
#

.SUFFIXES: .0 .man

.man.0:; nroff -man $< > $@

all: pi.0 piw.0 pisc.0 pisf.0 pisl.0

clean:; rm -f *.0
