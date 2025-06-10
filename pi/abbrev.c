/*
 * Copyright (c) 1993,96-99 Inujima, Masaru <qfwfq@kt.rim.or.jp>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef lint
static char rcsid[] = "@(#)$Id: abbrev.c,v 1.6 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: abbrev.c,v $
 * Revision 1.6  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.5  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.4  1999/02/15 08:47:38  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.3  1998/07/31 11:06:38  qfwfq
 * Fasloader support
 *
 * Revision 1.2  1996/09/06 06:11:22  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:46  qfwfq
 * Initial revision
 *
 */

/*
 * Extension of read to support abbreviations.
 */
#include "rhiz_pi.h"

#include <stdio.h>
#include <string.h>

#define GETSYM(name)	(RkInternSymbol((name), sizeof(name)-1))

static rk_object quotes_conti1_proc, unquotes_conti1_proc, unquotes_conti2_proc;

static rk_object
quotes(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = quotes_conti1_proc;
	cp[2] = (rk_eval_register[0] == RK_MAKEICHAR('\'')
		? RP_CAR(rk_eval_register[3]) : RP_CDR(rk_eval_register[3]));
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[2];
	rk_eval_register[3] = rk_eval_register[1];
	rk_eval_register[0] = cp[1];
	rk_eval_register[1] = cp[2];
	rk_eval_register[2] = cp[3];
	return	rk_read_proc;
}

static rk_object
quotes_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	cp = RkAllocCells(4);
	cp[0] = rk_continuation[2];
	cp[1] = (rk_object)&cp[2];
	cp[2] = rk_eval_register[0];
	cp[3] = RK_SOBJ_NIL;
	rk_eval_register[0] = (rk_object)cp;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
unquotes(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = unquotes_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = rk_eval_register[3];
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
unquotes_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_MAKEICHAR('@')) {
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = quotes_conti1_proc;
		cp[2] = RP_CDR(rk_continuation[3]);
		cp[3] = rk_continuation[4];
		rk_eval_register[3] = rk_eval_register[1];
		rk_eval_register[0] = ((rk_object *)rk_continuation[2])[1];
		rk_eval_register[1] = ((rk_object *)rk_continuation[2])[2];
		rk_eval_register[2] = ((rk_object *)rk_continuation[2])[3];
		rk_continuation = cp;
		rk_valid_register = 4;
		return	rk_read_proc;
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = unquotes_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = RP_CAR(rk_continuation[3]);
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	((rk_object *)rk_continuation[2])[2];
}

static rk_object
unquotes_conti2(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = quotes_conti1_proc;
	cp[2] = rk_continuation[3];
	cp[3] = rk_continuation[4];
	rk_eval_register[3] = rk_eval_register[0];
	rk_eval_register[0] = ((rk_object *)rk_continuation[2])[1];
	rk_eval_register[1] = ((rk_object *)rk_continuation[2])[2];
	rk_eval_register[2] = ((rk_object *)rk_continuation[2])[3];
	rk_continuation = cp;
	rk_valid_register = 4;
	return	rk_read_proc;
}

static rk_object usersyntax_proc;

rk_object rp_regsyntax_proc;
static rk_object
regsyntax(void)
{
	rk_object *cp;
	int c;

	RP_ASSERTARG(2);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
	if (!RK_ISICHAR(rk_eval_register[1]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[1]);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(2);
	cp[0] = usersyntax_proc;
	cp[1] = RP_CAR(rk_eval_register[0]) & ~7;
	RkMakeCharTableEntry(c, cp, RK_CFLAG_PUNCTUATION);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_regdispatch_proc;
static rk_object
regdispatch(void)
{
	rk_object *cp;
	int c;

	RP_ASSERTARG(2);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
	if (!RK_ISICHAR(rk_eval_register[1]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[1]);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(2);
	cp[0] = usersyntax_proc;
	cp[1] = RP_CAR(rk_eval_register[0]) & ~7;
	RkMakeMeshTableEntry(c, cp);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

static rk_object usersyn_conti1_proc;
static rk_object usersyn_result_proc, usersyn_error_proc, usersyn_getc_proc, usersyn_read_proc;
static rk_object usersyn_error_conti1_proc, usersyn_getc_conti1_proc, usersyn_read_conti1_proc;

static rk_object
usersyntax(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(38);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = usersyn_conti1_proc;
	cp[2] = (rk_object)rk_error_catcher;
	cp[3] = RK_DUMMY_OBJ;
	cp[4] = (rk_object)&cp[22];
	cp[5] = (rk_object)&cp[6];
	cp[6] = (rk_object)&cp[18];
	cp[7] = (rk_object)&cp[8];
	cp[8] = (rk_object)&cp[14];
	cp[9] = (rk_object)&cp[10];
	cp[10] = rk_eval_register[1];
	cp[11] = (rk_object)&cp[12];
	cp[12] = rk_eval_register[0];
	cp[13] = RK_SOBJ_NIL;
	cp[14] = (rk_object)&cp[16] | 3;
	cp[15] = rp_evlis_proc;
	cp[16] = usersyn_result_proc;
	cp[17] = (rk_object)&cp[30];
	cp[18] = (rk_object)&cp[20] | 3;
	cp[19] = rp_evlis_proc;
	cp[20] = usersyn_error_proc;
	cp[21] = (rk_object)&cp[30];
	cp[22] = (rk_object)&cp[24] | 3;
	cp[23] = rp_evlis_proc;
	cp[24] = usersyn_getc_proc;
	cp[25] = (rk_object)&cp[30];
	cp[26] = (rk_object)&cp[28] | 3;
	cp[27] = rp_evlis_proc;
	cp[28] = usersyn_read_proc;
	cp[29] = (rk_object)&cp[30];
	cp[30] = RK_VECTOR_TAG(8, 0);
	cp[31] = (rk_object)rp_dynamic_extent;
	cp[32] = (rk_object)rk_continuation;
	cp[33] = (rk_object)rk_error_catcher;
	cp[34] = rp_eval_fun;
	cp[35] = ((rk_object *)rk_eval_register[2])[1];
	cp[36] = ((rk_object *)rk_eval_register[2])[2];
	cp[37] = ((rk_object *)rk_eval_register[2])[3];
	rk_continuation = cp;
	rk_eval_register[0] = (rk_object)&cp[26];
	rk_eval_register[1] = (rk_object)&cp[4];
	rk_eval_register[2] = RK_MAKEINUM(6);
	proc = RP_CAR(rk_eval_register[3]);
	rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
	return	proc;
}

static rk_object
usersyn_conti1(void)
{
	rk_error_catcher = (rk_object *)rk_continuation[2];
	RK_SIGNAL_ERROR1(RP_ERROR_EVALRET);
}

static rk_object
usersyn_result(void)
{
	rk_object obj;

	RP_ASSERTARG(3);
	obj = RP_CAR(RP_CDR(rk_eval_register[1]));
	rk_eval_register[2] = RP_CAR(rk_eval_register[1]);
	if (!RK_ISICHAR(rk_eval_register[2]))
		rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = obj;
	rk_valid_register = 3;
	return	RpWindTo(((rk_object *)rk_eval_register[3])[1], ((rk_object *)rk_eval_register[3])[2]
		       , ((rk_object *)rk_eval_register[3])[3], ((rk_object *)rk_eval_register[3])[4]);
}

static rk_object
usersyn_error(void)
{
	rk_object *cp, obj;

	RP_ASSERTARG(3);
	if ((obj = RP_CAR(RP_CDR(rk_eval_register[1]))) == RK_SOBJ_FALSE) {
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(8, 0);
		cp[1] = usersyn_error_conti1_proc;
		cp[2] = RP_CAR(rk_eval_register[1]);
		cp[3] = ((rk_object *)rk_eval_register[3])[1];
		cp[4] = ((rk_object *)rk_eval_register[3])[2];
		cp[5] = ((rk_object *)rk_eval_register[3])[3];
		cp[6] = ((rk_object *)rk_eval_register[3])[4];
		cp[7] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_valid_register = 1;
		return	((rk_object *)rk_eval_register[3])[7];
	}
	rk_error_code = RK_GETINUM(obj);
	rk_error_obj = RP_CAR(rk_eval_register[1]);
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_valid_register = 3;
	return	RpWindTo(((rk_object *)rk_eval_register[3])[1], ((rk_object *)rk_eval_register[3])[2]
		       , ((rk_object *)rk_eval_register[3])[3], ((rk_object *)rk_eval_register[3])[4]);
}

#define DEF_SYNERR_MES	"Error in user syntax"

static rk_object
usersyn_error_conti1(void)
{
	char b[20], *s0, *str;
	int n, nn;
	unsigned l;

	if (!RK_ISSTRING(rk_continuation[2]) || !(n = RP_CAR(rk_continuation[2]) >> 12) || n > 0x10000) {
		s0 = DEF_SYNERR_MES;
		n = sizeof(DEF_SYNERR_MES)-1;
	} else
		s0 = (char *)RkGetMallocObject(rk_continuation[2]);
	l = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (l)
		(void)sprintf(b, " at line %d", l);
	else
		b[0] = '\0';
	nn = strlen(b);
	if (!(str = malloc(n+nn))) {
		rk_error_code = RK_ERROR_OUTOFSTORAGE;
		rk_error_obj = RK_SOBJ_UNSPEC;
	} else {
		memcpy(str, s0, n);
		memcpy(str+n, b, nn);
		if (!(rk_error_obj = RkMakeMallocObject(RK_MALLOC_TAG(n+nn, RK_TCODE_STRING)
							, rk_plain_destructor, str))) {
			free(str);
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
		} else
			rk_error_code = RK_ERROR_READ_SYNTAX;
	}
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	return	RpWindTo(rk_continuation[3], rk_continuation[4], rk_continuation[5], rk_continuation[6]);
}

static rk_object
usersyn_getc(void)
{
	rk_object *cp;

	RP_ASSERTARG(3);
	rk_eval_register[2] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[2] & 7) || (RP_CAR(rk_eval_register[2]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = usersyn_getc_conti1_proc;
	cp[2] = ((rk_object *)rk_eval_register[3])[1];
	cp[3] = ((rk_object *)rk_eval_register[3])[2];
	cp[4] = ((rk_object *)rk_eval_register[3])[3];
	cp[5] = ((rk_object *)rk_eval_register[3])[4];
	cp[6] = RP_CAR(rk_eval_register[2]) & ~7;
	cp[7] = rk_eval_register[0];
	rk_continuation = cp;
	rk_eval_register[0] = RP_CAR(RP_CDR(rk_eval_register[1]));
	rk_valid_register = 1;
	return	((rk_object *)rk_eval_register[3])[5];
}

static rk_object
usersyn_getc_conti1(void)
{
	rk_object *cp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR ||
	    rk_eval_register[0] == RK_SOBJ_EOF && rk_continuation[7] != RK_SOBJ_FALSE) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		return	RpWindTo(rk_continuation[2], rk_continuation[3], rk_continuation[4], rk_continuation[5]);
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = usersyn_conti1_proc;
	cp[2] = rk_continuation[4];
	cp[3] = RK_DUMMY_OBJ;
	cp[4] = rk_eval_register[0];
	cp[5] = RK_SOBJ_NIL;
	rk_eval_register[0] = rk_eval_register[1];
	rk_eval_register[1] = (rk_object)&cp[4];
	rk_eval_register[2] = RK_MAKEINUM(2);
	rk_eval_register[3] = RP_CDR(rk_continuation[6]);
	rk_valid_register = 4;
	proc = RP_CAR(rk_continuation[6]);
	rk_continuation = cp;
	return	proc;
}

static rk_object
usersyn_read(void)
{
	rk_object *cp;

	RP_ASSERTARG(4);
	rk_eval_register[2] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[2] & 7) || (RP_CAR(rk_eval_register[2]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (RP_CAR(RP_CDR(RP_CDR(rk_eval_register[1]))) == RK_SOBJ_FALSE) {
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(8, 0);
		cp[1] = usersyn_getc_conti1_proc;
		cp[2] = ((rk_object *)rk_eval_register[3])[1];
		cp[3] = ((rk_object *)rk_eval_register[3])[2];
		cp[4] = ((rk_object *)rk_eval_register[3])[3];
		cp[5] = ((rk_object *)rk_eval_register[3])[4];
		cp[6] = RP_CAR(rk_eval_register[2]) & ~7;
		cp[7] = rk_eval_register[0];
		rk_continuation = cp;
		cp = (rk_object *)rk_eval_register[3];
		rk_eval_register[3] = RP_CAR(RP_CDR(rk_eval_register[1]));
		rk_eval_register[0] = cp[5];
		rk_eval_register[1] = cp[6];
		rk_eval_register[2] = cp[7];
		return	rk_read_proc;
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = usersyn_read_conti1_proc;
	cp[2] = rk_eval_register[3];
	cp[3] = RP_CAR(rk_eval_register[2]) & ~7;
	cp[4] = rk_eval_register[0];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = RP_CAR(RP_CDR(RP_CDR(rk_eval_register[1])));
	rk_eval_register[1] = RP_CAR(RP_CDR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	((rk_object *)rk_eval_register[3])[6];
}

static rk_object
usersyn_read_conti1(void)
{
	rk_object *cp;

	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = usersyn_getc_conti1_proc;
	cp[2] = ((rk_object *)rk_continuation[2])[1];
	cp[3] = ((rk_object *)rk_continuation[2])[2];
	cp[4] = ((rk_object *)rk_continuation[2])[3];
	cp[5] = ((rk_object *)rk_continuation[2])[4];
	cp[6] = rk_continuation[3];
	cp[7] = rk_continuation[4];
	rk_eval_register[3] = rk_eval_register[0];
	rk_eval_register[0] = ((rk_object *)rk_continuation[2])[5];
	rk_eval_register[1] = ((rk_object *)rk_continuation[2])[6];
	rk_eval_register[2] = ((rk_object *)rk_continuation[2])[7];
	rk_valid_register = 4;
	rk_continuation = cp;
	return  rk_read_proc;
}

int
RpInitializeAbbreviation(int index)
{
	rk_object *cp, sym;

	if (index != -1) {
		cp = RkAllocCells(4);
		cp[0] = RkRegisterProcedure(index + 0, quotes);
		cp[1] = (rk_object)&cp[2];
		cp[2] = cp[3] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		rk_valid_register = 1;
		if (!(sym = GETSYM("quote")))
			return	-1;
		RkWriteCell(&RP_CAR(RP_CDR(rk_eval_register[0])), sym);
		if (!(sym = GETSYM("quasiquote")))
			return	-1;
		RkWriteCell(&RP_CDR(RP_CDR(rk_eval_register[0])), sym);
		RkMakeCharTableEntry('\'', (rk_object *)rk_eval_register[0], RK_CFLAG_PUNCTUATION);
		RkMakeCharTableEntry('`', (rk_object *)rk_eval_register[0], RK_CFLAG_PUNCTUATION);
		quotes_conti1_proc = RkRegisterProcedure(index + 1, quotes_conti1);
		cp = RkAllocCells(4);
		cp[0] = RkRegisterProcedure(index + 2, unquotes);
		cp[1] = (rk_object)&cp[2];
		cp[2] = cp[3] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		if (!(sym = GETSYM("unquote")))
			return	-1;
		RkWriteCell(&RP_CAR(RP_CDR(rk_eval_register[0])), sym);
		if (!(sym = GETSYM("unquote-splicing")))
			return	-1;
		RkWriteCell(&RP_CDR(RP_CDR(rk_eval_register[0])), sym);
		RkMakeCharTableEntry(',', (rk_object *)rk_eval_register[0], RK_CFLAG_PUNCTUATION);
		unquotes_conti1_proc = RkRegisterProcedure(index + 3, unquotes_conti1);
		unquotes_conti2_proc = RkRegisterProcedure(index + 4, unquotes_conti2);
		RP_DEFINESUBR("rp:register-read-syntax", rp_regsyntax_proc, index + 5, regsyntax);
		RP_DEFINESUBR("rp:register-read-dispatcher", rp_regdispatch_proc, index + 6, regdispatch);
		usersyntax_proc = RkRegisterProcedure(index + 7, usersyntax);
		usersyn_conti1_proc = RkRegisterProcedure(index + 8, usersyn_conti1);
		usersyn_result_proc = RkRegisterProcedure(index + 9, usersyn_result);
		usersyn_error_proc = RkRegisterProcedure(index + 10, usersyn_error);
		usersyn_error_conti1_proc = RkRegisterProcedure(index + 11, usersyn_error_conti1);
		usersyn_getc_proc = RkRegisterProcedure(index + 12, usersyn_getc);
		usersyn_getc_conti1_proc = RkRegisterProcedure(index + 13, usersyn_getc_conti1);
		usersyn_read_proc = RkRegisterProcedure(index + 14, usersyn_read);
		usersyn_read_conti1_proc = RkRegisterProcedure(index + 15, usersyn_read_conti1);
		rk_valid_register = 0;
	}
	return	16;
}
