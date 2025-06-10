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
static char rcsid[] = "@(#)$Id: prim_exp.c,v 1.7 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: prim_exp.c,v $
 * Revision 1.7  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.6  1999/02/15 08:47:39  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.5  1998/07/31 11:03:01  qfwfq
 * Add symbol-aux-datum
 *
 * Revision 1.4  1997/05/12 07:21:20  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1997/04/26 13:29:07  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.2  1996/09/06 06:11:29  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:51  qfwfq
 * Initial revision
 *
 */

/*
 * Procedures for primitive expressions.
 */
#include "rhiz_pi.h"

#define MAKEPRIM0(name, index, func)	if (!make_primitive(name, sizeof(name)-1, (index), (func) \
							  , (char const *)0, -1, -1, (rk_object (*)(void))0)) \
						return -1
#define MAKEPRIM(name, index, func, pfunc) \
					if (!make_primitive(name, sizeof(name)-1, (index), (func) \
							  , name "-parsed", sizeof(name)+6, (index)+1, (pfunc))) \
						return -1

#define ASSERTCONS(obj)		do if (!RK_ISCELL(obj) || (RP_CAR(obj) & 1) && (RP_CAR(obj) & 7) != 1) \
					RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX); \
				while (0)
#define ASSERTNCONS(obj)	do if (!RK_ISCELL(obj) || (RP_CAR(obj) & 1)) \
					RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX); \
				while (0)
#define ASSERTEND(obj)		do if ((rk_object)(obj) != RK_SOBJ_NIL) \
					RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX); \
				while (0)

rk_object rp_derivation_proc, rp_cwerr_proc;

static rk_object apply_lambda_proc, apply_lambda_conti1_proc;
static rk_object iff_conti1_proc, setbang_conti1_proc, define_conti1_proc;
static rk_object do_derivation_proc, do_derivation_conti1_proc;
static rk_object cwerr_conti1_proc, cwerr_conti2_proc, cwerr_conti3_proc;

static rk_object
quote(void)
{
	rk_object *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));

	ASSERTNCONS(obj);
	ASSERTEND(RP_CDR(obj));
	cp = RkAllocCells(2);
	cp[0] = RP_CDR(rk_eval_register[2]) | 1;
	cp[1] = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
	RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)cp);
	rk_eval_register[0] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RP_RETURN();
}

static rk_object
quote0(void)
{
	rk_eval_register[0] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RP_RETURN();
}

static rk_object
lambda(void)
{
	rk_object formals, *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));
	unsigned n, i;

	ASSERTNCONS(obj);
	ASSERTCONS(RP_CDR(obj));
	cp = RkAllocCells(6);
	cp[0] = RP_CDR(rk_eval_register[2]) | 1;
	cp[1] = (rk_object)&cp[2];
	cp[2] = RK_VECTOR_TAG(4, 0);
	cp[3] = cp[4] = RK_DUMMY_OBJ;
	cp[5] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	rk_eval_register[2] = (rk_object)cp;
	formals = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
	n = 0;
	while (RK_ISCELL(formals) && !(((rk_object *)formals)[0] & 1)) {
		if (!RK_ISSYMBOL(RP_CAR(formals)))
			RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX);
		formals = RP_CDR(formals);
		++n;
	}
	if (formals == RK_SOBJ_NIL) {
		if (n+1 >= RK_BULK_ALLOC_THRESHOLD)
			RK_SIGNAL_ERROR1(RP_ERROR_TOOMANYFORMALS);
		cp = RkAllocCells((n+2)&~1);
		cp[0] = RK_VECTOR_TAG(n+1, 0);
		cp[((n+2)&~1) - 1] = RK_DUMMY_OBJ;
		formals = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
		for (i = 0; i < n; ++i) {
			cp[i + 1] = RP_CAR(formals);
			formals = RP_CDR(formals);
		}
		((rk_object *)RP_CDR(rk_eval_register[2]))[2] = RK_SOBJ_FALSE;
	} else if (RK_ISSYMBOL(formals)) {
		if (n+2 >= RK_BULK_ALLOC_THRESHOLD)
			RK_SIGNAL_ERROR1(RP_ERROR_TOOMANYFORMALS);
		cp = RkAllocCells((n+3)&~1);
		cp[0] = RK_VECTOR_TAG(n+2, 0);
		cp[((n+3)&~1) - 1] = RK_DUMMY_OBJ;
		formals = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
		for (i = 0; i < n; ++i) {
			cp[i + 1] = RP_CAR(formals);
			formals = RP_CDR(formals);
		}
		cp[i + 1] = formals;
		((rk_object *)RP_CDR(rk_eval_register[2]))[2] = RK_SOBJ_TRUE;
	} else
		RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX);
	RkWriteCell(&((rk_object *)RP_CDR(rk_eval_register[2]))[1], (rk_object)cp);
	RkWriteCell(&RP_CAR(rk_eval_register[1]), rk_eval_register[2]);
	cp = RkAllocCells(6);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = rp_evlis_proc;
	cp[2] = apply_lambda_proc;
	cp[3] = (rk_object)&cp[4];
	cp[4] = rk_eval_register[0];
	cp[5] = RP_CDR(rk_eval_register[2]);
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

static rk_object
lambda0(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = rp_evlis_proc;
	cp[2] = apply_lambda_proc;
	cp[3] = (rk_object)&cp[4];
	cp[4] = rk_eval_register[0];
	cp[5] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

static rk_object
apply_lambda(void)
{
	rk_object *cp, *cp1, obj;
	unsigned n, m, i, j, k;

	n = RK_GETINUM(rk_eval_register[2]);
	obj = RP_CDR(rk_eval_register[3]);
	if (((rk_object *)obj)[2] == RK_SOBJ_FALSE) {
		if (n != (((rk_object *)((rk_object *)obj)[1])[0] >> 12) - 1)
			RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
		cp = RkAllocCells((n+6)&~1);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = (rk_object)(cp1 = &cp[4]);
		cp[2] = ((rk_object *)RP_CDR(rk_eval_register[3]))[1];
		cp[3] = RP_CAR(rk_eval_register[3]);
		cp1[0] = RK_VECTOR_TAG(n+1, 0);
		cp1[((n+2)&~1) - 1] = RK_DUMMY_OBJ;
		if (n > 0) {
			cp1[n] = rk_eval_register[0];
			obj = rk_eval_register[1];
			for (i = n-1; i > 0; --i) {
				cp1[i] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
		}
	} else {
		if (n < (i = (((rk_object *)((rk_object *)obj)[1])[0] >> 12) - 2))
			RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
		m = 4 + ((i+3)&~1) + (n-i)*2;
		rk_eval_register[2] = RK_SOBJ_NIL;
		while (m > RK_HEAP_CHUNK_SIZE) {
			if ((n-i)*2 < (j = RK_HEAP_CHUNK_SIZE))
				j = (n-i)*2;
			cp = RkAllocCells(j);
			cp[j-1] = rk_eval_register[2];
			cp[j-2] = rk_eval_register[0];
			obj = rk_eval_register[1];
			for (k = j-2; k > 0; k -= 2) {
				cp[k-1] = (rk_object)&cp[k];
				cp[k-2] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
			rk_eval_register[2] = (rk_object)cp;
			m -= j;
			if ((n -= j/2) > 0) {
				rk_eval_register[1] = RP_CDR(obj);
				rk_eval_register[0] = RP_CAR(obj);
			}
		}
		cp = RkAllocCells(m);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = (rk_object)(cp1 = &cp[4 + (j = (n-i)*2)]);
		cp[2] = ((rk_object *)RP_CDR(rk_eval_register[3]))[1];
		cp[3] = RP_CAR(rk_eval_register[3]);
		cp[m - 1] = RK_DUMMY_OBJ;
		cp1[0] = RK_VECTOR_TAG(i+2, 0);
		if (j > 0) {
			cp[j+3] = rk_eval_register[2];
			cp[j+2] = rk_eval_register[0];
			obj = rk_eval_register[1];
			while ((j -= 2) > 0) {
				cp[j+3] = (rk_object)&cp[j+4];
				cp[j+2] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
			cp1[i+1] = (rk_object)&cp[4];
			for (k = i; k > 0; --k) {
				cp1[k] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
		} else {
			cp1[i+1] = rk_eval_register[2];
			if (i > 0) {
				cp1[i] = rk_eval_register[0];
				obj = rk_eval_register[1];
				while (--i > 0) {
					cp1[i] = RP_CAR(obj);
					obj = RP_CDR(obj);
				}
			}
		}
	}
	rk_eval_register[0] = (rk_object)cp;
	rk_eval_register[1] = ((rk_object *)RP_CDR(rk_eval_register[3]))[3];
	rk_valid_register = 2;
	if (RP_CDR(rk_eval_register[1]) == RK_SOBJ_NIL)
		return	rp_eval_car_proc;
	ASSERTCONS(RP_CDR(rk_eval_register[1]));
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = apply_lambda_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(rk_eval_register[1]);
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rp_eval_car_proc;
}

static rk_object
apply_lambda_conti1(void)
{
	rk_object *cp;

	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = rk_continuation[3];
	rk_valid_register = 2;
	rk_continuation = (rk_object *)rk_continuation[4];
	if (RP_CDR(rk_eval_register[1]) == RK_SOBJ_NIL)
		return	rp_eval_car_proc;
	ASSERTCONS(RP_CDR(rk_eval_register[1]));
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = apply_lambda_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(rk_eval_register[1]);
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rp_eval_car_proc;
}

static rk_object
iff(void)
{
	rk_object *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));

	ASSERTCONS(obj);
	ASSERTCONS(RP_CDR(obj));
	if (RP_CDR(RP_CDR(obj)) != RK_SOBJ_NIL) {
		ASSERTCONS(RP_CDR(RP_CDR(obj)));
		ASSERTEND(RP_CDR(RP_CDR(RP_CDR(obj))));
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = iff_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	cp[6] = RP_CDR(rk_eval_register[2]) | 1;
	cp[7] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)&cp[6]);
	rk_eval_register[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
iff0(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = iff_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
iff_conti1(void)
{
	rk_object *cp, obj;

	if (rk_eval_register[0] == RK_SOBJ_FALSE) {
		if (RP_CDR(rk_continuation[3]) == RK_SOBJ_NIL) {
			rk_eval_register[0] = RK_SOBJ_UNSPEC;
			rk_continuation = (rk_object *)rk_continuation[4];
			RP_RETURN();
		}
		rk_eval_register[1] = RP_CDR(rk_continuation[3]);
	} else
		rk_eval_register[1] = rk_continuation[3];
	rk_eval_register[0] = rk_continuation[2];
	rk_valid_register = 2;
	rk_continuation = (rk_object *)rk_continuation[4];
	return	rp_eval_car_proc;
}

static rk_object
setbang(void)
{
	rk_object *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));

	ASSERTCONS(obj);
	ASSERTCONS(RP_CDR(obj));
	ASSERTEND(RP_CDR(RP_CDR(obj)));
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = setbang_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CAR(rk_eval_register[1]));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	cp[6] = RP_CDR(rk_eval_register[2]) | 1;
	cp[7] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)&cp[6]);
	rk_eval_register[1] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
setbang0(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = setbang_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CAR(rk_eval_register[1]));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[1] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static RK_INLINE void
writevloc(unsigned long frameno, unsigned long vno)
{
	rk_object **env;

	env = (rk_object **)rk_continuation[2];
	while (frameno--)
		env = (rk_object **)env[3];
	RkWriteCell(&env[1][vno], rk_eval_register[0]);
}

static rk_object
setbang_conti1(void)
{
	rk_object **env, *cp, obj = RP_CAR(rk_continuation[3]);
	unsigned long fn, vn, flen;

	if ((obj & 0xff) == 0x2c)
		writevloc(RP_GETFRAME0(obj), RP_GETVNUM0(obj));
	else if ((obj & 7) == 1)
		RkWriteCell(&((rk_object *)(obj & ~1))[1], rk_eval_register[0]);
	else if (!RK_ISCELL(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX);
	else {
		if ((((rk_object *)obj)[0] & 0xfff) == RK_VECTOR_TAG(0, RP_TCODE_VLOC1))
			writevloc(RP_GETFRAME1(obj), RP_GETVNUM1(obj));
		else if ((((rk_object *)obj)[0] & 7) == 5) {
			env = (rk_object **)rk_continuation[2];
			fn = 0;
			while (RK_ISCELL((rk_object)env)) {
				cp = env[2];
				flen = cp[0] >> 12;
				for (vn = 1; vn < flen; ++vn)
					if (cp[vn] == obj) {
						if ((rk_object)env[3] == RK_SOBJ_NIL)
							RK_SIGNAL_ERROR(RP_ERROR_ROENV, obj);
						RkWriteCell(&env[1][vn], rk_eval_register[0]);
						if (vn >= (1<<16) || fn >= (1<<8)) {
							cp = RkAllocCells(4);
							cp[0] = RK_VECTOR_TAG(4, RP_TCODE_VLOC1);
							cp[1] = RK_MAKEINUM(fn);
							cp[2] = RK_MAKEINUM(vn);
							cp[3] = RK_DUMMY_OBJ;
							RkWriteCell((rk_object *)rk_continuation[3], (rk_object)cp);
						} else
							((rk_object *)rk_continuation[3])[0] = RP_MAKEVLOC0(fn, vn);
						rk_eval_register[0] = RK_SOBJ_UNSPEC;
						rk_continuation = (rk_object *)rk_continuation[4];
						RP_RETURN();
					}
				env = (rk_object **)env[3];
				++fn;
			}
			if ((rk_object)env != RK_DUMMY_OBJ)
				RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, obj);
			((rk_object *)rk_continuation[3])[0] |= 1;
			RkWriteCell(&((rk_object *)obj)[1], rk_eval_register[0]);
		} else
			RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX);
	}
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	rk_continuation = (rk_object *)rk_continuation[4];
	RP_RETURN();
}

static rk_object
begin(void)
{
	rk_object *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));

	ASSERTCONS(obj);
	if (RP_CDR(obj) == RK_SOBJ_NIL) {
		RkWriteCell(&RP_CAR(rk_eval_register[1]), RP_CAR(obj));
		rk_valid_register = 2;
		return	rp_eval_car_proc;
	}
	ASSERTCONS(RP_CDR(obj));
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = apply_lambda_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	cp[6] = RP_CDR(rk_eval_register[2]) | 1;
	cp[7] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)&cp[6]);
	rk_eval_register[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
begin0(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = apply_lambda_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
define(void)
{
	rk_object *cp, obj = RP_CDR(RP_CAR(rk_eval_register[1]));

	ASSERTCONS(obj);
	ASSERTCONS(RP_CDR(obj));
	ASSERTEND(RP_CDR(RP_CDR(obj)));
	if (!RK_ISSYMBOL(RP_CAR(obj)))
		RK_SIGNAL_ERROR1(RP_ERROR_PRIMSYNTAX);
	if (rk_eval_register[0] != RK_DUMMY_OBJ)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALDEF);
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = define_conti1_proc;
	cp[2] = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp[4] = RP_CDR(rk_eval_register[2]) | 1;
	cp[5] = RP_CDR(RP_CAR(rk_eval_register[1]));
	RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)&cp[4]);
	rk_eval_register[1] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
define0(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = define_conti1_proc;
	cp[2] = RP_CAR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[1] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
define_conti1(void)
{
	rk_object *cp;

	RkWriteCell(&((rk_object *)rk_continuation[2])[1], rk_eval_register[0]);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	rk_continuation = (rk_object *)rk_continuation[3];
	RP_RETURN();
}

static rk_object
derivation(void)
{
	rk_object *cp;

	if (rk_eval_register[2] != RK_MAKEINUM(1))
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(4);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = do_derivation_proc;
	cp[2] = rp_noapply_proc;
	cp[3] = rk_eval_register[0];
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

static rk_object
do_derivation(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = do_derivation_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_eval_register[1];
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	proc = RP_CAR(RP_CAR(RP_CDR(rk_eval_register[2])) & ~7);
	rk_eval_register[0] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[3] = RP_CDR(RP_CAR(RP_CDR(rk_eval_register[2])) & ~7);
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_valid_register = 4;
	return	proc;
}

static rk_object
do_derivation_conti1(void)
{
	rk_object *cp;

	RkWriteCell(&RP_CAR(rk_continuation[3]), rk_eval_register[0]);
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = rk_continuation[3];
	rk_valid_register = 2;
	rk_continuation = (rk_object *)rk_continuation[4];
	return	rp_eval_car_proc;
}

static rk_object
cwerr(void)
{
	rk_object *cp, proc;

	if (rk_eval_register[2] != RK_MAKEINUM(2))
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[1] & 7) || (RP_CAR(rk_eval_register[1]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(12);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = cwerr_conti2_proc;
	cp[2] = rk_eval_register[1];
	cp[3] = (rk_object)rp_dynamic_extent;
	cp[4] = (rk_object)rk_error_catcher;
	cp[5] = rp_eval_fun;
	cp[6] = (rk_object)rk_continuation;
	cp[7] = RK_DUMMY_OBJ;
	cp[8] = RK_VECTOR_TAG(4, 0);
	cp[9] = cwerr_conti1_proc;
	cp[10] = (rk_object)rk_error_catcher;
	cp[11] = (rk_object)rk_continuation;
	rk_error_catcher = cp;
	rk_continuation = &cp[8];
	proc = RP_CAR(RP_CAR(rk_eval_register[0]) & ~7);
	rk_eval_register[3] = RP_CDR(RP_CAR(rk_eval_register[0]) & ~7);
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	return	proc;
}

static rk_object
cwerr_conti1(void)
{
	rk_error_catcher = (rk_object *)rk_continuation[2];
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
cwerr_conti2(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cwerr_conti3_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_eval_register[1];
	cp[4] = rk_continuation[2];
	cp[5] = rk_continuation[6];
	rk_valid_register = 0;
	return	RpWindTo(rk_continuation[3], (rk_object)cp, rk_continuation[4], rk_continuation[5]);
}

static rk_object
cwerr_conti3(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(2);
	cp[0] = rk_continuation[3];
	cp[1] = RK_SOBJ_NIL;
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = (rk_object)cp;
	rk_eval_register[2] = RK_MAKEINUM(2);
	rk_eval_register[3] = RP_CDR(RP_CAR(rk_continuation[4]) & ~7);
	rk_valid_register = 4;
	proc = RP_CAR(RP_CAR(rk_continuation[4]) & ~7);
	rk_continuation = (rk_object *)rk_continuation[5];
	return	proc;
}

static int
make_primitive(char const *name, unsigned namelen, int index, rk_object (*func)(void)
	     , char const *parsed_name, unsigned parsed_namelen, int parsed_index, rk_object (*parsed_func)(void))
{
	rk_object *cp;

	if (!parsed_name) {
		if (!(rk_eval_register[0] = RkInternSymbol(name, namelen))) {
			rk_valid_register = 0;
			return	0;
		}
		rk_valid_register = 1;
		cp = RkAllocCells(4);
		cp[0] = (rk_object)&cp[2] | 3;
		cp[1] = RkRegisterProcedure(index, func);
		cp[2] = rp_noapply_proc;
		cp[3] = RK_DUMMY_OBJ;
		RkWriteCell(&((rk_object *)rk_eval_register)[1], (rk_object)cp);
	} else {
		if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(parsed_namelen, RK_TCODE_STRING)
				, (void (*)(void *))0, (char *)parsed_name))) {
			rk_valid_register = 0;
			return	0;
		}
		rk_valid_register = 1;
		cp = RkAllocCells(12);
		cp[0] = (rk_object)&cp[2] | 3;
		cp[1] = RkRegisterProcedure(index, func);
		cp[2] = rp_noapply_proc;
		cp[3] = (rk_object)&cp[4];
		cp[4] = (rk_object)&cp[6] | 5;
		cp[5] = (rk_object)&cp[8];
		cp[6] = rk_eval_register[0];
		cp[7] = RK_SOBJ_UNBOUND;
		cp[8] = (rk_object)&cp[10] | 3;
		cp[9] = RkRegisterProcedure(parsed_index, parsed_func);
		cp[10] = rp_noapply_proc;
		cp[11] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		if (!(cp = (rk_object *)RkInternSymbol(name, namelen))) {
			rk_valid_register = 0;
			return	0;
		}
		RkWriteCell(&cp[1], rk_eval_register[0]);
	}
	return	1;
}

int
RpInitializePrimitives(int index)
{
	if (index != -1) {
		MAKEPRIM("quote", index + 0, quote, quote0);
		MAKEPRIM("rp:lambda", index + 2, lambda, lambda0);
		apply_lambda_proc = RkRegisterProcedure(index + 4, apply_lambda);
		apply_lambda_conti1_proc = RkRegisterProcedure(index + 5, apply_lambda_conti1);
		MAKEPRIM("if", index + 6, iff, iff0);
		iff_conti1_proc = RkRegisterProcedure(index + 8, iff_conti1);
		MAKEPRIM("set!", index + 9, setbang, setbang0);
		setbang_conti1_proc = RkRegisterProcedure(index + 11, setbang_conti1);
		MAKEPRIM("begin", index + 12, begin, begin0);
		MAKEPRIM("rp:define", index + 14, define, define0);
		define_conti1_proc = RkRegisterProcedure(index + 16, define_conti1);
		RP_DEFINESUBR("rp:derivation", rp_derivation_proc, index + 17, derivation);
		do_derivation_proc = RkRegisterProcedure(index + 18, do_derivation);
		do_derivation_conti1_proc = RkRegisterProcedure(index + 19, do_derivation_conti1);
		RP_DEFINESUBR("rp:call-with-error-handler", rp_cwerr_proc, index + 20, cwerr);
		cwerr_conti1_proc = RkRegisterProcedure(index + 21, cwerr_conti1);
		cwerr_conti2_proc = RkRegisterProcedure(index + 22, cwerr_conti2);
		cwerr_conti3_proc = RkRegisterProcedure(index + 23, cwerr_conti3);
		rk_valid_register = 0;
	}
	return	24;
}
