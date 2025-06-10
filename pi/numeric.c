/*
 * Copyright (c) 1993,96-99,2002 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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
static char rcsid[] = "@(#)$Id: numeric.c,v 1.10 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: numeric.c,v $
 * Revision 1.10  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.9  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.8  2002/09/27 12:07:56  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.7  1999/02/15 08:41:42  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.6  1998/07/31 10:53:52  qfwfq
 * Add bitwise operation
 *
 * Revision 1.5  1997/05/12 07:21:18  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.4  1996/10/10 08:26:57  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:11:26  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.2  1996/06/05 05:39:49  qfwfq
 * Add switches concerning IEEE floating point functions.
 *
 * Revision 1.1  93/11/08  14:09:48  qfwfq
 * Initial revision
 * 
 */

/*
 * Numeric functions.
 */
#include "rhiz_pi.h"

#include <math.h>
#include <float.h>
#include <string.h>

#ifdef RK_MY_RINT
#define rint RkRint
#endif

int
RpEqvP(rk_object obj1, rk_object obj2)
{
	int n, i;
	rk_object *cp1, *cp2;

	if (obj1 == obj2)
		return	1;
	if (!(obj1 & 7) && (((rk_object *)obj1)[0] & 0xf) == 7) {
		if ((obj2 & 7) || ((rk_object *)obj1)[0] != ((rk_object *)obj2)[0])
			return	0;
		switch (((rk_object *)obj1)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			cp1 = (rk_object *)obj1;
			cp2 = (rk_object *)obj2;
			n = cp1[0] >> 12;
			for (i = 1; i < n; ++i)
				if (cp1[i] != cp2[i])
					return	0;
			return	1;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			cp1 = (rk_object *)obj1;
			cp2 = (rk_object *)obj2;
			return	(cp1[1] == cp2[1]) && RpEqvP(cp1[2], cp2[2]) && RpEqvP(cp1[3], cp2[3]);
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			cp1 = (rk_object *)obj1;
			cp2 = (rk_object *)obj2;
			return	RpEqvP(cp1[1], cp2[1]) && RpEqvP(cp1[2], cp2[2]);
		default:
			return	0;
		}
	}
	return	0;
}

rk_object rp_equal_proc;
static rk_object
equal(void)
{
	int n, i;
	double re, im;
	rk_object obj, num, num_re, num_im;
	rk_object *cp1, *cp2;

	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RP_CAR(rk_eval_register[1]) == rk_eval_register[0]
					? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	if (RK_GETINUM(rk_eval_register[2]) < 2)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		n = RK_GETINUM(obj);
		obj = rk_eval_register[1];
		while (obj != RK_SOBJ_NIL) {
			if (!RK_ISINUM(RP_CAR(obj))) {
				if (!(RP_CAR(obj) & 7) && (((rk_object *)RP_CAR(obj))[0] & 0xf) == 7)
					switch (((rk_object *)RP_CAR(obj))[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						re = (double)n;
						goto	compare_flonum;
					default:
						break;
					}
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
			if (RK_GETINUM(RP_CAR(obj)) != n) {
				rk_eval_register[0] = RK_SOBJ_FALSE;
				RP_RETURN();
			}
			obj = RP_CDR(obj);
		}
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			num = obj;
			obj = rk_eval_register[1];
			while (obj != RK_SOBJ_NIL) {
				if (RK_ISINUM(RP_CAR(obj))) {
					rk_eval_register[0] = RK_SOBJ_FALSE;
					RP_RETURN();
				}
				if (!(RP_CAR(obj) & 7) && (((rk_object *)RP_CAR(obj))[0] & 0xf) == 7)
					switch (((rk_object *)RP_CAR(obj))[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
						cp1 = (rk_object *)num;
						cp2 = (rk_object *)RP_CAR(obj);
						n = cp1[0] >> 12;
						for (i = 0; i < n; ++i)
							if (cp1[i] != cp2[i]) {
								rk_eval_register[0] = RK_SOBJ_FALSE;
								RP_RETURN();
							}
						obj = RP_CDR(obj);
						continue;
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						re = RkConvertToFloat(num);
						goto	compare_flonum;
					default:
						break;
					}
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			num = obj;
			obj = rk_eval_register[1];
			while (obj != RK_SOBJ_NIL) {
				if (RpEqvP(num, RP_CAR(obj))) {
					obj = RP_CDR(obj);
					continue;
				}
				if (RK_ISINUM(RP_CAR(obj))) {
					rk_eval_register[0] = RK_SOBJ_FALSE;
					RP_RETURN();
				}
				if (!(RP_CAR(obj) & 7) && (((rk_object *)RP_CAR(obj))[0] & 0xf) == 7)
					switch (((rk_object *)RP_CAR(obj))[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						re = RkConvertToFloat(num);
						goto	compare_flonum;
					default:
						break;
					}
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			re = RkLoadFloat(obj);
			obj = rk_eval_register[1];
			while (obj != RK_SOBJ_NIL) {
				if (RK_ISINUM(RP_CAR(obj))) {
					if (re == RkConvertToFloat(RP_CAR(obj))) {
						obj = RP_CDR(obj);
						continue;
					}
					rk_eval_register[0] = RK_SOBJ_FALSE;
					RP_RETURN();
				} else if (!(RP_CAR(obj) & 7) && (((rk_object *)RP_CAR(obj))[0] & 0xf) == 7)
					switch (((rk_object *)RP_CAR(obj))[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
						if (re == RkConvertToFloat(RP_CAR(obj))) {
							obj = RP_CDR(obj);
							continue;
						}
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
compare_flonum:					if (re == RkLoadFloat(RP_CAR(obj))) {
							obj = RP_CDR(obj);
							continue;
						}
						/*FALLTHRU*/
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					default:
						break;
					}
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			num_re = ((rk_object *)obj)[1];
			num_im = ((rk_object *)obj)[2];
			if (!(num_re & 7)
			 && (((rk_object *)num_re)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_FLONUM)) {
				re = RkLoadFloat(num_re);
				im = RkLoadFloat(num_im);
				num_re = RK_DUMMY_OBJ;
			}
			obj = rk_eval_register[1];
			while (obj != RK_SOBJ_NIL) {
				if (RK_ISINUM(RP_CAR(obj))) {
					rk_eval_register[0] = RK_SOBJ_FALSE;
					RP_RETURN();
				} else if (!(RP_CAR(obj) & 7) && (((rk_object *)RP_CAR(obj))[0] & 0xf) == 7)
					switch (((rk_object *)RP_CAR(obj))[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						cp1 = (rk_object *)RP_CAR(obj);
						if (num_re != RK_DUMMY_OBJ) {
							if ((cp1[1] & 7)
							 || ((((rk_object *)(cp1[1]))[0] & 0xfff)
							  != RK_VECTOR_TAG(0, RK_TCODE_FLONUM))) {
								if (RpEqvP(num_re, cp1[1]) && RpEqvP(num_im, cp1[2])) {
									obj = RP_CDR(obj);
									continue;
								}
								rk_eval_register[0] = RK_SOBJ_FALSE;
								RP_RETURN();
							}
							re = RkConvertToFloat(num_re);
							im = RkConvertToFloat(num_im);
							num_re = RK_DUMMY_OBJ;
						}
						if (re == RkConvertToFloat(cp1[1]) && im == RkConvertToFloat(cp1[2])) {
							obj = RP_CDR(obj);
							continue;
						}
						rk_eval_register[0] = RK_SOBJ_FALSE;
						RP_RETURN();
					default:
						break;
					}
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

static rk_object
arith_sub(int (*calc_int)(rk_object, rk_object)
	, int (*calc_frac)(rk_object, rk_object)
	, int (*calc_flo)(double, double)
	, int (*calc_comp_exact)(void)
	, int (*calc_comp_inexact)(void))
{
	rk_object *cp, obj;
	int err;

	for (; rk_eval_register[1] != RK_SOBJ_NIL; rk_eval_register[1] = RP_CDR(rk_eval_register[1])) {
		obj = rk_eval_register[0];
		if (RK_ISINUM(obj)) {
integer_x:		obj = RP_CAR(rk_eval_register[1]);
			if (RK_ISINUM(obj)) {
integer_integer:		if (err = (*calc_int)(rk_eval_register[0], obj))
					RK_SIGNAL_ERROR1(err);
				continue;
			}
			if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
				switch (((rk_object *)obj)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					goto	integer_integer;
				case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
fraction:				if (err = (*calc_frac)(rk_eval_register[0], obj))
						RK_SIGNAL_ERROR1(err);
					continue;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					if (err = (*calc_flo)(RkConvertToFloat(rk_eval_register[0]), RkLoadFloat(obj)))
						RK_SIGNAL_ERROR1(err);
					continue;
				case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
complex:				if ((rk_eval_register[0] & 7)
					 || ((((rk_object *)rk_eval_register[0])[0] & 0xfff)
					  != RK_VECTOR_TAG(0, RK_TCODE_COMPLEX))) {
						rk_eval_register[2] = rk_eval_register[0];
						rk_eval_register[3] = RK_MAKEINUM(0);
					} else {
						rk_eval_register[2] = ((rk_object *)rk_eval_register[0])[1];
						rk_eval_register[3] = ((rk_object *)rk_eval_register[0])[2];
					}
					if ((obj & 7)
					 || (((rk_object *)obj)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_COMPLEX)) {
						rk_eval_register[4] = obj;
						rk_eval_register[5] = RK_MAKEINUM(0);
					} else {
						rk_eval_register[4] = ((rk_object *)obj)[1];
						rk_eval_register[5] = ((rk_object *)obj)[2];
					}
					rk_valid_register = 6;
					if ((!(rk_eval_register[2] & 7)
					  && ((((rk_object *)rk_eval_register[2])[0] & 0xfff)
					   == RK_VECTOR_TAG(0, RK_TCODE_FLONUM)))
					 || (!(rk_eval_register[4] & 7)
					  && ((((rk_object *)rk_eval_register[4])[0] & 0xfff)
					   == RK_VECTOR_TAG(0, RK_TCODE_FLONUM)))) {
						if (err = (*calc_comp_inexact)())
							RK_SIGNAL_ERROR1(err);
						if (RkLoadFloat(rk_eval_register[0]) == 0.0) {
							rk_eval_register[0] = rk_eval_register[2];
							continue;
						}
					} else {
						if (err = (*calc_comp_exact)())
							RK_SIGNAL_ERROR1(err);
						if (RK_ISINUM(rk_eval_register[0])
						 && RK_GETINUM(rk_eval_register[0]) == 0) {
							rk_eval_register[0] = rk_eval_register[2];
							continue;
						}
					}
					cp = RkAllocCells(4);
					cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
					cp[1] = rk_eval_register[2];
					cp[2] = rk_eval_register[0];
					cp[3] = RK_DUMMY_OBJ;
					rk_eval_register[0] = (rk_object)cp;
					continue;
				default:
					break;
				}
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
		if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				goto	integer_x;
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
				obj = RP_CAR(rk_eval_register[1]);
				if (RK_ISINUM(obj))
					goto	fraction;
				if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
					switch (((rk_object *)obj)[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
						goto	fraction;
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						if (err = (*calc_flo)(RkConvertToFloat(rk_eval_register[0])
									, RkLoadFloat(obj)))
							RK_SIGNAL_ERROR1(err);
						continue;
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						goto	complex;
					default:
						break;
					}
				break;
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				obj = RP_CAR(rk_eval_register[1]);
				if (RK_ISINUM(obj)) {
float_x:				if (err = (*calc_flo)(RkLoadFloat(rk_eval_register[0]), RkConvertToFloat(obj)))
						RK_SIGNAL_ERROR1(err);
					continue;
				}
				if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
					switch (((rk_object *)obj)[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
						goto	float_x;
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						goto	complex;
					default:
						break;
					}
				break;
			case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
				obj = RP_CAR(rk_eval_register[1]);
				if (RK_ISINUM(obj))
					goto	complex;
				if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
					switch (((rk_object *)obj)[0] & 0xfff) {
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
					case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
						goto	complex;
					default:
						break;
					}
				break;
			default:
				break;
			}
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
	RP_RETURN();
}

static int
add_int(rk_object num1, rk_object num2)
{
	RkLoadInteger(num1, 0);
	RkLoadInteger(num2, 1);
	if (!RkAddIntByInt(0, 1, 2))
		return	RK_ERROR_OVERFLOW;
	rk_eval_register[0] = RkStoreInt(2);
	return	0;
}

static int
add_frac(rk_object num1, rk_object num2)
{
	if ((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num1, 0);
		RkLoadShortInt(1, 1);
	} else {
		RkLoadInteger(((rk_object *)num1)[2], 0);
		if (RK_GETINUM(((rk_object *)num1)[1]))
			RkNegateInt(0);
		RkLoadInteger(((rk_object *)num1)[3], 1);
	}
	if ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num2, 2);
		RkLoadShortInt(1, 3);
	} else {
		RkLoadInteger(((rk_object *)num2)[2], 2);
		if (RK_GETINUM(((rk_object *)num2)[1]))
			RkNegateInt(2);
		RkLoadInteger(((rk_object *)num2)[3], 3);
	}
	if ((rk_eval_register[0] = RkAddFraction(0, 1, 2, 3)) == RK_SOBJ_ERROR)
		return	RK_ERROR_OVERFLOW;
	return	0;
}

static int
add_flo(double x, double y)
{
	rk_eval_register[0] = RkStoreFloat(x + y);
	return	0;
}

static int
add_comp_exact(void)
{
	int err;

	if (((rk_eval_register[2] & 7)
	  || (((rk_object *)rk_eval_register[2])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((rk_eval_register[4] & 7)
	  || (((rk_object *)rk_eval_register[4])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		err = add_int(rk_eval_register[2], rk_eval_register[4]);
	else
		err = add_frac(rk_eval_register[2], rk_eval_register[4]);
	if (err)
		return	err;
	rk_eval_register[2] = rk_eval_register[0];
	if (((rk_eval_register[3] & 7)
	  || (((rk_object *)rk_eval_register[3])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((rk_eval_register[5] & 7)
	  || (((rk_object *)rk_eval_register[5])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		err = add_int(rk_eval_register[3], rk_eval_register[5]);
	else
		err = add_frac(rk_eval_register[3], rk_eval_register[5]);
	if (err)
		return	err;
	return	0;
}

static int
add_comp_inexact(void)
{
	rk_eval_register[2] = RkStoreFloat(RkConvertToFloat(rk_eval_register[2])
					 + RkConvertToFloat(rk_eval_register[4]));
	rk_eval_register[0] = RkStoreFloat(RkConvertToFloat(rk_eval_register[3])
					 + RkConvertToFloat(rk_eval_register[5]));
	return	0;
}

rk_object rp_add_proc;
static rk_object
add(void)
{
	int n;
	rk_object obj;

	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		n = RK_GETINUM(RP_CAR(rk_eval_register[1])) + RK_GETINUM(rk_eval_register[0]);
		obj = RK_MAKEINUM(n);
		if (RK_GETINUM(obj) == n) {
			rk_eval_register[0] = obj;
			RP_RETURN();
		}
	}
	if (RK_GETINUM(rk_eval_register[2]) == 0) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	arith_sub(add_int, add_frac, add_flo, add_comp_exact, add_comp_inexact);
}

rk_object rp_numberp_proc;
static rk_object
numberp(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		default:
			break;
		}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_exactp_proc;
static rk_object
exactp(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	for (; ; ) {
		if (RK_ISINUM(obj)) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
				rk_eval_register[0] = RK_SOBJ_TRUE;
				RP_RETURN();
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				rk_eval_register[0] = RK_SOBJ_FALSE;
				RP_RETURN();
			case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
				obj = ((rk_object *)obj)[1];
				continue;
			default:
				break;
			}
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
}

rk_object rp_inexactp_proc;
static rk_object
inexactp(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	for (; ; ) {
		if (RK_ISINUM(obj)) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
				rk_eval_register[0] = RK_SOBJ_FALSE;
				RP_RETURN();
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				rk_eval_register[0] = RK_SOBJ_TRUE;
				RP_RETURN();
			case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
				obj = ((rk_object *)obj)[1];
				continue;
			default:
				break;
			}
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
}

#define COMP_ERROR	1
#define COMP_INEXACT	2

#define COMP_LT		1
#define COMP_EQ		2
#define COMP_GT		4

static int
compare_short_int(int m, int n)
{
	if (m < n)
		return	COMP_LT;
	else if (m == n)
		return	COMP_EQ;
	else
		return	COMP_GT;
}

static int
compare_big_int(void)
{
	switch (RkCompareIntByInt(0, 1)) {
	case -1:	return	COMP_LT;
	case 0:		return	COMP_EQ;
	case 1:		return	COMP_GT;
	}
}

static int
compare_fraction(void)
{
	switch (RkCompareFraction(0, 1, 2, 3)) {
	case -1:	return	COMP_LT;
	case 0:		return	COMP_EQ;
	case 1:		return	COMP_GT;
	defualt:	return	0;
	}
}

static int
compare_flonum(double x, double y)
{
	if (x < y)
		return	COMP_LT;
	else if (x == y)
		return	COMP_EQ;
	else
		return	COMP_GT;
}

static int
compare_real(rk_object num1, rk_object num2, int *flag)
{
	int m, n;
	double x, y;

	*flag = 0;
	if (RK_ISINUM(num1)) {
		m = RK_GETINUM(num1);
		if (RK_ISINUM(num2)) {
			n = RK_GETINUM(num2);
			return	compare_short_int(m, n);
		} else if (!(num2 & 7) && (((rk_object *)num2)[0] & 0xf) == 7)
			switch (((rk_object *)num2)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				return	COMP_LT;
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				return	COMP_GT;
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
				RkLoadShortInt(m, 0);
				RkLoadShortInt(1, 1);
				RkLoadInteger(((rk_object *)num2)[2], 2);
				if (RK_GETINUM(((rk_object *)num2)[1]))
					RkNegateInt(2);
				RkLoadInteger(((rk_object *)num2)[3], 3);
				if (!(m = compare_fraction())) {
					*flag = COMP_ERROR;
					return	RK_ERROR_OVERFLOW;
				}
				return	m;
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				*flag = COMP_INEXACT;
				x = (double)m;
				y = RkLoadFloat(num2);
				return	compare_flonum(x, y);
			default:
				break;
			}
		*flag = COMP_ERROR;
		return	RP_ERROR_ILLEGALARG;
	}
	if (!(num1 & 7) && (((rk_object *)num1)[0] & 0xf) == 7)
		switch (((rk_object *)num1)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			if (RK_ISINUM(num2))
				return	((((rk_object *)num1)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS))
						? COMP_GT : COMP_LT;
			else if (!(num2 & 7) && (((rk_object *)num2)[0] & 0xf) == 7)
				switch (((rk_object *)num2)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					if ((((rk_object *)num1)[0] & 0xfff) != (((rk_object *)num2)[0] & 0xfff))
						return	((((rk_object *)num1)[0] & 0xfff)
							 == RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS))
								? COMP_GT : COMP_LT;
					RkLoadBigInt(num1, 0);
					RkLoadBigInt(num2, 1);
					return	compare_big_int();
				case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					RkLoadBigInt(num1, 0);
					RkLoadShortInt(1, 1);
					RkLoadInteger(((rk_object *)num2)[2], 2);
					if (RK_GETINUM(((rk_object *)num2)[1]))
						RkNegateInt(2);
					RkLoadInteger(((rk_object *)num2)[3], 3);
					if (!(m = compare_fraction())) {
						*flag = COMP_ERROR;
						return	RK_ERROR_OVERFLOW;
					}
					return	m;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					*flag = COMP_INEXACT;
					x = RkConvertToFloat(num1);
					y = RkLoadFloat(num2);
					return	compare_flonum(x, y);
				default:
					break;
				}
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			RkLoadInteger(((rk_object *)num1)[2], 0);
			if (RK_GETINUM(((rk_object *)num1)[1]))
				RkNegateInt(0);
			RkLoadInteger(((rk_object *)num1)[3], 1);
			if (RK_ISINUM(num2)) {
				RkLoadShortInt(RK_GETINUM(num2), 2);
				RkLoadShortInt(1, 3);
				if (!(m = compare_fraction())) {
					*flag = COMP_ERROR;
					return	RK_ERROR_OVERFLOW;
				}
				return	m;
			} else if (!(num2 & 7) && (((rk_object *)num2)[0] & 0xf) == 7)
				switch (((rk_object *)num2)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					RkLoadBigInt(num2, 2);
					RkLoadShortInt(1, 3);
					if (!(m = compare_fraction())) {
						*flag = COMP_ERROR;
						return	RK_ERROR_OVERFLOW;
					}
					return	m;
				case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					RkLoadInteger(((rk_object *)num2)[2], 2);
					if (RK_GETINUM(((rk_object *)num2)[1]))
						RkNegateInt(2);
					RkLoadInteger(((rk_object *)num2)[3], 3);
					if (!(m = compare_fraction())) {
						*flag = COMP_ERROR;
						return	RK_ERROR_OVERFLOW;
					}
					return	m;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					*flag = COMP_INEXACT;
					x = RkIntToFloat(0) / RkIntToFloat(1);
					y = RkLoadFloat(num2);
					return	compare_flonum(x, y);
				default:
					break;
				}
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			*flag = COMP_INEXACT;
			x = RkLoadFloat(num1);
			if (RK_ISINUM(num2))
				y = (double)RK_GETINUM(num2);
			else if (!(num2 & 7) && (((rk_object *)num2)[0] & 0xf) == 7)
				switch (((rk_object *)num2)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
					y = RkConvertToFloat(num2);
					break;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					y = RkLoadFloat(num2);
					break;
				default:
					*flag = COMP_ERROR;
					return	RP_ERROR_ILLEGALARG;
				}
			else
				break;
			return	compare_flonum(x, y);
		default:
			break;
		}
	*flag = COMP_ERROR;
	return	RP_ERROR_ILLEGALARG;
}

static rk_object
compare_sub(int cond)
{
	rk_object obj, num1, num2;
	int comp, flag;

	if (RK_GETINUM(rk_eval_register[2]) < 2)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	num2 = rk_eval_register[0];
	obj = rk_eval_register[1];
	while (obj != RK_SOBJ_NIL) {
		num1 = RP_CAR(obj);
		comp = compare_real(num1, num2, &flag);
		if (flag & COMP_ERROR)
			RK_SIGNAL_ERROR1(comp);
		if (!(cond & comp)) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		num2 = num1;
		obj = RP_CDR(obj);
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_lt_proc;
static rk_object
lt(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) < RK_GETINUM(rk_eval_register[0])
					? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	return	compare_sub(COMP_LT);
}

rk_object rp_gt_proc;
static rk_object
gt(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) > RK_GETINUM(rk_eval_register[0])
					? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	return	compare_sub(COMP_GT);
}

rk_object rp_le_proc;
static rk_object
le(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) <= RK_GETINUM(rk_eval_register[0])
					? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	return	compare_sub(COMP_LT|COMP_EQ);
}

rk_object rp_ge_proc;
static rk_object
ge(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) >= RK_GETINUM(rk_eval_register[0])
					? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	return	compare_sub(COMP_EQ|COMP_GT);
}

rk_object rp_zerop_proc;
static rk_object
zerop(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		if (RK_GETINUM(obj) == 0)
			rk_eval_register[0] = RK_SOBJ_TRUE;
		else
			rk_eval_register[0] = RK_SOBJ_FALSE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			if (RkLoadFloat(obj) == 0.0)
				rk_eval_register[0] = RK_SOBJ_TRUE;
			else
				rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_positivep_proc;
static rk_object
positivep(void)
{
	int comp, flag;

	RP_ASSERTARG(1);
	if (RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(rk_eval_register[0]) > 0 ? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	comp = compare_real(RK_MAKEINUM(0), rk_eval_register[0], &flag);
	if (flag & COMP_ERROR)
		RK_SIGNAL_ERROR1(comp);
	if (comp & COMP_LT)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_negativep_proc;
static rk_object
negativep(void)
{
	int comp, flag;

	RP_ASSERTARG(1);
	if (RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(rk_eval_register[0]) < 0 ? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
		RP_RETURN();
	}
	comp = compare_real(RK_MAKEINUM(0), rk_eval_register[0], &flag);
	if (flag & COMP_ERROR)
		RK_SIGNAL_ERROR1(comp);
	if (comp & COMP_GT)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_oddp_proc;
static rk_object
oddp(void)
{
	rk_object obj;
	double x;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		if (RK_GETINUM(obj) & 1)
			rk_eval_register[0] = RK_SOBJ_TRUE;
		else
			rk_eval_register[0] = RK_SOBJ_FALSE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			if (((rk_object *)obj)[1] & (1<<2))
				rk_eval_register[0] = RK_SOBJ_TRUE;
			else
				rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(obj);
			if (x != floor(x))
				break;
			if (x != floor(x/2)*2)
				rk_eval_register[0] = RK_SOBJ_TRUE;
			else
				rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_evenp_proc;
static rk_object
evenp(void)
{
	rk_object obj;
	double x;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		if (RK_GETINUM(obj) & 1)
			rk_eval_register[0] = RK_SOBJ_FALSE;
		else
			rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			if (((rk_object *)obj)[1] & (1<<2))
				rk_eval_register[0] = RK_SOBJ_FALSE;
			else
				rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(obj);
			if (x != floor(x))
				break;
			if (x != floor(x/2)*2)
				rk_eval_register[0] = RK_SOBJ_FALSE;
			else
				rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

static rk_object
max_sub(int cond)
{
	rk_object obj, num1, num2;
	int comp, flag, inexactp;

	if (RK_GETINUM(rk_eval_register[2]) < 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	inexactp = 0;
	num1 = rk_eval_register[0];
	obj = rk_eval_register[1];
	while (obj != RK_SOBJ_NIL) {
		num2 = RP_CAR(obj);
		comp = compare_real(num1, num2, &flag);
		if (flag & COMP_ERROR)
			RK_SIGNAL_ERROR1(comp);
		if (comp & cond)
			num1 = num2;
		inexactp |= flag;
		obj = RP_CDR(obj);
	}
	if (inexactp & COMP_INEXACT)
		rk_eval_register[0] = RkStoreFloat(RkConvertToFloat(num1));
	else
		rk_eval_register[0] = num1;
	RP_RETURN();
}

rk_object rp_max_proc;
static rk_object
mmaaxx(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) < RK_GETINUM(rk_eval_register[0])
					? rk_eval_register[0] : RP_CAR(rk_eval_register[1]));
		RP_RETURN();
	}
	return	max_sub(COMP_LT);
}

rk_object rp_min_proc;
static rk_object
mmiinn(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		rk_eval_register[0] = (RK_GETINUM(RP_CAR(rk_eval_register[1])) > RK_GETINUM(rk_eval_register[0])
					? rk_eval_register[0] : RP_CAR(rk_eval_register[1]));
		RP_RETURN();
	}
	return	max_sub(COMP_GT);
}

static int
subtract_int(rk_object num1, rk_object num2)
{
	RkLoadInteger(num1, 0);
	RkLoadInteger(num2, 1);
	RkNegateInt(1);
	if (!RkAddIntByInt(0, 1, 2))
		return	RK_ERROR_OVERFLOW;
	rk_eval_register[0] = RkStoreInt(2);
	return	0;
}

static int
subtract_frac(rk_object num1, rk_object num2)
{
	if ((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num1, 0);
		RkLoadShortInt(1, 1);
	} else {
		RkLoadInteger(((rk_object *)num1)[2], 0);
		if (RK_GETINUM(((rk_object *)num1)[1]))
			RkNegateInt(0);
		RkLoadInteger(((rk_object *)num1)[3], 1);
	}
	if ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num2, 2);
		RkNegateInt(2);
		RkLoadShortInt(1, 3);
	} else {
		RkLoadInteger(((rk_object *)num2)[2], 2);
		if (!RK_GETINUM(((rk_object *)num2)[1]))
			RkNegateInt(2);
		RkLoadInteger(((rk_object *)num2)[3], 3);
	}
	if ((rk_eval_register[0] = RkAddFraction(0, 1, 2, 3)) == RK_SOBJ_ERROR)
		return	RK_ERROR_OVERFLOW;
	return	0;
}

static int
times_int(rk_object num1, rk_object num2)
{
	RkLoadInteger(num1, 0);
	RkLoadInteger(num2, 1);
	if (!RkMultiplyIntByInt(0, 1, 2))
		return	RK_ERROR_OVERFLOW;
	rk_eval_register[0] = RkStoreInt(2);
	return	0;
}

static int
times_frac(rk_object num1, rk_object num2)
{
	if ((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num1, 0);
		RkLoadShortInt(1, 1);
	} else {
		RkLoadInteger(((rk_object *)num1)[2], 0);
		if (RK_GETINUM(((rk_object *)num1)[1]))
			RkNegateInt(0);
		RkLoadInteger(((rk_object *)num1)[3], 1);
	}
	if ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num2, 2);
		RkLoadShortInt(1, 3);
	} else {
		RkLoadInteger(((rk_object *)num2)[2], 2);
		if (RK_GETINUM(((rk_object *)num2)[1]))
			RkNegateInt(2);
		RkLoadInteger(((rk_object *)num2)[3], 3);
	}
	if ((rk_eval_register[0] = RkMultiplyFraction(0, 1, 2, 3)) == RK_SOBJ_ERROR)
		return	RK_ERROR_OVERFLOW;
	return	0;
}

static int
times_flo(double x, double y)
{
	rk_eval_register[0] = RkStoreFloat(x * y);
	return	0;
}

static int
add_exact(rk_object num1, rk_object num2)
{
	if (((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		return	add_int(num1, num2);
	else
		return	add_frac(num1, num2);
}

static int
subtract_exact(rk_object num1, rk_object num2)
{
	if (((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		return	subtract_int(num1, num2);
	else
		return	subtract_frac(num1, num2);
}

static int
times_exact(rk_object num1, rk_object num2)
{
	if (((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		return	times_int(num1, num2);
	else
		return	times_frac(num1, num2);
}

static int
times_comp_exact(void)
{
	int err;

	if (err = times_exact(rk_eval_register[2], rk_eval_register[4]))
		return	err;
	rk_eval_register[6] = rk_eval_register[0];
	rk_valid_register = 7;
	if (err = times_exact(rk_eval_register[3], rk_eval_register[5]))
		return	err;
	if (err = subtract_exact(rk_eval_register[6], rk_eval_register[0]))
		return	err;
	rk_eval_register[6] = rk_eval_register[0];
	if (err = times_exact(rk_eval_register[2], rk_eval_register[5]))
		return	err;
	rk_eval_register[7] = rk_eval_register[0];
	rk_valid_register = 8;
	if (err = times_exact(rk_eval_register[3], rk_eval_register[4]))
		return	err;
	if (err = add_exact(rk_eval_register[0], rk_eval_register[7]))
		return	err;
	rk_eval_register[2] = rk_eval_register[6];
	return	0;
}

static int
times_comp_inexact(void)
{
	double r1, i1, r2, i2;

	r1 = RkConvertToFloat(rk_eval_register[2]);
	i1 = RkConvertToFloat(rk_eval_register[3]);
	r2 = RkConvertToFloat(rk_eval_register[4]);
	i2 = RkConvertToFloat(rk_eval_register[5]);
	rk_eval_register[2] = RkStoreFloat(r1*r2 - i1*i2);
	rk_eval_register[0] = RkStoreFloat(r1*i2 + r2*i1);
	return	0;
}

rk_object rp_times_proc;
static rk_object
times(void)
{
	int n, i, j;
	rk_object obj;

	if (RK_GETINUM(rk_eval_register[2]) == 2
	 && RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
		i = RK_GETINUM(RP_CAR(rk_eval_register[1]));
		j = RK_GETINUM(rk_eval_register[0]);
		if (i == 0) {
			rk_eval_register[0] = RK_MAKEINUM(0);
			RP_RETURN();
		}
		n = i*j;
		obj = RK_MAKEINUM(n);
		if (RK_GETINUM(obj) == n && n/i == j) {
			rk_eval_register[0] = obj;
			RP_RETURN();
		}
	}
	if (RK_GETINUM(rk_eval_register[2]) == 0) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	return	arith_sub(times_int, times_frac, times_flo, times_comp_exact, times_comp_inexact);
}

static int
subtract_flo(double x, double y)
{
	rk_eval_register[0] = RkStoreFloat(x - y);
	return	0;
}

static int
subtract_comp_exact(void)
{
	int err;

	if (((rk_eval_register[2] & 7)
	  || (((rk_object *)rk_eval_register[2])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((rk_eval_register[4] & 7)
	  || (((rk_object *)rk_eval_register[4])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		err = subtract_int(rk_eval_register[2], rk_eval_register[4]);
	else
		err = subtract_frac(rk_eval_register[2], rk_eval_register[4]);
	if (err)
		return	err;
	rk_eval_register[2] = rk_eval_register[0];
	if (((rk_eval_register[3] & 7)
	  || (((rk_object *)rk_eval_register[3])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((rk_eval_register[5] & 7)
	  || (((rk_object *)rk_eval_register[5])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		err = subtract_int(rk_eval_register[3], rk_eval_register[5]);
	else
		err = subtract_frac(rk_eval_register[3], rk_eval_register[5]);
	if (err)
		return	err;
	return	0;
}

static int
subtract_comp_inexact(void)
{
	rk_eval_register[2] = RkStoreFloat(RkConvertToFloat(rk_eval_register[2])
					 - RkConvertToFloat(rk_eval_register[4]));
	rk_eval_register[0] = RkStoreFloat(RkConvertToFloat(rk_eval_register[3])
					 - RkConvertToFloat(rk_eval_register[5]));
	return	0;
}

rk_object rp_minus_proc;
static rk_object
minus(void)
{
	int n;
	rk_object *cp, obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	case 1:
		if (RK_ISINUM(rk_eval_register[0])) {
			n = -RK_GETINUM(rk_eval_register[0]);
			obj = RK_MAKEINUM(n);
			if (RK_GETINUM(obj) == n) {
				rk_eval_register[0] = obj;
				RP_RETURN();
			}
		}
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = RK_MAKEINUM(0);
		rk_eval_register[1] = (rk_object)cp;
		break;
	case 2:
		if (RK_ISINUM(RP_CAR(rk_eval_register[1])) && RK_ISINUM(rk_eval_register[0])) {
			n = RK_GETINUM(RP_CAR(rk_eval_register[1])) - RK_GETINUM(rk_eval_register[0]);
			obj = RK_MAKEINUM(n);
			if (RK_GETINUM(obj) == n) {
				rk_eval_register[0] = obj;
				RP_RETURN();
			}
		}
		/*FALLTHRU*/
	default:
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = (rk_object)cp;
		while (RP_CDR(rk_eval_register[1]) != RK_SOBJ_NIL) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[1]);
			cp[1] = rk_eval_register[0];
			rk_eval_register[0] = (rk_object)cp;
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
		obj = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = rk_eval_register[0];
		rk_eval_register[0] = obj;
		break;
	}
	return	arith_sub(subtract_int, subtract_frac, subtract_flo, subtract_comp_exact, subtract_comp_inexact);
}

rk_object rp_abs_proc;
static rk_object
aabbss(void)
{
	rk_object *cp, num;
	int n;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		n = RK_GETINUM(num);
		if (n < 0) {
			RkLoadShortInt(n, 0);
			RkNegateInt(0);
			rk_eval_register[0] = RkStoreInt(0);
		} else
			rk_eval_register[0] = num;
		RP_RETURN();
	} else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7) {
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			rk_eval_register[0] = num;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			RkNegateInt(0);
			rk_eval_register[0] = RkStoreInt(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			if (!RK_GETINUM(((rk_object *)num)[1])) {
				rk_eval_register[0] = num;
				break;
			}
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(0, RK_TCODE_FRACTION);
			cp[1] = RK_MAKEINUM(0);
			cp[2] = ((rk_object *)rk_eval_register[0])[2];
			cp[3] = ((rk_object *)rk_eval_register[0])[3];
			rk_eval_register[0] = (rk_object)cp;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RkStoreFloat(fabs(RkLoadFloat(num)));
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
		RP_RETURN();
	}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_quotient_proc;
static rk_object
quotient(void)
{
	int q;
	rk_object m, n, obj;
	double x, y, z;

	RP_ASSERTARG(2);
	m = RP_CAR(rk_eval_register[1]);
	n = rk_eval_register[0];
	if (RK_ISINUM(m) && RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
		q = RK_GETINUM(m) / RK_GETINUM(n);
		obj = RK_MAKEINUM(q);
		if (RK_GETINUM(obj) == q) {
			rk_eval_register[0] = obj;
			RP_RETURN();
		}
	}
	if (RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
x_int:		if (RK_ISINUM(m)) {
int_int:		RkLoadInteger(m, 0);
			RkLoadInteger(n, 1);
			RkDivideIntByInt(0, 1, 2);
			rk_eval_register[0] = RkStoreInt(2);
			RP_RETURN();
		} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
			switch (((rk_object *)m)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				goto	int_int;
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				x = RkLoadFloat(m);
				y = RkConvertToFloat(n);
				if (floor(x) != x)
					RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
inexact:			z = x/y;
				if (z >= 0.0)
					z = floor(z);
				else
					z = -floor(-z);
				rk_eval_register[0] = RkStoreFloat(z);
				RP_RETURN();
			default:
				break;
			}
	} else if (!(n & 7) && (((rk_object *)n)[0] & 0xf) == 7)
		switch (((rk_object *)n)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			goto	x_int;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			y = RkLoadFloat(n);
			if (y == 0.0)
				RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
			if (floor(y) != y)
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			if (RK_ISINUM(m)) {
int_float:			x = RkConvertToFloat(m);
				goto	inexact;
			} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
				switch (((rk_object *)m)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					goto	int_float;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					x = RkLoadFloat(m);
					if (floor(x) != x)
						RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
					goto	inexact;
				default:
					break;
				}
			break;
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_remainder_proc;
static rk_object
rpremainder(void)
{
	rk_object m, n;
	double x, y, z;

	RP_ASSERTARG(2);
	m = RP_CAR(rk_eval_register[1]);
	n = rk_eval_register[0];
	if (RK_ISINUM(m) && RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
		rk_eval_register[0] = RK_MAKEINUM(RK_GETINUM(m) % RK_GETINUM(n));
		RP_RETURN();
	}
	if (RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
x_int:		if (RK_ISINUM(m)) {
int_int:		RkLoadInteger(m, 0);
			RkLoadInteger(n, 1);
			RkDivideIntByInt(0, 1, 2);
			rk_eval_register[0] = RkStoreInt(0);
			RP_RETURN();
		} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
			switch (((rk_object *)m)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				goto	int_int;
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				x = RkLoadFloat(m);
				y = RkConvertToFloat(n);
				if (floor(x) != x)
					RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
inexact:			z = x/y;
				if (z >= 0.0)
					z = floor(z);
				else
					z = -floor(-z);
				rk_eval_register[0] = RkStoreFloat(x - z*y);
				RP_RETURN();
			default:
				break;
			}
	} else if (!(n & 7) && (((rk_object *)n)[0] & 0xf) == 7)
		switch (((rk_object *)n)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			goto	x_int;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			y = RkLoadFloat(n);
			if (y == 0.0)
				RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
			if (floor(y) != y)
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			if (RK_ISINUM(m)) {
int_float:			x = RkConvertToFloat(m);
				goto	inexact;
			} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
				switch (((rk_object *)m)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					goto	int_float;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					x = RkLoadFloat(m);
					if (floor(x) != x)
						RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
					goto	inexact;
				default:
					break;
				}
			break;
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_modulo_proc;
static rk_object
modulo(void)
{
	rk_object m, n;
	int mi, ni, msig, nsig;
	double x, y, z;

	RP_ASSERTARG(2);
	m = RP_CAR(rk_eval_register[1]);
	n = rk_eval_register[0];
	if (RK_ISINUM(m) && RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
		msig = ((mi = RK_GETINUM(m)) < 0);
		nsig = ((ni = RK_GETINUM(n)) < 0);
		if ((mi = mi % ni) != 0 && msig != nsig)
			mi += ni;
		rk_eval_register[0] = RK_MAKEINUM(mi);
		RP_RETURN();
	}
	if (RK_ISINUM(n)) {
		if (n == RK_MAKEINUM(0))
			RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
		nsig = (RK_GETINUM(n) < 0);
x_int:		if (RK_ISINUM(m)) {
			msig = (RK_GETINUM(m) < 0);
int_int:		RkLoadInteger(m, 0);
			RkLoadInteger(n, 1);
			if (msig != nsig)
				RkCopyInt(1, 3);
			RkDivideIntByInt(0, 1, 2);
			if (RkIntegerIsZero(0))
				rk_eval_register[0] = RK_MAKEINUM(0);
			else if (msig == nsig)
				rk_eval_register[0] = RkStoreInt(0);
			else {
				RkAddIntByInt(0, 3, 1);
				rk_eval_register[0] = RkStoreInt(1);
			}
			RP_RETURN();
		} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
			switch (((rk_object *)m)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				msig = 0;
				goto	int_int;
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				msig = 1;
				goto	int_int;
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				x = RkLoadFloat(m);
				y = RkConvertToFloat(n);
				if (floor(x) != x)
					RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
inexact:			z = floor(x/y);
				rk_eval_register[0] = RkStoreFloat(x - z*y);
				RP_RETURN();
			default:
				break;
			}
	} else if (!(n & 7) && (((rk_object *)n)[0] & 0xf) == 7)
		switch (((rk_object *)n)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			nsig = 0;
			goto	x_int;
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			nsig = 1;
			goto	x_int;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			y = RkLoadFloat(n);
			if (y == 0.0)
				RK_SIGNAL_ERROR1(RK_ERROR_DIVIDEBYZERO);
			if (floor(y) != y)
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			if (RK_ISINUM(m)) {
int_float:			x = RkConvertToFloat(m);
				goto	inexact;
			} else if (!(m & 7) && (((rk_object *)m)[0] & 0xf) == 7)
				switch (((rk_object *)m)[0] & 0xfff) {
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
				case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
					goto	int_float;
				case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
					x = RkLoadFloat(m);
					if (floor(x) != x)
						RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
					goto	inexact;
				default:
					break;
				}
			break;
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

static int
gcd_int(rk_object num1, rk_object num2)
{
	unsigned gr;

	RkLoadInteger(num1, 0);
	RkLoadInteger(num2, 1);
	gr = RkComputeGCD(0, 1, 2, 3);
	rk_eval_register[0] = RkStoreInt(gr);
	return	0;
}

	/*ARGSUSED*/
static int
gcd_frac(rk_object num1, rk_object num2)
{
	return	RP_ERROR_ILLEGALARG;
}

static int
gcd_flo(double x, double y)
{
	double z;

	x = fabs(x);
	y = fabs(y);
	if (!RK_FINITE(x) || !RK_FINITE(y))
		return	RK_ERROR_OVERFLOW;
	if (floor(x) != x || floor(y) != y)
		return	RP_ERROR_ILLEGALARG;
	while (y > 0.1) {
		z = floor(x/y);
		z = x - z*y;
		x = y;
		y = z;
	}
	rk_eval_register[0] = RkStoreFloat(x);
	return	0;
}

static int
gcd_comp(void)
{
	return	RP_ERROR_ILLEGALARG;
}

rk_object rp_gcd_proc;
static rk_object
gcd(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 0) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	arith_sub(gcd_int, gcd_frac, gcd_flo, gcd_comp, gcd_comp);
}

static int
lcm_int(rk_object num1, rk_object num2)
{
	unsigned gr;

	if (num1 == RK_MAKEINUM(0) || num2 == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		return	0;
	}
	RkLoadInteger(num1, 0);
	if (RkIntegerIsNegative(0))
		RkNegateInt(0);
	RkCopyInt(0, 4);
	RkLoadInteger(num2, 1);
	if (RkIntegerIsNegative(1))
		RkNegateInt(1);
	RkCopyInt(1, 5);
	gr = RkComputeGCD(0, 1, 2, 3);
	RkDivideIntByInt(4, gr, 3);
	RkMultiplyIntByInt(5, 3, 0);
	rk_eval_register[0] = RkStoreInt(0);
	return	0;
}

static int
lcm_flo(double x, double y)
{
	double x0, y0, z;

	x0 = x = fabs(x);
	y0 = y = fabs(y);
	if (!RK_FINITE(x) || !RK_FINITE(y))
		return	RK_ERROR_OVERFLOW;
	if (floor(x) != x || floor(y) != y)
		return	RP_ERROR_ILLEGALARG;
	if (x == 0 || y == 0) {
		rk_eval_register[0] = RkStoreFloat(0);
		return	0;
	}
	while (y > 0.1) {
		z = floor(x/y);
		z = x - z*y;
		x = y;
		y = z;
	}
	rk_eval_register[0] = RkStoreFloat((x0/x)*y0);
	return	0;
}

rk_object rp_lcm_proc;
static rk_object
lcm(void)
{
	if (RK_GETINUM(rk_eval_register[2]) == 0) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	return	arith_sub(lcm_int, gcd_frac, lcm_flo, gcd_comp, gcd_comp);
}

rk_object rp_num2str_proc;
static rk_object
num2str(void)
{
	rk_object *cp, num;
	int r;
	char *str, *s;
	unsigned len, len0, i;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		r = 10;
		num = rk_eval_register[0];
		break;
	case 2:
		if (!RK_ISINUM(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		r = RK_GETINUM(rk_eval_register[0]);
		num = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISINUM(num)) {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	if (!(str = RkPrintNumber(num, r)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	len = len0 = i = strlen(str);
	if (len == 0 || len >= (1 << 20)) {
		len += 4;
		i = 0;
	}
	if (!(s = malloc(len))) {
		RkScavenge(1);
		if (!(s = malloc(len)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (!i) {
		*(unsigned long *)s = len0;
		s += 4;
	}
	for (i = 0; i < len0; ++i)
		s[i] = str[len0 - i - 1];
	RP_RETURN();
}

rk_object rp_str2num_proc;
static rk_object
str2num(void)
{
	int r, l;
	char *s;
	rk_object str;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		r = 10;
		str = rk_eval_register[0];
		break;
	case 2:
		if (!RK_ISINUM(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		r = RK_GETINUM(rk_eval_register[0]);
		str = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	l = RP_CAR(str) >> 12;
	s = RkGetMallocObject(str);
	if (!l) {
		l = *(unsigned long *)s;
		s += 4;
	}
	rk_eval_register[0] = RkReadNumber(s, l, r);
	RP_RETURN();
}

rk_object rp_realp_proc;
static rk_object
realp(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		default:
			break;
		}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_integerp_proc;
static rk_object
integerp(void)
{
	rk_object obj;
	double x;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (RK_ISINUM(obj)) {
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
		switch (((rk_object *)obj)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(obj);
			if (x == floor(x))
				rk_eval_register[0] = RK_SOBJ_TRUE;
			else
				rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		default:
			break;
		}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

static int
divide_int(rk_object num1, rk_object num2)
{
	if (RK_ISINUM(num2) && RK_GETINUM(num2) == 0)
		return	RK_ERROR_DIVIDEBYZERO;
	RkLoadInteger(num1, 0);
	RkLoadInteger(num2, 1);
	rk_eval_register[0] = RkMakeExactFraction(0, 1);
	return	0;
}

static int
divide_frac(rk_object num1, rk_object num2)
{
	if (RK_ISINUM(num2) && RK_GETINUM(num2) == 0)
		return	RK_ERROR_DIVIDEBYZERO;
	if ((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadInteger(num1, 0);
		RkLoadShortInt(1, 1);
	} else {
		RkLoadInteger(((rk_object *)num1)[2], 0);
		if (RK_GETINUM(((rk_object *)num1)[1]))
			RkNegateInt(0);
		RkLoadInteger(((rk_object *)num1)[3], 1);
	}
	if ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		RkLoadShortInt(1, 2);
		RkLoadInteger(num2, 3);
	} else {
		RkLoadInteger(((rk_object *)num2)[3], 2);
		if (RK_GETINUM(((rk_object *)num2)[1]))
			RkNegateInt(2);
		RkLoadInteger(((rk_object *)num2)[2], 3);
	}
	if ((rk_eval_register[0] = RkMultiplyFraction(0, 1, 2, 3)) == RK_SOBJ_ERROR)
		return	RK_ERROR_OVERFLOW;
	return	0;
}

static int
divide_flo(double x, double y)
{
	if (y == 0.0)
		return	RK_ERROR_DIVIDEBYZERO;
	rk_eval_register[0] = RkStoreFloat(x / y);
	return	0;
}

static int
divide_exact(rk_object num1, rk_object num2)
{
	if (((num1 & 7) || (((rk_object *)num1)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION))
	 && ((num2 & 7) || (((rk_object *)num2)[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FRACTION)))
		return	divide_int(num1, num2);
	else
		return	divide_frac(num1, num2);
}

static int
divide_comp_exact(void)
{
	int err;

	if (err = times_exact(rk_eval_register[4], rk_eval_register[4]))
		return	err;
	rk_eval_register[6] = rk_eval_register[0];
	rk_valid_register = 7;
	if (err = times_exact(rk_eval_register[5], rk_eval_register[5]))
		return	err;
	if (err = add_exact(rk_eval_register[0], rk_eval_register[6]))
		return	err;
	rk_eval_register[6] = rk_eval_register[0];
	if (err = divide_exact(rk_eval_register[4], rk_eval_register[6]))
		return	err;
	rk_eval_register[4] = rk_eval_register[0];
	if (err = divide_exact(rk_eval_register[5], rk_eval_register[6]))
		return	err;
	if (err = subtract_exact(RK_MAKEINUM(0), rk_eval_register[0]))
		return	err;
	rk_eval_register[5] = rk_eval_register[0];
	return	times_comp_exact();
}

static int
divide_comp_inexact(void)
{
	double r1, i1, r2, i2, mag;

	r1 = RkConvertToFloat(rk_eval_register[2]);
	i1 = RkConvertToFloat(rk_eval_register[3]);
	r2 = RkConvertToFloat(rk_eval_register[4]);
	i2 = RkConvertToFloat(rk_eval_register[5]);
	mag = r2*r2 + i2*i2;
	if (mag == 0.0)
		return	RK_ERROR_DIVIDEBYZERO;
	rk_eval_register[2] = RkStoreFloat((r1*r2 + i1*i2) / mag);
	rk_eval_register[0] = RkStoreFloat((-r1*i2 + r2*i1) / mag);
	return	0;
}

rk_object rp_slash_proc;
static rk_object
slash(void)
{
	rk_object *cp, obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	case 1:
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = RK_MAKEINUM(1);
		rk_eval_register[1] = (rk_object)cp;
		break;
	default:
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = (rk_object)cp;
		while (RP_CDR(rk_eval_register[1]) != RK_SOBJ_NIL) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[1]);
			cp[1] = rk_eval_register[0];
			rk_eval_register[0] = (rk_object)cp;
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
		obj = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = rk_eval_register[0];
		rk_eval_register[0] = obj;
		break;
	}
	return	arith_sub(divide_int, divide_frac, divide_flo, divide_comp_exact, divide_comp_inexact);
}

rk_object rp_numerator_proc;
static rk_object
numerator(void)
{
	rk_object num;
	double x;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = num;
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			if (RK_GETINUM(((rk_object *)num)[1])) {
				RkLoadInteger(((rk_object *)num)[2], 0);
				RkNegateInt(0);
				rk_eval_register[0] = RkStoreInt(0);
			} else
				rk_eval_register[0] = ((rk_object *)num)[2];
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			if (!RK_FINITE(x))
				RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
			RkConvertToFraction(x, 0, 1);
			rk_eval_register[0] = RkStoreFloat(RkIntToFloat(0));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_denominator_proc;
static rk_object
denominator(void)
{
	rk_object num;
	double x;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = RK_MAKEINUM(1);
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			rk_eval_register[0] = ((rk_object *)num)[3];
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			if (!RK_FINITE(x))
				RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
			RkConvertToFraction(x, 0, 1);
			rk_eval_register[0] = RkStoreFloat(RkIntToFloat(1));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_floor_proc;
static rk_object
fffloor(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = num;
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			RkLoadInteger(((rk_object *)num)[2], 0);
			RkLoadInteger(((rk_object *)num)[3], 1);
			RkDivideIntByInt(0, 1, 2);
			if (RK_GETINUM(((rk_object *)num)[1])) {
				RkAddIntByUshort(2, 1);
				RkNegateInt(2);
			}
			rk_eval_register[0] = RkStoreInt(2);
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RkStoreFloat(floor(RkLoadFloat(num)));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_ceiling_proc;
static rk_object
ccceiling(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = num;
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			RkLoadInteger(((rk_object *)num)[2], 0);
			RkLoadInteger(((rk_object *)num)[3], 1);
			RkDivideIntByInt(0, 1, 2);
			if (RK_GETINUM(((rk_object *)num)[1]))
				RkNegateInt(2);
			else
				RkAddIntByUshort(2, 1);
			rk_eval_register[0] = RkStoreInt(2);
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RkStoreFloat(-floor(-RkLoadFloat(num)));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_truncate_proc;
static rk_object
tttruncate(void)
{
	rk_object num;
	double x;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = num;
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			RkLoadInteger(((rk_object *)num)[2], 0);
			RkLoadInteger(((rk_object *)num)[3], 1);
			RkDivideIntByInt(0, 1, 2);
			if (RK_GETINUM(((rk_object *)num)[1]))
				RkNegateInt(2);
			rk_eval_register[0] = RkStoreInt(2);
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			rk_eval_register[0] = RkStoreFloat((x >= 0) ? floor(x) : -floor(-x));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

rk_object rp_round_proc;
static rk_object
rrround(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		rk_eval_register[0] = num;
		RP_RETURN();
	}
	if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			if (((rk_object *)num)[3] == RK_MAKEINUM(2)) {
				RkLoadInteger(((rk_object *)num)[2], 0);
				if (RK_ISINUM(((rk_object *)num)[2])
				  ? (((rk_object *)num)[2] & (2<<2))
				  : (((rk_object *)((rk_object *)num)[2])[1] & (2<<2)))
					RkAddIntByUshort(0, 1);
				RkDivideIntByUshort(0, 2);
			} else {
				RkLoadInteger(((rk_object *)num)[2], 0);
				RkLoadInteger(((rk_object *)num)[3], 1);
				RkCopyInt(1, 2);
				RkDivideIntByUshort(2, 2);
				RkAddIntByInt(0, 2, 3);
				RkDivideIntByInt(3, 1, 0);
			}
			if (RK_GETINUM(((rk_object *)num)[1]))
				RkNegateInt(0);
			rk_eval_register[0] = RkStoreInt(0);
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RkStoreFloat(rint(RkLoadFloat(num)));
			RP_RETURN();
		default:
			break;
		}
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

static rk_object
call_3m_function(double (*fun)(double), rk_object num)
{
	double x;

	if (RK_ISINUM(num))
		x = (double)RK_GETINUM(num);
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			x = RkIntToFloat(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			x = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
			if (RK_GETINUM(((rk_object *)num)[1]))
				x = -x;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RkStoreFloat((*fun)(x));
	RP_RETURN();
}

rk_object rp_exp_proc;
static rk_object
eeexp(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	return	call_3m_function(exp, num);
}

rk_object rp_log_proc;
static rk_object
lllog(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(1)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(log, num);
}

rk_object rp_sin_proc;
static rk_object
sssin(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(sin, num);
}

rk_object rp_cos_proc;
static rk_object
cccos(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	return	call_3m_function(cos, num);
}

rk_object rp_tan_proc;
static rk_object
tttan(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(tan, num);
}

rk_object rp_asin_proc;
static rk_object
aaasin(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(asin, num);
}

rk_object rp_acos_proc;
static rk_object
aaacos(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(1)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(acos, num);
}

rk_object rp_atan_proc;
static rk_object
aaatan(void)
{
	rk_object num;
	double x, y;

	RP_ASSERTARG(2);
	num = RP_CAR(rk_eval_register[1]);
	if (num == RK_MAKEINUM(0)) {
		num = rk_eval_register[0];
		if (RK_ISINUM(num) && RK_GETINUM(num) >= 0
		 || (!(num & 7)
		  && ((((rk_object *)num)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS)
		   || ((((rk_object *)num)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_FRACTION)
		    && !RK_GETINUM(((rk_object *)num)[1]))))) {
			rk_eval_register[0] = RK_MAKEINUM(0);
			RP_RETURN();
		}
		num = RP_CAR(rk_eval_register[1]);
	}
	if (RK_ISINUM(num))
		y = (double)RK_GETINUM(num);
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			y = RkIntToFloat(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			y = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
			if (RK_GETINUM(((rk_object *)num)[1]))
				y = -y;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			y = RkLoadFloat(num);
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		x = (double)RK_GETINUM(num);
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			x = RkIntToFloat(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			x = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
			if (RK_GETINUM(((rk_object *)num)[1]))
				x = -x;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RkStoreFloat(atan2(y, x));
	RP_RETURN();
}

rk_object rp_atan_proc;
static rk_object
sssqrt(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	return	call_3m_function(sqrt, num);
}

rk_object rp_expt_proc;
static rk_object
eeexpt(void)
{
	rk_object num;
	double x, y;

	RP_ASSERTARG(2);
	num = RP_CAR(rk_eval_register[1]);
	if (RK_ISINUM(num))
		y = (double)RK_GETINUM(num);
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			y = RkIntToFloat(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			y = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
			if (RK_GETINUM(((rk_object *)num)[1]))
				y = -y;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			y = RkLoadFloat(num);
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		x = (double)RK_GETINUM(num);
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			RkLoadBigInt(num, 0);
			x = RkIntToFloat(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			x = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
			if (RK_GETINUM(((rk_object *)num)[1]))
				x = -x;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RkStoreFloat(pow(y, x));
	RP_RETURN();
}

rk_object rp_makerect_proc;
static rk_object
makerect(void)
{
	rk_object *cp, num;
	int rexact, iexact;
	double re, im;

	RP_ASSERTARG(2);
	num = RP_CAR(rk_eval_register[1]);
	if (RK_ISINUM(num))
		rexact = 1;
	else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			rexact = 1;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rexact = 0;
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		if (num == RK_MAKEINUM(0)) {
			rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
			RP_RETURN();
		}
		iexact = 1;
	} else if (!(num & 7) && (((rk_object *)num)[0] & 0xf) == 7)
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			iexact = 1;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			iexact = 0;
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (rexact && iexact) {
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
		cp[1] = RP_CAR(rk_eval_register[1]);
		cp[2] = rk_eval_register[0];
		cp[3] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		RP_RETURN();
	}
	re = RkConvertToFloat(RP_CAR(rk_eval_register[1]));
	im = RkConvertToFloat(rk_eval_register[0]);
	if (im == 0.0) {
		rk_eval_register[0] = RkStoreFloat(re);
		RP_RETURN();
	}
	rk_eval_register[0] = RkStoreFloat(re);
	rk_eval_register[1] = RkStoreFloat(im);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
	cp[1] = rk_eval_register[0];
	cp[2] = rk_eval_register[1];
	cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_makepole_proc;
static rk_object
makepole(void)
{
	rk_object *cp, num;
	double m, a, re, im;

	RP_ASSERTARG(2);
	num = RP_CAR(rk_eval_register[1]);
	if (RK_ISINUM(num)) {
		if (num == RK_MAKEINUM(0)) {
			rk_eval_register[0] = RK_MAKEINUM(0);
			RP_RETURN();
		}
	} else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	num = rk_eval_register[0];
	if (RK_ISINUM(num)) {
		if (num == RK_MAKEINUM(0)) {
			rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
			RP_RETURN();
		}
	} else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	m = RkConvertToFloat(RP_CAR(rk_eval_register[1]));
	a = RkConvertToFloat(rk_eval_register[0]);
	re = m * cos(a);
	im = m * sin(a);
	if (im == 0.0) {
		rk_eval_register[0] = RkStoreFloat(re);
		RP_RETURN();
	}
	rk_eval_register[0] = RkStoreFloat(re);
	rk_eval_register[1] = RkStoreFloat(im);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
	cp[1] = rk_eval_register[0];
	cp[2] = rk_eval_register[1];
	cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_realpart_proc;
static rk_object
realpart(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		rk_eval_register[0] = num;
	else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = num;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			rk_eval_register[0] = ((rk_object *)num)[1];
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	RP_RETURN();
}

rk_object rp_imagpart_proc;
static rk_object
imagpart(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		rk_eval_register[0] = RK_MAKEINUM(0);
	else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			rk_eval_register[0] = RK_MAKEINUM(0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = RkStoreFloat(0.0);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			rk_eval_register[0] = ((rk_object *)num)[2];
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	RP_RETURN();
}

rk_object rp_exact2inexact_proc;
static rk_object
exact2inexact(void)
{
	rk_object *cp, num;
	double x, y;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		x = (double)RK_GETINUM(num);
	else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			x = RkConvertToFloat(num);
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			rk_eval_register[0] = num;
			RP_RETURN();
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			cp = (rk_object *)num;
			if (!RK_ISINUM(cp[1])
			 && (((rk_object *)cp[1])[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_FLONUM)) {
				rk_eval_register[0] = num;
				RP_RETURN();
			}
			x = RkConvertToFloat(cp[1]);
			y = RkConvertToFloat(cp[2]);
			if (y == 0.0) {
				rk_eval_register[0] = RkStoreFloat(x);
				RP_RETURN();
			}
			rk_eval_register[0] = RkStoreFloat(x);
			rk_eval_register[1] = RkStoreFloat(y);
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
			cp[1] = rk_eval_register[0];
			cp[2] = rk_eval_register[1];
			cp[3] = RK_DUMMY_OBJ;
			rk_eval_register[0] = (rk_object)cp;
			RP_RETURN();
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	rk_eval_register[0] = RkStoreFloat(x);
	RP_RETURN();
}

rk_object rp_inexact2exact_proc;
static rk_object
inexact2exact(void)
{
	rk_object *cp, num;
	int xsig, ysig;
	double x, y;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (RK_ISINUM(num))
		rk_eval_register[0] = num;
	else {
		if ((num & 7) || (((rk_object *)num)[0] & 0xf) != 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)num)[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			rk_eval_register[0] = num;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			x = RkLoadFloat(num);
			if (!RK_FINITE(x))
				RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
			if (xsig = (x < 0.0))
				x = -x;
			RkConvertToFraction(x, 0, 1);
			rk_eval_register[1] = RkStoreInt(1);
			if (rk_eval_register[1] == RK_MAKEINUM(1)) {
				if (xsig)
					RkNegateInt(0);
				rk_eval_register[0] = RkStoreInt(0);
				break;
			}
			rk_eval_register[0] = RkStoreInt(0);
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
			cp[1] = RK_MAKEINUM(xsig);
			cp[2] = rk_eval_register[0];
			cp[3] = rk_eval_register[1];
			rk_eval_register[0] = (rk_object)cp;
			break;
		case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
			cp = (rk_object *)num;
			if (RK_ISINUM(cp[1])
			 || (((rk_object *)cp[1])[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_FLONUM)) {
				rk_eval_register[0] = num;
				break;
			}
			x = RkLoadFloat(cp[1]);
			y = RkLoadFloat(cp[2]);
			if (!RK_FINITE(x) || !RK_FINITE(y))
				RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
			if (xsig = (x < 0.0))
				x = -x;
			if (ysig = (y < 0.0))
				y = -y;
			RkConvertToFraction(x, 0, 1);
			rk_eval_register[1] = RkStoreInt(1);
			if (rk_eval_register[1] == RK_MAKEINUM(1)) {
				if (xsig)
					RkNegateInt(0);
				rk_eval_register[0] = RkStoreInt(0);
			} else {
				rk_eval_register[0] = RkStoreInt(0);
				cp = RkAllocCells(4);
				cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
				cp[1] = RK_MAKEINUM(xsig);
				cp[2] = rk_eval_register[0];
				cp[3] = rk_eval_register[1];
				rk_eval_register[0] = (rk_object)cp;
			}
			RkConvertToFraction(y, 0, 1);
			rk_eval_register[2] = RkStoreInt(1);
			if (rk_eval_register[2] == RK_MAKEINUM(1)) {
				if (ysig)
					RkNegateInt(0);
				rk_eval_register[1] = RkStoreInt(0);
			} else {
				rk_eval_register[1] = RkStoreInt(0);
				cp = RkAllocCells(4);
				cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
				cp[1] = RK_MAKEINUM(ysig);
				cp[2] = rk_eval_register[1];
				cp[3] = rk_eval_register[2];
				rk_eval_register[1] = (rk_object)cp;
			}
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
			cp[1] = rk_eval_register[0];
			cp[2] = rk_eval_register[1];
			cp[3] = RK_DUMMY_OBJ;
			rk_eval_register[0] = (rk_object)cp;
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	RP_RETURN();
}

rk_object rp_sinh_proc;
static rk_object
sssinh(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(0);
		RP_RETURN();
	}
	return	call_3m_function(sinh, num);
}

rk_object rp_cosh_proc;
static rk_object
cccosh(void)
{
	rk_object num;

	RP_ASSERTARG(1);
	num = rk_eval_register[0];
	if (num == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_MAKEINUM(1);
		RP_RETURN();
	}
	return	call_3m_function(cosh, num);
}

static rk_object
bitop_sub(int (*op)(unsigned, unsigned, unsigned), int unit)
{
	rk_object obj;

	if (RK_GETINUM(rk_eval_register[2]) == 0) {
		rk_eval_register[0] = RK_MAKEINUM(unit);
		RP_RETURN();
	}
	if (!RK_ISINUM(rk_eval_register[0])) {
		if (rk_eval_register[0] & 7)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (((rk_object *)rk_eval_register[0])[0] & 0xfff) {
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
	}
	if (RK_GETINUM(rk_eval_register[2]) == 1)
		RP_RETURN();
	RkLoadInteger(rk_eval_register[0], 0);
	for (; ; ) {
		obj = RP_CAR(rk_eval_register[1]);
		if (!RK_ISINUM(obj)) {
			if (obj & 7)
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
				break;
			default:
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
		}
		RkLoadInteger(obj, 1);
		if (!op(0, 1, 2))
			RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		if (rk_eval_register[1] == RK_SOBJ_NIL) {
			rk_eval_register[0] = RkStoreInt(2);
			RP_RETURN();
		}
		RkCopyInt(2, 0);
	}
}

rk_object rp_bitand_proc;
static rk_object
bitand(void)
{
	return	bitop_sub(RkBitwiseAnd, -1);
}

rk_object rp_bitor_proc;
static rk_object
bitor(void)
{
	return	bitop_sub(RkBitwiseOr, 0);
}

rk_object rp_bitxor_proc;
static rk_object
bitxor(void)
{
	return	bitop_sub(RkBitwiseXor, 0);
}

static rk_object
fpc_sub(int (*pred)(rk_object))
{
	int tcc;
	rk_object obj;

	RP_ASSERTARG(1);
	for (tcc = 1; tcc-- > 0; ) {
		obj = rk_eval_register[tcc];
		if (RK_ISINUM(obj)) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (!(obj & 7) && (((rk_object *)obj)[0] & 0xf) == 7)
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
				rk_eval_register[0] = RK_SOBJ_FALSE;
				RP_RETURN();
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
				if (pred(obj)) {
					rk_eval_register[0] = RK_SOBJ_TRUE;
					RP_RETURN();
				}
				break;
			case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
				rk_eval_register[0] = ((rk_object *)obj)[1];
				rk_eval_register[1] = ((rk_object *)obj)[2];
				tcc = 2;
				break;
			default:
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			}
		else
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

static int
fpc_pred_isnan(rk_object obj)
{
	return	RK_ISNAN(RkLoadFloat(obj));
}

static int
fpc_pred_isinf(rk_object obj)
{
	return	!RK_FINITE(RkLoadFloat(obj));
}

rk_object rp_isnan_proc;
static rk_object
fpc_isnan(void)
{
	return	fpc_sub(fpc_pred_isnan);
}

rk_object rp_isinf_proc;
static rk_object
fpc_isinf(void)
{
	return	fpc_sub(fpc_pred_isinf);
}

int
RpInitializeNumeric(int index)
{
	if (index != -1) {
#ifdef __BORLANDC__
		_control87(MCW_EM, MCW_EM);
#endif
		RP_DEFINESUBR("=", rp_equal_proc, index + 0, equal);
		RP_DEFINESUBR("+", rp_add_proc, index + 1, add);
		RP_DEFINESUBR("number?", rp_numberp_proc, index + 2, numberp);
		RP_DEFINESUBR("exact?", rp_exactp_proc, index + 3, exactp);
		RP_DEFINESUBR("inexact?", rp_inexactp_proc, index + 4, inexactp);
		RP_DEFINESUBR("<", rp_lt_proc, index + 5, lt);
		RP_DEFINESUBR(">", rp_gt_proc, index + 6, gt);
		RP_DEFINESUBR("<=", rp_le_proc, index + 7, le);
		RP_DEFINESUBR(">=", rp_ge_proc, index + 8, ge);
		RP_DEFINESUBR("zero?", rp_zerop_proc, index + 9, zerop);
		RP_DEFINESUBR("positive?", rp_positivep_proc, index + 10, positivep);
		RP_DEFINESUBR("negative?", rp_negativep_proc, index + 11, negativep);
		RP_DEFINESUBR("odd?", rp_oddp_proc, index + 12, oddp);
		RP_DEFINESUBR("even?", rp_evenp_proc, index + 13, evenp);
		RP_DEFINESUBR("max", rp_max_proc, index + 14, mmaaxx);
		RP_DEFINESUBR("min", rp_min_proc, index + 15, mmiinn);
		RP_DEFINESUBR("*", rp_times_proc, index + 16, times);
		RP_DEFINESUBR("-", rp_minus_proc, index + 17, minus);
		RP_DEFINESUBR("abs", rp_abs_proc, index + 18, aabbss);
		RP_DEFINESUBR("quotient", rp_quotient_proc, index + 19, quotient);
		RP_DEFINESUBR("remainder", rp_remainder_proc, index + 20, rpremainder);
		RP_DEFINESUBR("modulo", rp_modulo_proc, index + 21, modulo);
		RP_DEFINESUBR("gcd", rp_gcd_proc, index + 22, gcd);
		RP_DEFINESUBR("lcm", rp_lcm_proc, index + 23, lcm);
		RP_DEFINESUBR("number->string", rp_num2str_proc, index + 24, num2str);
		RP_DEFINESUBR("string->number", rp_str2num_proc, index + 25, str2num);
		RP_DEFINESUBR("real?", rp_realp_proc, index + 26, realp);
		RP_DEFINESUBR("integer?", rp_integerp_proc, index + 27, integerp);
		RP_DEFINESUBR("/", rp_slash_proc, index + 28, slash);
		RP_DEFINESUBR("numerator", rp_numerator_proc, index + 29, numerator);
		RP_DEFINESUBR("denominator", rp_denominator_proc, index + 30, denominator);
		RP_DEFINESUBR("floor", rp_floor_proc, index + 31, fffloor);
		RP_DEFINESUBR("ceiling", rp_ceiling_proc, index + 32, ccceiling);
		RP_DEFINESUBR("truncate", rp_truncate_proc, index + 33, tttruncate);
		RP_DEFINESUBR("round", rp_round_proc, index + 34, rrround);
		RP_DEFINESUBR("exp", rp_exp_proc, index + 35, eeexp);
		RP_DEFINESUBR("log", rp_log_proc, index + 36, lllog);
		RP_DEFINESUBR("sin", rp_sin_proc, index + 37, sssin);
		RP_DEFINESUBR("cos", rp_cos_proc, index + 38, cccos);
		RP_DEFINESUBR("tan", rp_tan_proc, index + 39, tttan);
		RP_DEFINESUBR("asin", rp_asin_proc, index + 40, aaasin);
		RP_DEFINESUBR("acos", rp_acos_proc, index + 41, aaacos);
		RP_DEFINESUBR("atan", rp_atan_proc, index + 42, aaatan);
		RP_DEFINESUBR("sqrt", rp_atan_proc, index + 43, sssqrt);
		RP_DEFINESUBR("expt", rp_expt_proc, index + 44, eeexpt);
		RP_DEFINESUBR("make-rectangular", rp_makerect_proc, index + 45, makerect);
		RP_DEFINESUBR("make-polar", rp_makepole_proc, index + 46, makepole);
		RP_DEFINESUBR("real-part", rp_realpart_proc, index + 47, realpart);
		RP_DEFINESUBR("imag-part", rp_imagpart_proc, index + 48, imagpart);
		RP_DEFINESUBR("exact->inexact", rp_exact2inexact_proc, index + 49, exact2inexact);
		RP_DEFINESUBR("inexact->exact", rp_inexact2exact_proc, index + 50, inexact2exact);
		RP_DEFINESUBR("rp:sinh", rp_sinh_proc, index + 51, sssinh);
		RP_DEFINESUBR("rp:cosh", rp_cosh_proc, index + 52, cccosh);
		RP_DEFINESUBR("rp:bitwise-and", rp_bitand_proc, index + 53, bitand);
		RP_DEFINESUBR("rp:bitwise-or", rp_bitor_proc, index + 54, bitor);
		RP_DEFINESUBR("rp:bitwise-xor", rp_bitxor_proc, index + 55, bitxor);
		RP_DEFINESUBR("rp:not-a-number?", rp_isnan_proc, index + 56, fpc_isnan);
		RP_DEFINESUBR("rp:infinite?", rp_isinf_proc, index + 57, fpc_isinf);
		rk_valid_register = 0;
	}
	return	58;
}
