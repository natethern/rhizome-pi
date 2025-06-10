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
static char rcsid[] = "@(#)$Id: eval.c,v 1.6 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: eval.c,v $
 * Revision 1.6  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.5  1999/02/15 08:47:38  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.4  1997/05/12 07:21:16  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1997/04/26 13:29:05  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.2  1996/09/06 06:11:24  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:47  qfwfq
 * Initial revision
 *
 */

/*
 * Evaluation of lisp expression.
 */
#include "rhiz_pi.h"

rk_object rp_default_eval_car_proc, rp_eval_car_proc, rp_evlis_proc, rp_noapply_proc;

static rk_object eval_conti1_proc;
static rk_object evlis_conti1_proc, evlis_conti2_proc;

static RK_INLINE rk_object
getvloc(unsigned long frameno, unsigned long vno)
{
	rk_object **env;

	env = (rk_object **)rk_eval_register[0];
	while (frameno--)
		env = (rk_object **)env[3];
	return	env[1][vno];
}

static rk_object
eval_car(void)
{
	rk_object obj, **env, *cp;
	unsigned long fn, vn, flen;

	obj = RP_CAR(rk_eval_register[1]);
	if (RK_ISCELL(obj)) {
		if (obj & 1) {
			if ((rk_eval_register[0] = ((rk_object *)(obj&~1))[1]) == RK_SOBJ_UNBOUND)
				RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, obj&~1);
			RP_RETURN();
		}
		switch (((rk_object *)obj)[0] & 0xf) {
		default:
			if (((obj = ((rk_object *)obj)[0]) & 0xff) == 0x2c) {
				cp = (rk_object *)getvloc(RP_GETFRAME0(obj), RP_GETVNUM0(obj));
				if ((unsigned long)cp & 7 || (cp[0] & 7) != 3)
					RK_SIGNAL_ERROR(RP_ERROR_ILLEGALAPPLY, (rk_object)cp);
				obj = cp[1];
				rk_eval_register[2] = cp[0]&~7;
				rk_valid_register = 3;
				return	obj;
			}
			cp = RkAllocCells(6);
			cp[0] = RK_VECTOR_TAG(6, 0);
			cp[1] = eval_conti1_proc;
			cp[2] = rk_eval_register[0];
			cp[3] = rk_eval_register[1];
			cp[4] = (rk_object)rk_continuation;
			cp[5] = RK_DUMMY_OBJ;
			rk_continuation = cp;
			rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
			return	rp_eval_car_proc;
		case 1: case 9:
			cp = (rk_object *)((rk_object *)(((rk_object *)obj)[0]&~1))[1];
			if ((unsigned long)cp & 7 || (cp[0] & 7) != 3) {
				if ((rk_object)cp == RK_SOBJ_UNBOUND)
					RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, ((rk_object *)obj)[0]&~1);
				else
					RK_SIGNAL_ERROR(RP_ERROR_ILLEGALAPPLY, (rk_object)cp);
			}
			obj = cp[1];
			rk_eval_register[2] = cp[0]&~7;
			rk_valid_register = 3;
			return	obj;
		case 15:
			if ((((rk_object *)obj)[0] & 0xfff) == RK_MALLOC_TAG(0, RK_TCODE_STRING))
				rk_eval_register[0] = obj;
			else
				RK_SIGNAL_ERROR(RP_ERROR_ILLEGALEVAL, obj);
			RP_RETURN();
		case 3: case 11:
			RK_SIGNAL_ERROR(RP_ERROR_ILLEGALEVAL, obj);
		case 7:
			switch (((rk_object *)obj)[0] & 0xfff) {
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
			case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
			case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
			case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
			case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
				rk_eval_register[0] = obj;
				break;
			case RK_VECTOR_TAG(0, RP_TCODE_VLOC1):
				rk_eval_register[0] = getvloc(RP_GETFRAME1(obj), RP_GETVNUM1(obj));
				break;
			default:
				RK_SIGNAL_ERROR(RP_ERROR_ILLEGALEVAL, obj);
			}
			RP_RETURN();
		case 5: case 13:
			env = (rk_object **)rk_eval_register[0];
			fn = 0;
			while (RK_ISCELL((rk_object)env)) {
				cp = env[2];
				flen = cp[0] >> 12;
				for (vn = 1; vn < flen; ++vn)
					if (cp[vn] == obj) {
						rk_eval_register[0] = env[1][vn];
						if (vn >= (1<<16) || fn >= (1<<8)) {
							cp = RkAllocCells(4);
							cp[0] = RK_VECTOR_TAG(4, RP_TCODE_VLOC1);
							cp[1] = RK_MAKEINUM(fn);
							cp[2] = RK_MAKEINUM(vn);
							cp[3] = RK_DUMMY_OBJ;
							RkWriteCell(&RP_CAR(rk_eval_register[1]), (rk_object)cp);
						} else
							RP_CAR(rk_eval_register[1]) = RP_MAKEVLOC0(fn, vn);
						RP_RETURN();
					}
				env = (rk_object **)env[3];
				++fn;
			}
			if ((rk_object)env != RK_DUMMY_OBJ)
				RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, obj);
			((rk_object *)rk_eval_register[1])[0] |= 1;
			if ((rk_eval_register[0] = ((rk_object *)obj)[1]) == RK_SOBJ_UNBOUND)
				RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, obj);
			RP_RETURN();
		}
	}
	if ((obj & 2) || (obj & 0xff) == 0x1c || obj == RK_SOBJ_FALSE || obj == RK_SOBJ_TRUE)
		rk_eval_register[0] = obj;
	else if ((obj & 0xff) == 0x2c)
		rk_eval_register[0] = getvloc(RP_GETFRAME0(obj), RP_GETVNUM0(obj));
	else
		RK_SIGNAL_ERROR(RP_ERROR_ILLEGALEVAL, obj);
	RP_RETURN();
}

static rk_object
eval_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] & 7 || ((cp = (rk_object *)rk_eval_register[0])[0] & 7) != 3)
		RK_SIGNAL_ERROR(RP_ERROR_ILLEGALAPPLY, rk_eval_register[0]);
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = rk_continuation[3];
	rk_eval_register[2] = cp[0] & ~7;
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[4];
	return	cp[1];
}

static rk_object
evlis(void)
{
	rk_object *cp, obj;

	if ((obj = RP_CDR(RP_CAR(rk_eval_register[1]))) == RK_SOBJ_NIL) {
		obj = rk_eval_register[2];
		rk_eval_register[0] = RK_DUMMY_OBJ;
		rk_eval_register[1] = RK_DUMMY_OBJ;
		rk_eval_register[2] = RK_MAKEINUM(0);
		rk_eval_register[3] = RP_CDR(obj);
		rk_valid_register = 4;
		return	RP_CAR(obj);
	}
	if (!RK_ISCELL(obj) || (((rk_object *)obj)[0] & 1) && (((rk_object *)obj)[0] & 7) != 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALEVAL);
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = evlis_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = RP_CDR(RP_CDR(RP_CAR(rk_eval_register[1])));
	cp[4] = RK_MAKEINUM(1);
	cp[5] = RK_SOBJ_NIL;
	cp[6] = rk_eval_register[2];
	cp[7] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
evlis_conti1(void)
{
	rk_object *cp, obj;

	if ((obj = rk_continuation[3]) == RK_SOBJ_NIL) {
		rk_eval_register[1] = rk_continuation[5];
		rk_eval_register[2] = rk_continuation[4];
		rk_eval_register[3] = RP_CDR(rk_continuation[6]);
		rk_valid_register = 4;
		obj = RP_CAR(rk_continuation[6]);
		rk_continuation = (rk_object *)rk_continuation[7];
		return	obj;
	}
	if (!RK_ISCELL(obj) || (((rk_object *)obj)[0] & 1) && (((rk_object *)obj)[0] & 7) != 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALEVAL);
	cp = RkAllocCells(10);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = evlis_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = RP_CDR(rk_continuation[3]);
	cp[4] = rk_continuation[4] + (1<<2);
	cp[5] = (rk_object)&cp[8];
	cp[6] = rk_continuation[6];
	cp[7] = rk_continuation[7];
	cp[8] = rk_eval_register[0];
	cp[9] = rk_continuation[5];
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = rk_continuation[3];
	rk_valid_register = 2;
	rk_continuation = cp;
	return	rp_eval_car_proc;
}

static rk_object
noapply(void)
{
	RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
}

int
RpInitializeEval(int index)
{
	if (index != -1) {
		rp_default_eval_car_proc = rp_eval_car_proc = RkRegisterProcedure(index + 0, eval_car);
		eval_conti1_proc = RkRegisterProcedure(index + 1, eval_conti1);
		rp_evlis_proc = RkRegisterProcedure(index + 2, evlis);
		evlis_conti1_proc = RkRegisterProcedure(index + 3, evlis_conti1);
		rp_noapply_proc = RkRegisterProcedure(index + 4, noapply);
	}
	return	5;
}
