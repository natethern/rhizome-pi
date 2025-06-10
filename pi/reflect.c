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
static char rcsid[] = "@(#)$Id: reflect.c,v 1.5 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: reflect.c,v $
 * Revision 1.5  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.4  1999/02/15 08:47:39  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.3  1997/05/12 07:21:21  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.2  1996/09/06 06:11:30  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:51  qfwfq
 * Initial revision
 *
 */

/*
 * Programmable evaluator.
 */
#include "rhiz_pi.h"

rk_object rp_reflect_eval_proc;
rk_object rp_eval_fun;

static rk_object debugcont_proc, hookeval_conti1_proc, reflect_apply_proc;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	(*scan_fun)(&rp_eval_fun, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

static rk_object reflect_eval_conti1_proc;
static rk_object
reflect_eval(void)
{
	rk_object *cp, obj;

	if (rk_continuation[1] == hookeval_conti1_proc) {
		cp = RkAllocCells(6);
		obj = rk_continuation[2];
	} else {
		cp = RkAllocCells(16);
		cp[0] = (rk_object)&cp[2] | 3;
		cp[1] = rp_evlis_proc;
		cp[2] = debugcont_proc;
		cp[3] = (rk_object)&cp[4];
		cp[4] = RK_VECTOR_TAG(6, 0);
		cp[5] = (rk_object)rp_dynamic_extent;
		cp[6] = (rk_object)rk_continuation;
		cp[7] = (rk_object)rk_error_catcher;
		cp[8] = rp_eval_fun;
		cp[9] = RK_DUMMY_OBJ;
		obj = (rk_object)cp;
		cp = &cp[10];
	}
	cp[0] = rk_eval_register[0];
	cp[1] = (rk_object)&cp[2];
	cp[2] = rk_eval_register[1];
	cp[3] = RK_SOBJ_NIL;
	cp[4] = RK_VECTOR_TAG(2, 0);
	cp[5] = reflect_eval_conti1_proc;
	rk_continuation = &cp[4];
	rk_eval_register[0] = obj;
	rk_eval_register[1] = (rk_object)cp;
	rk_eval_register[2] = RK_MAKEINUM(3);
	rk_eval_register[3] = RP_CDR(RP_CAR(rp_eval_fun) & ~7);
	rk_valid_register = 4;
	obj = RP_CAR(RP_CAR(rp_eval_fun) & ~7);
	rp_eval_car_proc = rp_default_eval_car_proc;
	rp_eval_fun = RK_SOBJ_FALSE;
	return	obj;
}

static rk_object
reflect_eval_conti1(void)
{
	RK_SIGNAL_ERROR1(RP_ERROR_EVALRET);
}

static rk_object
debugcont(void)
{
	rk_valid_register = 3;
	return	RpWindTo(((rk_object *)rk_eval_register[3])[1], ((rk_object *)rk_eval_register[3])[2]
		       , ((rk_object *)rk_eval_register[3])[3], ((rk_object *)rk_eval_register[3])[4]);
}

rk_object rp_hookeval_proc;
static rk_object
hookeval(void)
{
	rk_object *cp, obj;
	unsigned tag;

	RP_ASSERTARG(4);
	if (rk_eval_register[0] & 7 || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[2] = rk_eval_register[0];
	obj = rk_eval_register[1];
	rk_eval_register[0] = RP_CAR(obj);
	if (rk_eval_register[0] != RK_DUMMY_OBJ
	 && ((rk_eval_register[0] & 7) || ((rk_object *)rk_eval_register[0])[0] != RK_VECTOR_TAG(4, 0)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CDR(obj);
	rk_eval_register[1] = RP_CAR(obj);
	if ((rk_eval_register[1] & 7) || ((tag = ((((rk_object *)rk_eval_register[1])[0]) & 7)) & 1) && tag != 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CAR(RP_CDR(obj));
	if (obj & 7 || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rp_eval_fun = obj;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = hookeval_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_valid_register = 2;
	rp_eval_car_proc = rp_reflect_eval_proc;
	return	rp_default_eval_car_proc;
}

static rk_object
hookeval_conti1(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(2);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = reflect_eval_conti1_proc;
	if (rk_valid_register != 3) {
		rk_eval_register[1] = RK_SOBJ_NIL;
		rk_eval_register[2] = RK_MAKEINUM(1);
	}
	rk_eval_register[3] = RP_CDR(RP_CAR(rk_continuation[2]) & ~7);
	rk_valid_register = 4;
	proc = RP_CAR(RP_CAR(rk_continuation[2]) & ~7);
	rk_continuation = cp;
	rp_eval_car_proc = rp_default_eval_car_proc;
	rp_eval_fun = RK_SOBJ_FALSE;
	return	proc;
}

rk_object rp_topenv_proc;
static rk_object
topenv(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = RK_DUMMY_OBJ;
	RP_RETURN();
}

static rk_object
getvloc(unsigned long frameno, unsigned long vno, rk_object **env)
{
	while (frameno--)
		env = (rk_object **)env[3];
	return	env[2][vno];
}

rk_object rp_exp2data_proc;
static rk_object
exp2data(void)
{
	rk_object *cp, obj, **env;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	env = (rk_object **)rk_eval_register[0];
	if ((rk_object)env != RK_DUMMY_OBJ && (((rk_object)env & 7) || ((rk_object *)env)[0] != RK_VECTOR_TAG(4, 0)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((obj & 0xff) == 0x2c)
		rk_eval_register[0] = getvloc(RP_GETFRAME0(obj), RP_GETVNUM0(obj), env);
	else if (!(obj & 7))
		switch (RP_CAR(obj) & 0xf) {
		case 1: case 9:
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(RP_CAR(rk_eval_register[1])) & ~7;
			cp[1] = RP_CDR(RP_CAR(rk_eval_register[1]));
			rk_eval_register[0] = (rk_object)cp;
			break;
		case 7:
			if ((RP_CAR(obj) & 0xfff) == RK_VECTOR_TAG(0, RP_TCODE_VLOC1)) {
				rk_eval_register[0] = getvloc(RP_GETFRAME1(obj), RP_GETVNUM1(obj), env);
				break;
			}
			/*FALLTHRU*/
		default:
			rk_eval_register[0] = obj;
			break;
		}
	else
		rk_eval_register[0] = obj;
	RP_RETURN();
}

rk_object rp_calleval_proc;
static rk_object
calleval(void)
{
	unsigned tag;

	RP_ASSERTARG(2);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[1] & 7) || ((tag = ((((rk_object *)rk_eval_register[1])[0]) & 7)) & 1) && tag != 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (rk_eval_register[0] != RK_DUMMY_OBJ
	 && ((rk_eval_register[0] & 7) || ((rk_object *)rk_eval_register[0])[0] != RK_VECTOR_TAG(4, 0)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object applhook_conti1_proc;
rk_object rp_applhook_proc;
static rk_object
applhook(void)
{
	int i;
	rk_object *cp, func, obj;

	RP_ASSERTARG(3);
	rk_eval_register[2] = RP_CAR(RP_CDR(rk_eval_register[1]));
	if (rk_eval_register[2] != RK_SOBJ_FALSE
	 && (rk_eval_register[2] & 7 || (RP_CAR(rk_eval_register[2]) & 7) != 3))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CAR(rk_eval_register[1]);
	if (obj & 7 || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[3] = RP_CAR(obj) & ~7;
	if (RP_ISCONS(rk_eval_register[0])) {
		rk_eval_register[1] = RK_SOBJ_NIL;
		i = 1;
		while (RP_ISCONS(RP_CDR(rk_eval_register[0]))) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[0]);
			cp[1] = rk_eval_register[1];
			rk_eval_register[1] = (rk_object)cp;
			rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
			++i;
		}
		if (RP_CDR(rk_eval_register[0]) != RK_SOBJ_NIL)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		rk_eval_register[0] = RP_CAR(rk_eval_register[0]);
	} else {
		if (rk_eval_register[0] != RK_SOBJ_NIL)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
		i = 0;
	}
	func = rk_eval_register[2];
	obj = rk_eval_register[3];
	rk_eval_register[2] = RK_MAKEINUM(i);
	rk_eval_register[3] = RP_CDR(obj);
	if (func == RK_SOBJ_FALSE)
		return	RP_CAR(obj);
	rp_eval_fun = func;
	rp_eval_car_proc = rp_reflect_eval_proc;
	func = RP_CAR(obj);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = applhook_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	func;
}

static rk_object
applhook_conti1(void)
{
	rp_eval_car_proc = rp_default_eval_car_proc;
	rp_eval_fun = RK_SOBJ_FALSE;
	rk_continuation = (rk_object *)rk_continuation[2];
	RK_PROCEED();
}

rk_object rp_hookappl_proc;
static rk_object
hookappl(void)
{
	rk_object *cp, obj;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	if (obj & 7 || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[1] = RP_CAR(obj) & ~7;
	if (rk_eval_register[0] & 7 || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(6);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = RP_CDR(rk_eval_register[0]);
	cp[2] = reflect_apply_proc;
	cp[3] = (rk_object)&cp[4];
	cp[4] = rk_eval_register[1];
	cp[5] = rk_eval_register[0];
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_unhookappl_proc;
static rk_object
unhookappl(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (obj & 7 || (RP_CAR(obj) & 7) != 3 || RP_CAR(RP_CAR(obj) & ~7) != reflect_apply_proc)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CDR(RP_CDR(RP_CAR(obj) & ~7));
	RP_RETURN();
}

static rk_object
reflect_apply(void)
{
	rk_object *cp, proc;

	if (RK_GETINUM(rk_eval_register[2]) == 0)
		rk_eval_register[0] = RK_SOBJ_NIL;
	else {
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = (rk_object)cp;
		while (rk_eval_register[1] != RK_SOBJ_NIL) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[1]);
			cp[1] = rk_eval_register[0];
			rk_eval_register[0] = (rk_object)cp;
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
	}
	cp = RkAllocCells(4);
	cp[0] = RP_CDR(rk_eval_register[3]);
	cp[1] = (rk_object)&cp[2];
	cp[2] = rp_eval_fun;
	cp[3] = RK_SOBJ_NIL;
	proc = RP_CAR(RP_CAR(rk_eval_register[3]));
	rk_eval_register[1] = (rk_object)cp;
	rk_eval_register[2] = RK_MAKEINUM(3);
	rk_eval_register[3] = RP_CDR(RP_CAR(rk_eval_register[3]));
	rp_eval_car_proc = rp_default_eval_car_proc;
	rp_eval_fun = RK_SOBJ_FALSE;
	return	proc;
}

int
RpInitializeReflection(int index)
{
	rk_object *cp;

	if (index != -1) {
		rp_eval_fun = RK_SOBJ_FALSE;
		p_traverse = rk_traverse_root;
		rk_traverse_root = traverse;
		rp_reflect_eval_proc = RkRegisterProcedure(index + 0, reflect_eval);
		reflect_eval_conti1_proc = RkRegisterProcedure(index + 1, reflect_eval_conti1);
		debugcont_proc = RkRegisterProcedure(index + 2, debugcont);
		RP_DEFINESUBR("rp:hook-evaluator", rp_hookeval_proc, index + 3, hookeval);
		hookeval_conti1_proc = RkRegisterProcedure(index + 4, hookeval_conti1);
		RP_DEFINESUBR("rp:top-level-environment", rp_topenv_proc, index + 5, topenv);
		RP_DEFINESUBR("rp:expression->data", rp_exp2data_proc, index + 6, exp2data);
		RP_DEFINESUBR("rp:call-evaluator", rp_calleval_proc, index + 7, calleval);
		RP_DEFINESUBR("rp:apply-with-evaluator-hook", rp_applhook_proc, index + 8, applhook);
		applhook_conti1_proc = RkRegisterProcedure(index + 9, applhook_conti1);
		RP_DEFINESUBR("rp:hook-applicator", rp_hookappl_proc, index + 10, hookappl);
		RP_DEFINESUBR("rp:unhook-applicator", rp_unhookappl_proc, index + 11, unhookappl);
		reflect_apply_proc = RkRegisterProcedure(index + 12, reflect_apply);
		rk_valid_register = 0;
	}
	return	13;
}
