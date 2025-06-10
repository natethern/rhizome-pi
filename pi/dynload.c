/*
 * Copyright (c) 1996-99,2002,04 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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
static char rcsid[] = "@(#)$Id: dynload.c,v 1.8 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: dynload.c,v $
 * Revision 1.8  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.7  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.6  2004/07/23 05:09:31  qfwfq
 * workaround of lcc win32 potimizer bug
 *
 * Revision 1.5  2002/09/27 12:07:56  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.4  1999/06/15 07:43:41  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.3  1999/03/15 12:57:25  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.2  1999/02/15 08:37:16  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.1  1997/10/16 06:24:53  qfwfq
 * Release version 0.40
 *
 */

/*
 * Support of dynamic loading.
 */
#include "rhiz_pi.h"

#include <float.h>

static char *
str_obj2asciz(rk_object obj)
{
	unsigned len;
	char *s, *ss;

	len = (RP_CAR(obj) >> 12);
	s = RkGetMallocObject(obj);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (!(ss = malloc(len+1)))
		return	NULL;
	strncpy(ss, s, len);
	ss[len] = '\0';
	return	ss;
}

static rk_object lcmp_cont_proc;
static rk_object
lcmp_cont(void)
{
	rk_object *cp;
	int n;

	if ((n = RK_GETINUM(rk_continuation[3])) == RK_GETINUM(rk_continuation[2])) {
		rk_continuation = (rk_object *)rk_continuation[5];
		rk_eval_register[0] = RK_SOBJ_UNSPEC;
		RP_RETURN();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = lcmp_cont_proc;
	cp[2] = rk_continuation[2];
	cp[3] = RK_MAKEINUM(n+1);
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	((rk_object *)rk_continuation[4])[n];
}

/* #if (!defined(WIN32) || defined(__BORLANDC__)) && !defined(__CYGWIN32__) && !defined(__BEOS__) */
#ifndef RK_NO_LEADING_UNDERSCORE
#	define SYMPREFIX	"_"
#else
#	define SYMPREFIX	""
#endif
#define PDESCSYM	SYMPREFIX "RpProgramDesc"

rk_object rp_load_cmpmod_proc;
static rk_object
load_cmpmod(void)
{
	char *libname;
	void *libhnd;
	struct RP_PROGRAM_DESC const *(*pdesc)(void);
	struct RP_PROGRAM_DESC const *pd;
	int pn, m, i;
	rk_object *cp;

	RP_ASSERTARG(1);
	if (!RK_ISSTRING(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(libname = str_obj2asciz(rk_eval_register[0])))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	libhnd = RkLoadSharedObj(libname);
	free(libname);
	if (!libhnd)
		RK_SIGNAL_ERROR(RK_ERROR_DYNLOAD, rk_error_obj);
	if (!(pdesc = (struct RP_PROGRAM_DESC const *(*)(void))RkGetObjEntry(libhnd, PDESCSYM))) {
		RkUnloadSharedObj(libhnd);
		RK_SIGNAL_ERROR(RK_ERROR_DYNLOAD, rk_error_obj);
	}
	pd = pdesc();
	if (pd->rp_nmodules == 0) {
		rk_eval_register[0] = RK_SOBJ_UNSPEC;
		RP_RETURN();
	}
	if ((pn = RpCallInitProcs(-1, pd->rp_nmodules, pd->rp_initprocs)) == -1
	 || (pn = RkExtendProcsArray(pn)) == -1
	 || RpCallInitProcs(pn, pd->rp_nmodules, pd->rp_initprocs) == -1)
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	m = (pd->rp_nmodules+1)&~1;
	cp = RkAllocCells(6+m);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = lcmp_cont_proc;
	cp[2] = RK_MAKEINUM(pd->rp_nmodules);
	cp[3] = RK_MAKEINUM(1);
	cp[4] = (rk_object)&cp[6];
	cp[5] = (rk_object)rk_continuation;
	cp[6] = RK_VECTOR_TAG(m, 0);
	cp[m+5] = RK_DUMMY_OBJ;
	for (i = 1; i < pd->rp_nmodules; ++i)
		cp[6+i] = (pd->rp_runprocs[i])();
	rk_continuation = cp;
	return	(pd->rp_runprocs[0])();
}

rk_object rp_load_extobj_proc;
static rk_object
load_extobj(void)
{
	char *libname;
	void *libhnd;
	rk_object *cp;

	RP_ASSERTARG(1);
	if (!RK_ISSTRING(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(libname = str_obj2asciz(rk_eval_register[0])))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	libhnd = RkLoadSharedObj(libname);
	free(libname);
	if (!libhnd)
		RK_SIGNAL_ERROR(RK_ERROR_DYNLOAD, rk_error_obj);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_SHAREDOBJ);
	cp[1] = ((unsigned long)libhnd&0xffff0000) | RK_MAKEINUM(0);
	cp[2] = RK_MAKEINUM((unsigned long)libhnd&0xffff);
	cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_unload_extobj_proc;
static rk_object
unload_extobj(void)
{
	rk_object *cp;
	void *libhnd;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_SHAREDOBJ))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	*(unsigned long *)&libhnd = (cp[1]&0xffff0000) | (cp[2]>>2);
	RkUnloadSharedObj(libhnd);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_importp_proc;
static rk_object
importp(void)
{
	void *libhnd;
	char *entname;
	void *(*p)(void);
	rk_object *cp;

	RP_ASSERTARG(2);
	cp = (rk_object *)RP_CAR(rk_eval_register[1]);
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_SHAREDOBJ))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	*(unsigned long *)&libhnd = (cp[1]&0xffff0000) | (cp[2]>>2);
	if (!RK_ISSTRING(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(entname = str_obj2asciz(rk_eval_register[0])))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	p = RkGetObjEntry(libhnd, entname);
	free(entname);
	if (!p)
		RK_SIGNAL_ERROR(RK_ERROR_DYNLOAD, rk_error_obj);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_EXTPROC);
	cp[1] = ((unsigned long)p&0xffff0000) | RK_MAKEINUM(0);
	cp[2] = RK_MAKEINUM((unsigned long)p&0xffff);
	cp[3] = RK_MAKEINUM(-1);
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_callext_proc;
static rk_object
callext(void)
{
	rk_object *cp;

	RP_ASSERTARG(2);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = (rk_object *)(rk_eval_register[1] = RP_CAR(rk_eval_register[1]));
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_EXTPROC))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	return	rk_call_external_proc;
}

static rk_object callint_proc;
static rk_object
callint(void)
{
	rk_object obj;

	obj = RP_CAR(rk_eval_register[1]) & ~7;
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RP_CDR(obj);
	rk_valid_register = 4;
	return	RP_CAR(obj);
}

rk_object rp_exportp_proc;
static rk_object
exportp(void)
{
	void *(*p)(void);
	int index;
	rk_object *cp;

	RP_ASSERTARG(2);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3
	 || !RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(p = RkMakeCallbackEntry(callint_proc, RK_GETINUM(RP_CAR(rk_eval_register[1])), &index)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_EXTPROC);
	cp[1] = ((unsigned long)p&0xffff0000) | RK_MAKEINUM(0);
	cp[2] = RK_MAKEINUM((unsigned long)p&0xffff);
	cp[3] = RK_MAKEINUM(index);
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_unexportp_proc;
static rk_object
unexportp(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_EXTPROC))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (cp[3] != RK_MAKEINUM(-1))
		RkDestroyCallbackEntry((void *(*)(void))((cp[1]&0xffff0000) | (cp[2]>>2)), RK_GETINUM(cp[3]));
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

#define EA_T_SGINT	0
#define EA_T_USINT	1
#define EA_T_DBLFA	2
#define EA_T_DBLFB	3
#define EA_T_SCSTR	4
#define EA_T_EXTBF	5
#define EA_T_SCPRC	6

rk_object rp_extarg_proc;
static rk_object
extarg(void)
{
	rk_object obj, tcode, *cp;
	int s, i;
	unsigned long l;
	double x;

	if ((s = RK_GETINUM(rk_eval_register[2]))&1 || s >= RK_BULK_ALLOC_THRESHOLD)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	cp = RkAllocCells(s+2);
	cp[0] = RK_VECTOR_TAG(s+1, RK_TCODE_EXTARGV);
	cp[s+1] = RK_DUMMY_OBJ;
	obj = rk_eval_register[0];
	rk_eval_register[0] = (rk_object)cp;
	cp = &cp[1];
	for (i = s/2; i--; ) {
		tcode = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		switch (RK_ISINUM(tcode) ? RK_GETINUM(tcode) : -1) {
		case EA_T_SGINT:
			if (RK_ISINUM(obj))
				l = RK_GETINUM(obj);
			else if (obj&7)
				goto	argerror;
			else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS))
				l = ((rk_object *)obj)[1]>>2;
			else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(2, RK_TCODE_BIGINT_NEG))
				l = (unsigned long)-(signed long)(((rk_object *)obj)[1]>>2);
			else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS)) {
				if ((l = ((rk_object *)obj)[2]) & 0xfffffff0)
					goto	argerror;
				l = ((l&0xc)<<28)|(((rk_object *)obj)[1]>>2);
				if ((signed long)l < 0)
					goto	argerror;
			} else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(3, RK_TCODE_BIGINT_NEG)) {
				if ((l = ((rk_object *)obj)[2]) & 0xfffffff0)
					goto	argerror;
				l = ((l&0xc)<<28)|(((rk_object *)obj)[1]>>2);
#ifndef __LCC__
				l = (unsigned long)-(signed long)l;
#else	/* lcc optimizer enbugs here, this fixes it (by chance?) */
				{
					signed long tmp = (signed long)l;
					*(signed long *)&l = -tmp; _asm("");
				}
#endif
				if ((signed long)l > 0)
					goto	argerror;
			} else
				goto	argerror;
			cp[i*2] = (l&0xffff0000) | RK_MAKEINUM(0);
			cp[i*2+1] = RK_MAKEINUM(l&0xffff);
			break;
		case EA_T_USINT:
			if (RK_ISINUM(obj)) {
				l = RK_GETINUM(obj);
				if ((signed long)l < 0)
					goto	argerror;
			} else if (obj&7)
				goto	argerror;
			else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS))
				l = ((rk_object *)obj)[1]>>2;
			else if (((rk_object *)obj)[0] == RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS)) {
				if ((l = ((rk_object *)obj)[2]) & 0xfffffff0)
					goto	argerror;
				l = ((l&0xc)<<28)|(((rk_object *)obj)[1]>>2);
			} else
				goto	argerror;
			cp[i*2] = (l&0xffff0000) | RK_MAKEINUM(0);
			cp[i*2+1] = RK_MAKEINUM(l&0xffff);
			break;
		case EA_T_DBLFA:
			l = ((unsigned long *)&x)[0];
			cp[i*2] = (l&0xffff0000) | RK_MAKEINUM(0);
			cp[i*2+1] = RK_MAKEINUM(l&0xffff);
			break;
		case EA_T_DBLFB:
			if (obj&7 || ((rk_object *)obj)[0] != RK_VECTOR_TAG(4, RK_TCODE_FLONUM))
				goto	argerror;
			x = RkLoadFloat(obj);
			l = ((unsigned long *)&x)[1];
			cp[i*2] = (l&0xffff0000) | RK_MAKEINUM(0);
			cp[i*2+1] = RK_MAKEINUM(l&0xffff);
			break;
		case EA_T_SCSTR:
			if (!RK_ISSTRING(obj)) {
				if (obj == RK_SOBJ_FALSE) {
					cp[i*2] = RK_MAKEINUM(0);
					cp[i*2+1] = RK_MAKEINUM(0);
					break;
				}
				goto	argerror;
			}
			l = (unsigned long)((char *)RkGetMallocObject(obj) + (RP_CAR(obj) >> 12 ? 0 : 4));
			cp[i*2] = (l&0xffff0000) | RK_MAKEINUM(0);
			cp[i*2+1] = RK_MAKEINUM(l&0xffff);
			break;
		case EA_T_EXTBF:
			if (obj&7 || ((rk_object *)obj)[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV)) {
				if (obj == RK_SOBJ_FALSE) {
					cp[i*2] = RK_MAKEINUM(0);
					cp[i*2+1] = RK_MAKEINUM(0);
					break;
				}
				goto	argerror;
			}
			cp[i*2] = ((rk_object *)obj)[1];
			cp[i*2+1] = ((rk_object *)obj)[2];
			break;
		case EA_T_SCPRC:
			if (obj&7 || ((rk_object *)obj)[0] != RK_VECTOR_TAG(4, RK_TCODE_EXTPROC)) {
				if (obj == RK_SOBJ_FALSE) {
					cp[i*2] = RK_MAKEINUM(0);
					cp[i*2+1] = RK_MAKEINUM(0);
					break;
				}
				goto	argerror;
			}
			cp[i*2] = ((rk_object *)obj)[1];
			cp[i*2+1] = ((rk_object *)obj)[2];
			break;
argerror:	default:
			for (i = 0; i < s; ++i)
				cp[i] = RK_DUMMY_OBJ;
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
		if (i) {
			obj = RP_CAR(rk_eval_register[1]);
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
	}
	RP_RETURN();
}

rk_object rp_extval_proc;
static rk_object
extval(void)
{
	rk_object *cp;
	unsigned long l;

	RP_ASSERTARG(2);
	cp = (rk_object *)RP_CAR(rk_eval_register[1]);
	if (!RK_ISINUM(rk_eval_register[0])
	 || ((unsigned long)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	l = (cp[1]&0xffff0000) | (cp[2]>>2);
	switch (RK_GETINUM(rk_eval_register[0])) {
	case EA_T_SGINT:
		if (-0x20000000 <= (signed long)l && (signed long)l <= 0x1fffffff)
			rk_eval_register[0] = RK_MAKEINUM(l);
		else if (0 <= (signed long)l && (signed long)l <= 0x3fffffff) {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(2));
			cp[0] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS);
			cp[1] = RK_MAKEINUM(l);
		} else if (-0x3fffffff <= (signed long)l && (signed long)l < 0) {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(2));
			cp[0] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_NEG);
			cp[1] = RK_MAKEINUM((unsigned long)-(signed long)l);
		} else if (0 <= (signed long)l) {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(4));
			cp[0] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS);
			cp[1] = RK_MAKEINUM(l);
			cp[2] = RK_MAKEINUM(l>>30);
			cp[3] = RK_DUMMY_OBJ;
		} else {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(4));
			cp[0] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_NEG);
			cp[1] = RK_MAKEINUM(l = (unsigned long)-(signed long)l);
			cp[2] = RK_MAKEINUM(l>>30);
			cp[3] = RK_DUMMY_OBJ;
		}
		break;
	case EA_T_USINT:
		if (l <= 0x1fffffff)
			rk_eval_register[0] = RK_MAKEINUM(l);
		else if (l <= 0x3fffffff) {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(2));
			cp[0] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS);
			cp[1] = RK_MAKEINUM(l);
		} else {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(4));
			cp[0] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS);
			cp[1] = RK_MAKEINUM(l);
			cp[2] = RK_MAKEINUM(l>>30);
			cp[3] = RK_DUMMY_OBJ;
		}
		break;
	case EA_T_EXTBF:
		if (l)
			rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
		else
			rk_eval_register[0] = RK_SOBJ_FALSE;
		break;
	case EA_T_SCPRC:
		if (l) {
			rk_eval_register[0] = (rk_object)(cp = RkAllocCells(4));
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_EXTPROC);
			cp[1] = ((unsigned long)l&0xffff0000) | RK_MAKEINUM(0);
			cp[2] = RK_MAKEINUM((unsigned long)l&0xffff);
			cp[3] = RK_MAKEINUM(-1);
		} else
			rk_eval_register[0] = RK_SOBJ_FALSE;
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
	RP_RETURN();
}

rk_object rp_mkextbf_proc;
static rk_object
mkextbf(void)
{
	int n;
	void *p;
	rk_object *cp;

	RP_ASSERTARG(1);
	if (!RK_ISINUM(rk_eval_register[0]) || (n = RK_GETINUM(rk_eval_register[0])) < 0)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(p = malloc(n*4)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(3, RK_TCODE_EXTARGV);
	cp[1] = ((unsigned long)p&0xffff0000) | RK_MAKEINUM(0);
	cp[2] = RK_MAKEINUM((unsigned long)p&0xffff);
	cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_dlextbf_proc;
static rk_object
dlextbf(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	free((void *)((cp[1]&0xffff0000) | (cp[2]>>2)));
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_stextbf_proc;
static rk_object
stextbf(void)
{
	rk_object obj, *cp;
	int off, s, i;
	unsigned long *p;

	RP_ASSERTARG(3);
	obj = RP_CAR(rk_eval_register[1]);
	if (!RK_ISINUM(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(obj);
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = (unsigned long *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	cp = (rk_object *)rk_eval_register[0];
	if (((rk_object)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = (cp++)[0] >> 13;
	for (i = 0; i < s; ++i)
		p[i] = (cp[i*2]&0xffff0000) | (cp[i*2+1]>>2);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_ldextbf_proc;
static rk_object
ldextbf(void)
{
	rk_object obj, *cp;
	int off;
	unsigned long *p, l;
	double x;

	RP_ASSERTARG(3);
	obj = RP_CAR(rk_eval_register[1]);
	if (!RK_ISINUM(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(obj);
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = (unsigned long *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = RK_SOBJ_NIL;
	while (rk_eval_register[1] != RK_SOBJ_NIL) {
		if (!RP_ISCONS(rk_eval_register[1]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		obj = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		if (!RK_ISINUM(obj))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		switch (RK_GETINUM(obj)) {
		case EA_T_SGINT:
			l = (p++)[0];
			if (-0x20000000 <= (signed long)l && (signed long)l <= 0x1fffffff) {
				cp = RkAllocCells(2);
				cp[0] = RK_MAKEINUM(l);
				break;
			} else if (0 <= (signed long)l && (signed long)l <= 0x3fffffff) {
				cp = RkAllocCells(4);
				cp[2] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS);
				cp[3] = RK_MAKEINUM(l);
			} else if (-0x3fffffff <= (signed long)l && (signed long)l < 0) {
				cp = RkAllocCells(4);
				cp[2] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_NEG);
				cp[3] = RK_MAKEINUM((unsigned long)-(signed long)l);
			} else if (0 <= (signed long)l) {
				cp = RkAllocCells(6);
				cp[2] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS);
				cp[3] = RK_MAKEINUM(l);
				cp[4] = RK_MAKEINUM(l>>30);
				cp[5] = RK_DUMMY_OBJ;
			} else {
				cp = RkAllocCells(6);
				cp[2] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_NEG);
				cp[3] = RK_MAKEINUM(l = (unsigned long)-(signed long)l);
				cp[4] = RK_MAKEINUM(l>>30);
				cp[5] = RK_DUMMY_OBJ;
			}
			cp[0] = (rk_object)&cp[2];
			break;
		case EA_T_USINT:
			l = (p++)[0];
			if (l <= 0x1fffffff) {
				cp = RkAllocCells(2);
				cp[0] = RK_MAKEINUM(l);
				break;
			} else if (l <= 0x3fffffff) {
				cp = RkAllocCells(4);
				cp[2] = RK_VECTOR_TAG(2, RK_TCODE_BIGINT_POS);
				cp[3] = RK_MAKEINUM(l);
			} else {
				cp = RkAllocCells(6);
				cp[2] = RK_VECTOR_TAG(3, RK_TCODE_BIGINT_POS);
				cp[3] = RK_MAKEINUM(l);
				cp[4] = RK_MAKEINUM(l>>30);
				cp[5] = RK_DUMMY_OBJ;
			}
			cp[0] = (rk_object)&cp[2];
			break;
		case EA_T_DBLFA:
			x = ((*(double **)&p)++)[0];
			rk_eval_register[2] = RkStoreFloat(x);
			cp = RkAllocCells(2);
			cp[0] = rk_eval_register[2];
			break;
		case EA_T_EXTBF:
			if (l = (p++)[0]) {
				cp = RkAllocCells(6);
				cp[2] = RK_VECTOR_TAG(3, RK_TCODE_EXTARGV);
				cp[3] = (l&0xffff0000) | RK_MAKEINUM(0);
				cp[4] = RK_MAKEINUM(l&0xffff);
				cp[5] = RK_DUMMY_OBJ;
				cp[0] = (rk_object)&cp[2];
			} else {
				cp = RkAllocCells(2);
				cp[0] = RK_SOBJ_FALSE;
			}
			break;
		case EA_T_SCPRC:
			if (l = (p++)[0]) {
				cp = RkAllocCells(6);
				cp[2] = RK_VECTOR_TAG(4, RK_TCODE_EXTPROC);
				cp[3] = (l&0xffff0000) | RK_MAKEINUM(0);
				cp[4] = RK_MAKEINUM(l&0xffff);
				cp[5] = RK_MAKEINUM(-1);
				cp[0] = (rk_object)&cp[2];
			} else {
				cp = RkAllocCells(2);
				cp[0] = RK_SOBJ_FALSE;
			}
			break;
		default:
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		}
		cp[1] = rk_eval_register[0];
		rk_eval_register[0] = (rk_object)cp;
	}
	RP_RETURN();
}

rk_object rp_stebfhw_proc;
static rk_object
stebfhw(void)
{
	rk_object obj, *cp;
	int off, n;

	RP_ASSERTARG(4);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	n = RK_GETINUM(rk_eval_register[0]);
	if (RP_CAR(rk_eval_register[1]) == RK_SOBJ_TRUE) {
		if (n < -0x8000 || 0x7fff < n)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	} else if (RP_CAR(rk_eval_register[1]) == RK_SOBJ_FALSE) {
		if (n < 0 || 0xffff < n)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	} else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CDR(rk_eval_register[1]);
	if (!RK_ISINUM(RP_CAR(obj)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(RP_CAR(obj));
	cp = (rk_object *)RP_CAR(RP_CDR(obj));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	*(short *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off) = (short)n;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_ldebfhw_proc;
static rk_object
ldebfhw(void)
{
	rk_object *cp;
	int off, n;
	short *p;

	RP_ASSERTARG(3);
	if (!RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = (short *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	if (rk_eval_register[0] == RK_SOBJ_TRUE)
		n = *p;
	else if (rk_eval_register[0] == RK_SOBJ_FALSE)
		n = *(unsigned short *)p;
	else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_MAKEINUM(n);
	RP_RETURN();
}

rk_object rp_stebfch_proc;
static rk_object
stebfch(void)
{
	int size, off;
	unsigned len;
	char *p, *s;
	rk_object *cp;

	if (rk_eval_register[2] == RK_MAKEINUM(3))
		size = -2;
	else if (rk_eval_register[2] == RK_MAKEINUM(4)) {
		if (rk_eval_register[0] == RK_SOBJ_FALSE)
			size = -1;
		else if (RK_ISINUM(rk_eval_register[0])) {
			if ((size = RK_GETINUM(rk_eval_register[0])) < 0)
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		} else
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
	} else
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if (!RK_ISSTRING(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = RP_CAR(rk_eval_register[0]) >> 12;
	s = RkGetMallocObject(rk_eval_register[0]);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (size == -2)
		size = len;
	else if (size == -1)
		size = len+1;
	if (!RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = (char *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	if (size <= len)
		memcpy(p, s, size);
	else {
		memcpy(p, s, len);
		memset(p+len, 0, size-len);
	}
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_ldebfch_proc;
static rk_object
ldebfch(void)
{
	int size, off, len, i;
	char *s, *p;
	rk_object *cp;

	RP_ASSERTARG(3);
	if (rk_eval_register[0] == RK_SOBJ_FALSE)
		size = -1;
	else if (RK_ISINUM(rk_eval_register[0])) {
		if ((size = RK_GETINUM(rk_eval_register[0])) < 0)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	} else
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = (char *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	if (size == -1)
		size = strlen(p);
	i = len = size;
	if (size == 0 || size >= (1<<20)) {
		size += 4;
		i = 0;
	}
	if (!(s = malloc(size)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (!i) {
		*(unsigned long *)s = len;
		s += 4;
	}
	memcpy(s, p, len);
	RP_RETURN();
}

rk_object rp_skpebf_proc;
static rk_object
skpebf(void)
{
	int off;
	unsigned long p;
	rk_object *cp;

	RP_ASSERTARG(2);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(rk_eval_register[0]);
	cp = (rk_object *)RP_CAR(rk_eval_register[1]);
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	p = ((cp[1]&0xffff0000) | (cp[2]>>2)) + off;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(3, RK_TCODE_EXTARGV);
	cp[1] = (p&0xffff0000) | RK_MAKEINUM(0);
	cp[2] = RK_MAKEINUM(p&0xffff);
	cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_stebfsf_proc;
static rk_object
stebfsf(void)
{
	rk_object obj, *cp;
	int off;
	double x;

	RP_ASSERTARG(3);
	obj = rk_eval_register[0];
	if (obj&7 || ((rk_object *)obj)[0] != RK_VECTOR_TAG(4, RK_TCODE_FLONUM))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	x = RkLoadFloat(obj);
	if (x < -FLT_MAX || FLT_MAX < x)
		RK_SIGNAL_ERROR1(RK_ERROR_OVERFLOW);
	obj = RP_CAR(rk_eval_register[1]);
	if (!RK_ISINUM(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(obj);
	cp = (rk_object *)RP_CAR(RP_CDR(rk_eval_register[1]));
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	*(float *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off) = x;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_ldebfsf_proc;
static rk_object
ldebfsf(void)
{
	rk_object *cp;
	int off;
	double x;

	RP_ASSERTARG(2);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	off = RK_GETINUM(rk_eval_register[0]);
	cp = (rk_object *)RP_CAR(rk_eval_register[1]);
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	x = *(float *)(((cp[1]&0xffff0000) | (cp[2]>>2)) + off);
	rk_eval_register[0] = RkStoreFloat(x);
	RP_RETURN();
}

rk_object rp_ebfp_proc;
static rk_object
ebfp(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		rk_eval_register[0] = RK_SOBJ_FALSE;
	else
		rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_eprocp_proc;
static rk_object
eprocp(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((rk_object)cp & 7) || cp[0] != RK_VECTOR_TAG(4, RK_TCODE_EXTPROC))
		rk_eval_register[0] = RK_SOBJ_FALSE;
	else
		rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

int
RpInitializeDynload(int index)
{
	if (index != -1) {
		lcmp_cont_proc = RkRegisterProcedure(index + 0, lcmp_cont);
		RP_DEFINESUBR("rp:load-compiled-module", rp_load_cmpmod_proc, index + 1, load_cmpmod);
		RP_DEFINESUBR("rp:load-external-object", rp_load_extobj_proc, index + 2, load_extobj);
		RP_DEFINESUBR("rp:unload-external-object", rp_unload_extobj_proc, index + 3, unload_extobj);
		RP_DEFINESUBR("rp:import-procedure", rp_importp_proc, index + 4, importp);
		RP_DEFINESUBR("rp:call-external-procedure", rp_callext_proc, index + 5, callext);
		callint_proc = RkRegisterProcedure(index + 6, callint);
		RP_DEFINESUBR("rp:export-procedure", rp_exportp_proc, index + 7, exportp);
		RP_DEFINESUBR("rp:destroy-exported-procedure", rp_unexportp_proc, index + 8, unexportp);
		RP_DEFINESUBR("rp:external-arguments", rp_extarg_proc, index + 9, extarg);
		RP_DEFINESUBR("rp:load-external-value", rp_extval_proc, index + 10, extval);
		RP_DEFINESUBR("rp:make-external-buffer", rp_mkextbf_proc, index + 11, mkextbf);
		RP_DEFINESUBR("rp:destroy-external-buffer", rp_dlextbf_proc, index + 12, dlextbf);
		RP_DEFINESUBR("rp:store-external-data", rp_stextbf_proc, index + 13, stextbf);
		RP_DEFINESUBR("rp:load-external-data", rp_ldextbf_proc, index + 14, ldextbf);
		RP_DEFINESUBR("rp:store-external-halfword", rp_stebfhw_proc, index + 15, stebfhw);
		RP_DEFINESUBR("rp:load-external-halfword", rp_ldebfhw_proc, index + 16, ldebfhw);
		RP_DEFINESUBR("rp:store-external-chars", rp_stebfch_proc, index + 17, stebfch);
		RP_DEFINESUBR("rp:load-external-chars", rp_ldebfch_proc, index + 18, ldebfch);
		RP_DEFINESUBR("rp:skip-buffer-element", rp_skpebf_proc, index + 19, skpebf);
		RP_DEFINESUBR("rp:store-external-single-float", rp_stebfsf_proc, index + 20, stebfsf);
		RP_DEFINESUBR("rp:load-external-single-float", rp_ldebfsf_proc, index + 21, ldebfsf);
		RP_DEFINESUBR("rp:external-buffer?", rp_ebfp_proc, index + 22, ebfp);
		RP_DEFINESUBR("rp:exported-procedure?", rp_eprocp_proc, index + 23, eprocp);
		rk_valid_register = 0;
	}
	return	24;
}
