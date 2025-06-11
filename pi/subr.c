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
static char rcsid[] = "@(#)$Id: subr.c,v 1.14 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: subr.c,v $
 * Revision 1.14  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.13  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.12  2002/09/27 12:07:57  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.11  1999/06/15 07:43:45  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.10  1999/02/15 08:47:42  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.9  1998/07/31 11:03:02  qfwfq
 * Add symbol-aux-datum
 *
 * Revision 1.8  1997/10/16 06:24:56  qfwfq
 * Release version 0.40
 *
 * Revision 1.7  1997/05/12 07:21:22  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.6  1997/04/26 13:29:08  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.5  1996/10/10 08:27:01  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.4  1996/09/06 06:11:32  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.3  1996/06/05 05:44:27  qfwfq
 * Fix declaration of sys_siglist.
 *
 * Revision 1.2  93/11/13  16:14:54  qfwfq
 * Add procedure-port feature.
 * 
 * Revision 1.1  93/11/08  14:09:53  qfwfq
 * Initial revision
 * 
 */

/*
 * General subr functions.
 */
#include "rhiz_pi.h"

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#if !(defined(WIN32) && !defined(__CYGWIN32__)) && !defined(__BEOS__)
#include <sys/time.h>
#include <sys/resource.h>
#elif defined(WIN32) && !defined(__CYGWIN32__)
#include <time.h>
# ifdef __LCC__
#	define CLK_TCK	CLOCKS_PER_SEC
# endif
#else
#include <be/kernel/OS.h>
#endif

#ifndef ENAMETOOLONG
#	define	ENAMETOOLONG	EINVAL
#endif

#define SIGNAL_VECTOR_SIZE	((NSIG+2)&~1)

#ifndef __CYGWIN32__
extern int errno;
#endif

rk_object *rp_dynamic_extent;
static rk_object *signal_handlers;
static rk_object *cmd_args;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	(*scan_fun)((rk_object *)&rp_dynamic_extent, cookie);
	(*scan_fun)((rk_object *)&signal_handlers, cookie);
	(*scan_fun)((rk_object *)&cmd_args, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

rk_object rp_error_proc;
static rk_object
error(void)
{
	int code;
	rk_object obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		code = RP_ERROR_PROGRAM;
		obj = RK_SOBJ_UNSPEC;
		break;
	case 1:
		if (RK_ISINUM(rk_eval_register[0]))
			code = RK_GETINUM(rk_eval_register[0]);
		else
			code = RP_ERROR_ILLEGALARG;
		obj = RK_SOBJ_UNSPEC;
		break;
	case 2:
		if (RK_ISINUM(RP_CAR(rk_eval_register[1]))) {
			code = RK_GETINUM(RP_CAR(rk_eval_register[1]));
			obj = rk_eval_register[0];
		} else {
			code = RP_ERROR_ILLEGALARG;
			obj = RK_SOBJ_UNSPEC;
		}
		break;
	default:
		code = RP_ERROR_ARGNO;
		obj = RK_SOBJ_UNSPEC;
		break;
	}
	RK_SIGNAL_ERROR(code, obj);
}

static char mes_buf[256];

rk_object rp_errormess_proc;
static rk_object
errormess(void)
{
	char *s;
	int n;

	RP_ASSERTARG(2);
	if (!RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	n = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	if (n == 0)
		mes_buf[0] = '\0';
	else
		sprintf(mes_buf, "Error #%d", n);
	n = strlen(mes_buf);
	if (!(s = malloc(n ? n : 4))) {
		RkScavenge(1);
		if (!(s = malloc(n ? n : 4)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (n)
		strncpy(s, mes_buf, n);
	else
		*(unsigned long *)s = 0;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(n, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	RP_RETURN();
}

rk_object rp_car_proc;
static rk_object
car(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (!RK_ISCELL(obj) || (RP_CAR(obj) & 1))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CAR(obj);
	RP_RETURN();
}

rk_object rp_cdr_proc;
static rk_object
cdr(void)
{
	rk_object obj;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	if (!RK_ISCELL(obj) || (RP_CAR(obj) & 1))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CDR(obj);
	RP_RETURN();
}

rk_object rp_cons_proc;
static rk_object
cons(void)
{
	rk_object *cp;

	RP_ASSERTARG(2);
	cp = RkAllocCells(2);
	cp[0] = RP_CAR(rk_eval_register[1]);
	cp[1] = rk_eval_register[0];
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_symbolp_proc;
static rk_object
symbolp(void)
{
	RP_ASSERTARG(1);
	if (RK_ISSYMBOL(rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_list_proc;
static rk_object
list(void)
{
	int n, i;
	rk_object *cp, obj;

	n = RK_GETINUM(rk_eval_register[2]);
	rk_eval_register[2] = RK_SOBJ_NIL;
	while (n*2 > RK_HEAP_CHUNK_SIZE) {
		cp = RkAllocCells(RK_HEAP_CHUNK_SIZE);
		cp[RK_HEAP_CHUNK_SIZE-2] = rk_eval_register[0];
		cp[RK_HEAP_CHUNK_SIZE-1] = rk_eval_register[2];
		for (obj = rk_eval_register[1], i = RK_HEAP_CHUNK_SIZE-4; i >= 0; i -= 2) {
			cp[i] = RP_CAR(obj);
			cp[i+1] = (rk_object)&cp[i+2];
			obj = RP_CDR(obj);
		}
		if ((n -= RK_HEAP_CHUNK_SIZE/2) > 0) {
			rk_eval_register[0] = RP_CAR(obj);
			rk_eval_register[1] = RP_CDR(obj);
		}
		rk_eval_register[2] = (rk_object)cp;
	}
	if (n > 0) {
		cp = RkAllocCells(n*2);
		cp[n*2-2] = rk_eval_register[0];
		cp[n*2-1] = rk_eval_register[2];
		for (obj = rk_eval_register[1], i = n*2-4; i >= 0; i -= 2) {
			cp[i] = RP_CAR(obj);
			cp[i+1] = (rk_object)&cp[i+2];
			obj = RP_CDR(obj);
		}
		rk_eval_register[0] = (rk_object)cp;
	} else
		rk_eval_register[0] = rk_eval_register[2];
	RP_RETURN();
}

rk_object rp_map_proc;
static rk_object map_conti1_proc;
static rk_object
map_func(void)
{
	int n, i;
	rk_object *cp, obj;

	if ((n = RK_GETINUM(rk_eval_register[2]) - 1) < 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if (rk_eval_register[0] == RK_SOBJ_NIL) {
		obj = rk_eval_register[1];
		for (i = 1; i < n; ++i) {
			if (RP_CAR(obj) != RK_SOBJ_NIL)
				RK_SIGNAL_ERROR1(RP_ISCONS(RP_CAR(obj)) ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			obj = RP_CDR(obj);
		}
		rk_eval_register[0] = RK_SOBJ_NIL;
		RP_RETURN();
	}
	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(2);
	cp[0] = RP_CDR(rk_eval_register[0]);
	cp[1] = RK_SOBJ_NIL;
	rk_eval_register[3] = rk_eval_register[1];
	rk_eval_register[0] = RP_CAR(rk_eval_register[0]);
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = (rk_object)cp;
	if (n > 1) {
		if (!RP_ISCONS(RP_CAR(rk_eval_register[3])))
			RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[3]) == RK_SOBJ_NIL
					 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
		cp = RkAllocCells(4);
		cp[0] = RP_CDR(RP_CAR(rk_eval_register[3]));
		cp[1] = rk_eval_register[2];
		rk_eval_register[2] = (rk_object)cp;
		cp[2] = RP_CAR(RP_CAR(rk_eval_register[3]));
		cp[3] = RK_SOBJ_NIL;
		rk_eval_register[4] = rk_eval_register[1] = (rk_object)&cp[2];
		rk_valid_register = 5;
		rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
		for (i = 2; i < n; ++i) {
			if (!RP_ISCONS(RP_CAR(rk_eval_register[3])))
				RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[3]) == RK_SOBJ_NIL
						 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			cp = RkAllocCells(4);
			cp[0] = RP_CDR(RP_CAR(rk_eval_register[3]));
			cp[1] = rk_eval_register[2];
			rk_eval_register[2] = (rk_object)cp;
			cp[2] = RP_CAR(RP_CAR(rk_eval_register[3]));
			cp[3] = RK_SOBJ_NIL;
			RkWriteCell(&RP_CDR(rk_eval_register[4]), (rk_object)&cp[2]);
			rk_eval_register[4] = RP_CDR(rk_eval_register[4]);
			rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
		}
	}
	obj = RP_CAR(rk_eval_register[3]);
	if ((obj & 7) || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[3] = RP_CAR(obj) & ~7;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = map_conti1_proc;
	cp[2] = rk_eval_register[3];
	cp[3] = rk_eval_register[2];
	cp[4] = RK_SOBJ_NIL;
	cp[5] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[2] = RK_MAKEINUM(n);
	rk_eval_register[3] = RP_CDR(rk_continuation[2]);
	rk_valid_register = 4;
	return	RP_CAR(rk_continuation[2]);
}

static rk_object
map_conti1(void)
{
	int i;
	rk_object *cp, obj;

	if (RP_CAR(rk_continuation[3]) == RK_SOBJ_NIL) {
		obj = RP_CDR(rk_continuation[3]);
		while (obj != RK_SOBJ_NIL) {
			if (RP_CAR(obj) != RK_SOBJ_NIL)
				RK_SIGNAL_ERROR1(RP_ISCONS(RP_CAR(obj)) ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			obj = RP_CDR(obj);
		}
		cp = RkAllocCells(2);
		cp[0] = rk_eval_register[0];
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[0] = (rk_object)cp;
		rk_eval_register[1] = rk_continuation[4];
		rk_valid_register = 2;
		while (rk_eval_register[1] != RK_SOBJ_NIL) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[1]);
			cp[1] = rk_eval_register[0];
			rk_eval_register[0] = (rk_object)cp;
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RP_RETURN();
	}
	if (!RP_ISCONS(RP_CAR(rk_continuation[3])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(4);
	cp[0] = rk_eval_register[0];
	cp[1] = rk_continuation[4];
	cp[2] = RP_CDR(RP_CAR(rk_continuation[3]));
	cp[3] = RK_SOBJ_NIL;
	rk_eval_register[0] = RP_CAR(RP_CAR(rk_continuation[3]));
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = (rk_object)cp;
	rk_eval_register[3] = RP_CDR(rk_continuation[3]);
	rk_eval_register[4] = rk_eval_register[5] = (rk_object)&cp[2];
	rk_valid_register = 6;
	for (i = 1; rk_eval_register[3] != RK_SOBJ_NIL; ++i) {
		if (!RP_ISCONS(RP_CAR(rk_eval_register[3])))
			RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[3]) == RK_SOBJ_NIL
					 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
		cp = RkAllocCells(4);
		cp[0] = rk_eval_register[0];
		cp[1] = rk_eval_register[1];
		rk_eval_register[1] = (rk_object)cp;
		rk_eval_register[0] = RP_CAR(RP_CAR(rk_eval_register[3]));
		cp[2] = RP_CDR(RP_CAR(rk_eval_register[3]));
		cp[3] = RK_SOBJ_NIL;
		RkWriteCell(&RP_CDR(rk_eval_register[5]), (rk_object)&cp[2]);
		rk_eval_register[5] = RP_CDR(rk_eval_register[5]);
		rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = map_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_eval_register[4];
	cp[4] = rk_eval_register[2];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	rk_eval_register[2] = RK_MAKEINUM(i);
	rk_eval_register[3] = RP_CDR(rk_continuation[2]);
	rk_valid_register = 4;
	return	RP_CAR(rk_continuation[2]);
}

rk_object rp_void_proc;
static rk_object
void_func(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_nullp_proc;
static rk_object
nullp(void)
{
	RP_ASSERTARG(1);
	if (rk_eval_register[0] == RK_SOBJ_NIL)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_eqp_proc;
static rk_object
eqp(void)
{
	RP_ASSERTARG(2);
	if (RP_CAR(rk_eval_register[1]) == rk_eval_register[0])
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_gensym_proc;
static rk_object
gensym(void)
{
	static unsigned counter = 0;
	char *s, buffer[16];
	rk_object *cp;

	RP_ASSERTARG(0);
	sprintf(buffer, "#<G.%08X>", counter++);
	if (!(s = malloc(13))) {
		RkScavenge(1);
		if (!(s = malloc(13)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	memcpy(s, buffer, 13);
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(13, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	cp = RkAllocCells(4);
	cp[0] = (rk_object)&cp[2] | 5;
	cp[1] = RK_SOBJ_UNBOUND;
	cp[2] = rk_eval_register[0];
	cp[3] = RK_SOBJ_UNBOUND;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_pairp_proc;
static rk_object
pairp(void)
{
	RP_ASSERTARG(1);
	if (RP_ISCONS(rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_vectorp_proc;
static rk_object
vectorp(void)
{
	RP_ASSERTARG(1);
	if (RK_ISVECTOR(rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_vectlen_proc;
static rk_object
vectlen(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	if (!RK_ISVECTOR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = (rk_object *)rk_eval_register[0];
	rk_eval_register[0] = RK_MAKEINUM(cp[0] >> 12 ? (cp[0] >> 12) - 1 : (cp[1] >> 2) - 2);
	RP_RETURN();
}

static rk_object *
get_vectdata(rk_object obj, int *len)
{
	rk_object *cp;

	cp = (rk_object *)obj;
	if (*len = (cp[0] >> 12)) {
		--*len;
		return	&cp[1];
	} else {
		*len = RK_GETINUM(cp[1]) - 2;
		return	&cp[2];
	}
}

rk_object rp_vectref_proc;
static rk_object
vectref(void)
{
	rk_object *cp;
	int len, n;

	RP_ASSERTARG(2);
	if (!RK_ISVECTOR(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = get_vectdata(RP_CAR(rk_eval_register[1]), &len);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	n = RK_GETINUM(rk_eval_register[0]);
	if (n < 0 || n >= len)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = cp[n];
	RP_RETURN();
}

static int
create_vector(int len)
{
	rk_object *cp;
	int s, i;

	if (len + 1 < RK_BULK_ALLOC_THRESHOLD) {
		cp = RkAllocCells((len+2)&~1);
		cp[0] = RK_VECTOR_TAG(len+1, RK_TCODE_VECTOR);
		cp[((len+2)&~1) - 1] = RK_DUMMY_OBJ;
		s = 0;
	} else {
		if (!(cp = RkAllocVector(len+1 + (len+1 >= (1<<20)))))
			return	-1;
		if (len+1 >= (1<<20)) {
			cp[0] = RK_VECTOR_TAG(0, RK_TCODE_VECTOR);
			cp[1] = RK_MAKEINUM(len+2);
			s = 2;
		} else {
			cp[0] = RK_VECTOR_TAG(len+1, RK_TCODE_VECTOR);
			s = 1;
		}
		for (i = 0; i < len; ++i)
			cp[s + i] = RK_DUMMY_OBJ;
	}
	rk_eval_register[0] = (rk_object)cp;
	return	s;
}

rk_object rp_vector_proc;
static rk_object
vector(void)
{
	rk_object *cp, obj;
	int i, len, s;

	rk_eval_register[3] = rk_eval_register[0];
	if ((s = create_vector(len = RK_GETINUM(rk_eval_register[2]))) == -1)
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (!s) {
		cp = &((rk_object *)rk_eval_register[0])[1];
		if (len) {
			cp[len-1] = rk_eval_register[3];
			obj = rk_eval_register[1];
			for (i = len-2; i >= 0; --i) {
				cp[i] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
		}
		RP_RETURN();
	}
	RkWriteCell(&((rk_object *)rk_eval_register[0])[s + len-1], rk_eval_register[3]);
	for (i = len-2; i >= 0; --i) {
		RkWriteCell(&((rk_object *)rk_eval_register[0])[s + i], RP_CAR(rk_eval_register[1]));
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
	}
	RP_RETURN();
}

rk_object rp_append_proc;
static rk_object
append(void)
{
	rk_object *cp;

	if (rk_eval_register[2] == RK_MAKEINUM(0)) {
		rk_eval_register[0] = RK_SOBJ_NIL;
		RP_RETURN();
	}
	rk_eval_register[3] = rk_eval_register[4] = RK_DUMMY_OBJ;
	rk_valid_register = 5;
	while (rk_eval_register[1] != RK_SOBJ_NIL) {
		rk_eval_register[2] = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		if (rk_eval_register[2] != RK_SOBJ_NIL) {
			if (!RP_ISCONS(rk_eval_register[2]))
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[2]);
			cp[1] = RK_SOBJ_NIL;
			rk_eval_register[3] = rk_eval_register[4] = (rk_object)cp;
			rk_eval_register[2] = RP_CDR(rk_eval_register[2]);
			while (rk_eval_register[2] != RK_SOBJ_NIL) {
				if (!RP_ISCONS(rk_eval_register[2]))
					RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
				cp = RkAllocCells(2);
				cp[0] = RP_CAR(rk_eval_register[2]);
				cp[1] = RK_SOBJ_NIL;
				RkWriteCell(&RP_CDR(rk_eval_register[4]), (rk_object)cp);
				rk_eval_register[4] = RP_CDR(rk_eval_register[4]);
				rk_eval_register[2] = RP_CDR(rk_eval_register[2]);
			}
			RkWriteCell(&RP_CDR(rk_eval_register[4]), rk_eval_register[0]);
			rk_eval_register[0] = rk_eval_register[3];
		}
	}
	RP_RETURN();
}

rk_object rp_apply_proc;
static rk_object
apply(void)
{
	int n, i;
	rk_object *cp, obj;

	n = RK_GETINUM(rk_eval_register[2]) - 2;
	if (n < 0)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if (n == 0)
		rk_eval_register[2] = RK_SOBJ_NIL;
	else {
		cp = RkAllocCells(2);
		cp[0] = RP_CAR(rk_eval_register[1]);
		cp[1] = RK_SOBJ_NIL;
		rk_eval_register[2] = rk_eval_register[3] = (rk_object)cp;
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		for (i = 1; i < n; ++i) {
			cp = RkAllocCells(2);
			cp[0] = RP_CAR(rk_eval_register[1]);
			cp[1] = RK_SOBJ_NIL;
			RkWriteCell(&RP_CDR(rk_eval_register[3]), (rk_object)cp);
			rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
			rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		}
	}
	while (rk_eval_register[0] != RK_SOBJ_NIL) {
		if (!RP_ISCONS(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		++n;
		cp = RkAllocCells(2);
		cp[0] = RP_CAR(rk_eval_register[0]);
		cp[1] = rk_eval_register[2];
		rk_eval_register[2] = (rk_object)cp;
		rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	}
	obj = RP_CAR(rk_eval_register[1]);
	if ((obj & 7) || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CAR(obj) & ~7;
	if (n == 0)
		rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	else {
		rk_eval_register[0] = RP_CAR(rk_eval_register[2]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[2]);
	}
	rk_eval_register[2] = RK_MAKEINUM(n);
	rk_eval_register[3] = RP_CDR(obj);
	return	RP_CAR(obj);
}

rk_object rp_memv_proc;
static rk_object
memv(void)
{
	rk_object obj, list;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	list = rk_eval_register[0];
	while (RP_ISCONS(list)) {
		if (RpEqvP(obj, RP_CAR(list))) {
			rk_eval_register[0] = list;
			RP_RETURN();
		}
		list = RP_CDR(list);
	}
	if (list != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_not_proc;
static rk_object
nnoott(void)
{
	RP_ASSERTARG(1);
	if (rk_eval_register[0] == RK_SOBJ_FALSE)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_boolenp_proc;
static rk_object
booleanp(void)
{
	RP_ASSERTARG(1);
	if (rk_eval_register[0] == RK_SOBJ_FALSE || rk_eval_register[0] == RK_SOBJ_TRUE)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_eqvp_proc;
static rk_object
eqvp(void)
{
	RP_ASSERTARG(2);
	if (RpEqvP(RP_CAR(rk_eval_register[1]), rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringp_proc;
static rk_object
stringp(void)
{
	RP_ASSERTARG(1);
	if (RK_ISSTRING(rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_setcarbang_proc;
static rk_object
setcarbang(void)
{
	rk_object pair, obj;

	RP_ASSERTARG(2);
	pair = RP_CAR(rk_eval_register[1]);
	obj = rk_eval_register[0];
	if (!RP_ISCONS(pair))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	RkWriteCell(&RP_CAR(pair), obj);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_setcdrbang_proc;
static rk_object
setcdrbang(void)
{
	rk_object pair, obj;

	RP_ASSERTARG(2);
	pair = RP_CAR(rk_eval_register[1]);
	obj = rk_eval_register[0];
	if (!RP_ISCONS(pair))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	RkWriteCell(&RP_CDR(pair), obj);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_cadrs_proc;
static rk_object
cadrs(void)
{
	rk_object obj;
	int l, n, i;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	n = RK_GETINUM(rk_eval_register[3]);
	l = (n >> 8);
	for (i = 0; i < l; ++i) {
		if (!RP_ISCONS(obj))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		if (!(n & (1<<i)))
			obj = RP_CAR(obj);
		else
			obj = RP_CDR(obj);
	}
	rk_eval_register[0] = obj;
	RP_RETURN();
}

static int
make_cadrs(void)
{
	rk_object *cp;
	char buf[8];
	int l, n, i;

	buf[0] = 'c';
	for (l = 2; l < 5; ++l) {
		buf[l+1] = 'r';
		for (n = 0; n < (1 << l); ++n) {
			for (i = 0; i < l; ++i)
				buf[l-i] = (n & (1<<i)) ? 'd' : 'a';
			if (!(rk_eval_register[0] = RkInternSymbol(buf, l+2))) {
				rk_valid_register = 0;
				return	0;
			}
			cp = RkAllocCells(4);
			cp[0] = (rk_object)&cp[2] | 3;
			cp[1] = rp_evlis_proc;
			cp[2] = rp_cadrs_proc;
			cp[3] = RK_MAKEINUM((l<<8)|n);
			RkWriteCell(&((rk_object *)rk_eval_register[0])[1], (rk_object)cp);
		}
	}
	return	1;
}

rk_object rp_listp_proc;
static rk_object
listp(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(1);
	obj1 = obj2 = rk_eval_register[0];
	while (RP_ISCONS(obj1)) {
		obj1 = RP_CDR(obj1);
		if (!RP_ISCONS(obj1))
			break;
		obj1 = RP_CDR(obj1);
		obj2 = RP_CDR(obj2);
		if (obj1 == obj2)
			break;
	}
	if (obj1 == RK_SOBJ_NIL)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_length_proc;
static rk_object
length(void)
{
	rk_object obj;
	int n;

	RP_ASSERTARG(1);
	obj = rk_eval_register[0];
	n = 0;
	while (RP_ISCONS(obj)) {
		obj = RP_CDR(obj);
		if (++n < 0)
			break;
	}
	if (obj != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_MAKEINUM(n);
	RP_RETURN();
}

rk_object rp_reverse_proc;
static rk_object
reverse(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	while (RP_ISCONS(rk_eval_register[0])) {
		cp = RkAllocCells(2);
		cp[0] = RP_CAR(rk_eval_register[0]);
		cp[1] = rk_eval_register[1];
		rk_eval_register[1] = (rk_object)cp;
		rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	}
	if (rk_eval_register[0] != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = rk_eval_register[1];
	RP_RETURN();
}

rk_object rp_listref_proc;
static rk_object
listref(void)
{
	rk_object obj;
	int n;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	if (!RK_ISINUM(rk_eval_register[0])
	 || (n = RK_GETINUM(rk_eval_register[0])) < 0
	 || !RP_ISCONS(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	while (n--) {
		obj = RP_CDR(obj);
		if (!RP_ISCONS(obj))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	}
	rk_eval_register[0] = RP_CAR(obj);
	RP_RETURN();
}

rk_object rp_memq_proc;
static rk_object
memq(void)
{
	rk_object obj, list;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	list = rk_eval_register[0];
	while (RP_ISCONS(list)) {
		if (obj == RP_CAR(list)) {
			rk_eval_register[0] = list;
			RP_RETURN();
		}
		list = RP_CDR(list);
	}
	if (list != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_assq_proc;
static rk_object
assq(void)
{
	rk_object obj, list, pair;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	list = rk_eval_register[0];
	while (RP_ISCONS(list)) {
		pair = RP_CAR(list);
		if (!RP_ISCONS(pair))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		if (obj == RP_CAR(pair)) {
			rk_eval_register[0] = pair;
			RP_RETURN();
		}
		list = RP_CDR(list);
	}
	if (list != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_assv_proc;
static rk_object
assv(void)
{
	rk_object obj, list, pair;

	RP_ASSERTARG(2);
	obj = RP_CAR(rk_eval_register[1]);
	list = rk_eval_register[0];
	while (RP_ISCONS(list)) {
		pair = RP_CAR(list);
		if (!RP_ISCONS(pair))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		if (RpEqvP(obj, RP_CAR(pair))) {
			rk_eval_register[0] = pair;
			RP_RETURN();
		}
		list = RP_CDR(list);
	}
	if (list != RK_SOBJ_NIL)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_sym2str_proc;
static rk_object
sym2str(void)
{
	RP_ASSERTARG(1);
	if (!RK_ISSYMBOL(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CAR(RP_CAR(rk_eval_register[0]) & ~7);
	RP_RETURN();
}

rk_object rp_str2sym_proc;
static rk_object
str2sym(void)
{
	rk_object str;
	unsigned len;
	char *s;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = (RP_CAR(str) >> 12);
	s = RkGetMallocObject(str);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (!(rk_eval_register[0] = RkInternSymbol(s, len)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	RP_RETURN();
}

rk_object rp_charp_proc;
static rk_object
charp(void)
{
	RP_ASSERTARG(1);
	if (RK_ISICHAR(rk_eval_register[0]))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_mkvect_proc;
static rk_object
mkvect(void)
{
	rk_object *cp, obj;
	int len, i, s;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		obj = RK_SOBJ_UNSPEC;
		if (!RK_ISINUM(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		len = RK_GETINUM(rk_eval_register[0]);
		break;
	case 2:
		obj = rk_eval_register[0];
		if (!RK_ISINUM(RP_CAR(rk_eval_register[1])))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		len = RK_GETINUM(RP_CAR(rk_eval_register[1]));
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[1] = obj;
	if (len < 0)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((s = create_vector(len)) == -1)
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (!s) {
		cp = &((rk_object *)rk_eval_register[0])[1];
		for (i = 0; i < len; ++i)
			cp[i] = rk_eval_register[1];
	} else {
		for (i = 0; i < len; ++i)
			RkWriteCell(&((rk_object *)rk_eval_register[0])[s + i], rk_eval_register[1]);
	}
	RP_RETURN();
}

rk_object rp_vectsetbang_proc;
static rk_object
vectsetbang(void)
{
	rk_object *cp;
	int len, n;

	RP_ASSERTARG(3);
	if (!RK_ISVECTOR(RP_CAR(RP_CDR(rk_eval_register[1]))) || !RK_ISINUM(RP_CAR(rk_eval_register[1])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = get_vectdata(RP_CAR(RP_CDR(rk_eval_register[1])), &len);
	n = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	if (n < 0 || n >= len)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	RkWriteCell(&cp[n], rk_eval_register[0]);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_procedurep_proc;
static rk_object
procedurep(void)
{
	RP_ASSERTARG(1);
	if (!(rk_eval_register[0] & 7) && (RP_CAR(rk_eval_register[0]) & 7) == 3)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_foreach_proc;
static rk_object foreach_conti1_proc;
static rk_object
foreach(void)
{
	int n, i;
	rk_object *cp, obj;

	if ((n = RK_GETINUM(rk_eval_register[2]) - 1) < 1)
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	if (rk_eval_register[0] == RK_SOBJ_NIL) {
		obj = rk_eval_register[1];
		for (i = 1; i < n; ++i) {
			if (RP_CAR(obj) != RK_SOBJ_NIL)
				RK_SIGNAL_ERROR1(RP_ISCONS(RP_CAR(obj)) ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			obj = RP_CDR(obj);
		}
		rk_eval_register[0] = RK_SOBJ_UNSPEC;
		RP_RETURN();
	}
	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(2);
	cp[0] = RP_CDR(rk_eval_register[0]);
	cp[1] = RK_SOBJ_NIL;
	rk_eval_register[3] = rk_eval_register[1];
	rk_eval_register[0] = RP_CAR(rk_eval_register[0]);
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = (rk_object)cp;
	if (n > 1) {
		if (!RP_ISCONS(RP_CAR(rk_eval_register[3])))
			RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[3]) == RK_SOBJ_NIL
					 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
		cp = RkAllocCells(4);
		cp[0] = RP_CDR(RP_CAR(rk_eval_register[3]));
		cp[1] = rk_eval_register[2];
		rk_eval_register[2] = (rk_object)cp;
		cp[2] = RP_CAR(RP_CAR(rk_eval_register[3]));
		cp[3] = RK_SOBJ_NIL;
		rk_eval_register[4] = rk_eval_register[1] = (rk_object)&cp[2];
		rk_valid_register = 5;
		rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
		for (i = 2; i < n; ++i) {
			if (!RP_ISCONS(RP_CAR(rk_eval_register[3])))
				RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[3]) == RK_SOBJ_NIL
						 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			cp = RkAllocCells(4);
			cp[0] = RP_CDR(RP_CAR(rk_eval_register[3]));
			cp[1] = rk_eval_register[2];
			rk_eval_register[2] = (rk_object)cp;
			cp[2] = RP_CAR(RP_CAR(rk_eval_register[3]));
			cp[3] = RK_SOBJ_NIL;
			RkWriteCell(&RP_CDR(rk_eval_register[4]), (rk_object)&cp[2]);
			rk_eval_register[4] = RP_CDR(rk_eval_register[4]);
			rk_eval_register[3] = RP_CDR(rk_eval_register[3]);
		}
	}
	obj = RP_CAR(rk_eval_register[3]);
	if ((obj & 7) || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[3] = RP_CAR(obj) & ~7;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = foreach_conti1_proc;
	cp[2] = RP_CAR(rk_eval_register[3]);
	cp[3] = RP_CDR(rk_eval_register[3]);
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[2] = RK_MAKEINUM(n);
	rk_eval_register[3] = rk_continuation[3];
	rk_valid_register = 4;
	return	rk_continuation[2];
}

static rk_object
foreach_conti1(void)
{
	int i;
	rk_object *cp, obj;

	if (RP_CAR(rk_continuation[4]) == RK_SOBJ_NIL) {
		obj = RP_CDR(rk_continuation[4]);
		while (obj != RK_SOBJ_NIL) {
			if (RP_CAR(obj) != RK_SOBJ_NIL)
				RK_SIGNAL_ERROR1(RP_ISCONS(RP_CAR(obj)) ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
			obj = RP_CDR(obj);
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		rk_eval_register[0] = RK_SOBJ_UNSPEC;
		RP_RETURN();
	}
	if (!RP_ISCONS(RP_CAR(rk_continuation[4])))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(2);
	cp[0] = RP_CDR(RP_CAR(rk_continuation[4]));
	cp[1] = RK_SOBJ_NIL;
	rk_eval_register[0] = RP_CAR(RP_CAR(rk_continuation[4]));
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RP_CDR(rk_continuation[4]);
	rk_eval_register[3] = rk_eval_register[4] = (rk_object)cp;
	rk_valid_register = 5;
	for (i = 1; rk_eval_register[2] != RK_SOBJ_NIL; ++i) {
		if (!RP_ISCONS(RP_CAR(rk_eval_register[2])))
			RK_SIGNAL_ERROR1(RP_CAR(rk_eval_register[2]) == RK_SOBJ_NIL
					 ? RP_ERROR_MAPLENGTH : RP_ERROR_ILLEGALARG);
		cp = RkAllocCells(4);
		cp[0] = rk_eval_register[0];
		cp[1] = rk_eval_register[1];
		rk_eval_register[1] = (rk_object)cp;
		rk_eval_register[0] = RP_CAR(RP_CAR(rk_eval_register[2]));
		cp[2] = RP_CDR(RP_CAR(rk_eval_register[2]));
		cp[3] = RK_SOBJ_NIL;
		RkWriteCell(&RP_CDR(rk_eval_register[4]), (rk_object)&cp[2]);
		rk_eval_register[4] = RP_CDR(rk_eval_register[4]);
		rk_eval_register[2] = RP_CDR(rk_eval_register[2]);
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = foreach_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_eval_register[3];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	rk_eval_register[2] = RK_MAKEINUM(i);
	rk_eval_register[3] = rk_continuation[3];
	rk_valid_register = 4;
	return	rk_continuation[2];
}

rk_object rp_cwcc_proc;
static rk_object docont_proc;
static rk_object
cwcc(void)
{
	rk_object *cp, proc;

	RP_ASSERTARG(1);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	proc = RP_CAR(RP_CAR(rk_eval_register[0]) & ~7);
	rk_eval_register[3] = RP_CDR(RP_CAR(rk_eval_register[0]) & ~7);
	cp = RkAllocCells(10);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = rp_evlis_proc;
	cp[2] = docont_proc;
	cp[3] = (rk_object)&cp[4];
	cp[4] = RK_VECTOR_TAG(6, 0);
	cp[5] = (rk_object)rp_dynamic_extent;
	cp[6] = (rk_object)rk_continuation;
	cp[7] = (rk_object)rk_error_catcher;
	cp[8] = rp_eval_fun;
	cp[9] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	return	proc;
}

static rk_object
docont(void)
{
	rk_valid_register = 3;
	return	RpWindTo(((rk_object *)rk_eval_register[3])[1], ((rk_object *)rk_eval_register[3])[2]
		       , ((rk_object *)rk_eval_register[3])[3], ((rk_object *)rk_eval_register[3])[4]);
}

rk_object rp_eofobjp_proc;
static rk_object
eofobjp(void)
{
	RP_ASSERTARG(1);
	if (rk_eval_register[0] == RK_SOBJ_EOF)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

static void (*p_handler[NSIG])(int);
static char pending_signal[NSIG];

static rk_object signal_conti1_proc;
static rk_object
signal_conti1(void)
{
	int i;
	rk_object *cp, proc;

	cp = (rk_object *)rk_continuation[3];
	rk_valid_register = (cp[0] >> 12) - 1;
	for (i = 0; i < rk_valid_register; ++i)
		rk_eval_register[i] = cp[i+1];
	proc = rk_continuation[2];
	rp_eval_car_proc = rk_continuation[4];
	rk_continuation = (rk_object *)rk_continuation[5];
	return	proc;
}

rk_object
RkHandleSignal(rk_object proc)
{
	int i, j;
	rk_object *cp, func;

	for (i = 0; i < NSIG; ++i)
		if (pending_signal[i]) {
			pending_signal[i] = 0;
			if (signal_handlers[i+1] == RK_DUMMY_OBJ)
				continue;
			for (j = i+1; j < NSIG && !pending_signal[j]; ++j) ;
			if (j == NSIG)
				rk_got_signal = 0;
			cp = RkAllocCells(6 + ((rk_valid_register + 2) & ~1));
			cp[0] = RK_VECTOR_TAG(6, 0);
			cp[1] = signal_conti1_proc;
			cp[2] = proc;
			cp[3] = (rk_object)&cp[6];
			cp[4] = rp_eval_car_proc;
			cp[5] = (rk_object)rk_continuation;
			cp[6] = RK_VECTOR_TAG(rk_valid_register + 1, 0);
			cp[6 + ((rk_valid_register + 2) & ~1) - 1] = RK_DUMMY_OBJ;
			for (j = 0; j < rk_valid_register; ++j)
				cp[j+7] = rk_eval_register[j];
			func = RP_CAR(signal_handlers[i+1]) & ~7;
			rk_continuation = cp;
			rk_eval_register[0] = RK_MAKEINUM(i);
			rk_eval_register[1] = RK_SOBJ_NIL;
			rk_eval_register[2] = RK_MAKEINUM(1);
			rk_eval_register[3] = RP_CDR(func);
			rk_valid_register = 4;
			rp_eval_car_proc = rp_default_eval_car_proc;
			return	RP_CAR(func);
		}
	rk_got_signal = 0;
	return	proc;
}

static void
handler(int sig)
{
#if defined(RK_OLD_SYSV_SIGNAL) || defined(WIN32) && !defined(__CYGWIN32__)
	signal(sig, handler);
#endif
	pending_signal[sig] = 1;
	rk_got_signal = 1;
	if (p_handler[sig] != (void (*)(int))-1 && p_handler[sig] != SIG_DFL && p_handler[sig] != SIG_IGN)
		(*p_handler[sig])(-1);
	if (rp_io_signal_catcher)
		longjmp(*rp_io_signal_catcher, 1);
}

rk_object rp_raisesig_proc;
static rk_object
raisesig(void)
{
	int signo;

	RP_ASSERTARG(1);
	if (!RK_ISINUM(rk_eval_register[0]) || (signo = RK_GETINUM(rk_eval_register[0])) < 0 || signo >= NSIG)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	pending_signal[signo] = 1;
	rk_got_signal = 1;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_sighandler_proc;
static rk_object
sighandler(void)
{
	int signo;
	rk_object func, ofunc;
	void (*ohandler)(int);

	RP_ASSERTARG(2);
	if (!RK_ISINUM(RP_CAR(rk_eval_register[1]))
	 || (signo = RK_GETINUM(RP_CAR(rk_eval_register[1]))) < 0 || signo >= NSIG)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	ofunc = signal_handlers[signo+1];
	switch (func = rk_eval_register[0]) {
	case RK_SOBJ_TRUE:
		if ((ohandler = p_handler[signo]) != (void (*)(int))-1)
			if ((ohandler = signal(signo, p_handler[signo])) == (void (*)(int))-1)
				RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
		break;
	default:
		if ((func & 7) || (RP_CAR(func) & 7) != 3)
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
#if !(defined(RK_OLD_SYSV_SIGNAL) || defined(GO32) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__))
		siginterrupt(signo, 1);
#endif
		/*FALLTRHU*/
	case RK_SOBJ_FALSE:
		if ((ohandler = signal(signo, func == RK_SOBJ_FALSE ? SIG_IGN : handler)) == (void (*)(int))-1)
			RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
		if (p_handler[signo] == (void (*)(int))-1)
			p_handler[signo] = ohandler;
		break;
	}
	rk_eval_register[0] = (ofunc == RK_DUMMY_OBJ ? ((ohandler == SIG_IGN) ? RK_SOBJ_FALSE : RK_SOBJ_TRUE) : ofunc);
	RkWriteCell(&signal_handlers[signo+1], (func == RK_SOBJ_TRUE || func == RK_SOBJ_FALSE) ? RK_DUMMY_OBJ : func);
	RP_RETURN();
}

#if !(defined(GO32) || defined(WIN32) || defined(__CYGWIN32__))
extern char const * const sys_siglist[];
#endif

rk_object rp_sigmess_proc;
static rk_object
sigmess(void)
{
	char *s;
	int n;

	RP_ASSERTARG(1);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	n = RK_GETINUM(rk_eval_register[0]);
#if !(defined(GO32) || defined(WIN32) || defined(__CYGWIN32__))
	//if (n < 0 || n >= NSIG)
		sprintf(mes_buf, "Unknown signal %d", n);
  //else
	//	sprintf(mes_buf, "%s", sys_siglist[n]);
#else
	sprintf(mes_buf, "Signal %d", n);
#endif
	n = strlen(mes_buf);
	if (!(s = malloc(n))) {
		RkScavenge(1);
		if (!(s = malloc(n)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	strncpy(s, mes_buf, n);
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(n, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	RP_RETURN();
}

rk_object rp_cmdarg_proc;
static rk_object
cmdarg(void)
{
	rk_object obj;
	int i, n;
	char *s;

	RP_ASSERTARG(0);
	if (cmd_args != (rk_object *)RK_DUMMY_OBJ) {
		rk_eval_register[0] = (rk_object)cmd_args;
		RP_RETURN();
	}
	cmd_args = RkAllocCells((rp_argc + 2) & ~1);
	cmd_args[0] = RK_VECTOR_TAG(rp_argc + 1, RK_TCODE_VECTOR);
	for (i = 1; i < ((rp_argc + 2) & ~1); ++i)
		cmd_args[i] = RK_DUMMY_OBJ;
	for (i = 0; i < rp_argc; ++i) {
		n = strlen(rp_argv[i]);
		if (!(s = malloc(n ? n : 4))) {
			RkScavenge(1);
			if (!(s = malloc(n ? n : 4))) {
				cmd_args = (rk_object *)RK_DUMMY_OBJ;
				RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
			}
		}
		if (n)
			strncpy(s, rp_argv[i], n);
		else
			*(unsigned long *)s = 0;
		if (!(obj = RkMakeMallocObject(RK_MALLOC_TAG(n, RK_TCODE_STRING), rk_plain_destructor, s))) {
			free(s);
			cmd_args = (rk_object *)RK_DUMMY_OBJ;
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
		}
		RkWriteCell(&cmd_args[i + 1], obj);
	}
	rk_eval_register[0] = (rk_object)cmd_args;
	RP_RETURN();
}

#if !(defined(WIN32) && !defined(__CYGWIN32__)) && !defined(__BEOS__)
static struct timeval epoch;
#elif defined(WIN32) && !defined(__CYGWIN32__)
static time_t epoch;
#else
static bigtime_t epoch;
#endif

rk_object rp_time_proc;
static rk_object
tttime(void)
{
#if !(defined(WIN32) && !defined(__CYGWIN32__)) && !defined(__BEOS__)
	struct timeval tp1;
	struct rusage resource;
#elif defined(__BEOS__)
	int32 cookie;
	thread_info info;
#endif
	double real, user, sys;
	rk_object *cp, obj;

	RP_ASSERTARG(0);
#if !(defined(WIN32) && !defined(__CYGWIN32__)) && !defined(__BEOS__)
	gettimeofday(&tp1, NULL);
	getrusage(RUSAGE_SELF, &resource);
	real = (double)(tp1.tv_sec - epoch.tv_sec) + (double)(tp1.tv_usec - epoch.tv_usec) / 1000000.0;
	user = (double)resource.ru_utime.tv_sec + (double)resource.ru_utime.tv_usec / 1000000.0;
	sys = (double)resource.ru_stime.tv_sec + (double)resource.ru_stime.tv_usec / 1000000.0;
#elif defined(WIN32) && !defined(__CYGWIN32__)
	real = (double)(time(NULL) - epoch);
	user = (double)clock() / CLK_TCK;
	sys = 0.0;
#else
	cookie = 0, user = sys = 0.0;
	while (get_next_thread_info(0, &cookie, &info) == B_OK) {
		user += (double)info.user_time;
		sys += (double)info.kernel_time;
	}
	real = (double)(system_time() - epoch) / 1000000.0;
	user /= 1000000.0;
	sys /= 1000000.0;
#endif
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_VECTOR);
	cp[1] = cp[2] = cp[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	obj = RkStoreFloat(real);
	RkWriteCell(&((rk_object *)rk_eval_register[0])[1], obj);
	obj = RkStoreFloat(user);
	RkWriteCell(&((rk_object *)rk_eval_register[0])[2], obj);
	obj = RkStoreFloat(sys);
	RkWriteCell(&((rk_object *)rk_eval_register[0])[3], obj);
	RP_RETURN();
}

rk_object rp_exit_proc;
static rk_object
eeexit(void)
{
	int exitcode;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		exitcode = 0;
		break;
	case 1:
		if (!RK_ISINUM(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		exitcode = RK_GETINUM(rk_eval_register[0]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
#ifndef __BEOS__
	exit(exitcode);
#else
	exit_thread(exitcode);
#endif
	/*NOTREACHED*/
}

rk_object rp_system_proc;
static rk_object
sssystem(void)
{
	rk_object str;
	unsigned len;
	char *s, *ss;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = (RP_CAR(str) >> 12);
	s = RkGetMallocObject(str);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (!(ss = malloc(len+1)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	strncpy(ss, s, len);
	ss[len] = '\0';
	rk_eval_register[0] = RK_MAKEINUM(system(ss));
	free(ss);
	RP_RETURN();
}

rk_object rp_getenv_proc;
static rk_object
gggetenv(void)
{
	rk_object str;
	unsigned len;
	char *s, *ss;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = (RP_CAR(str) >> 12);
	s = RkGetMallocObject(str);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (!(ss = malloc(len+1)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	strncpy(ss, s, len);
	ss[len] = '\0';
	s = getenv(ss);
	free(ss);
	if (!s)
		rk_eval_register[0] = RK_SOBJ_FALSE;
	else {
		len = strlen(s);
		if (!(ss = malloc(len ? len : 4)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
		if (len)
			strncpy(ss, s, len);
		else
			*(unsigned long *)ss = 0;
		if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(len, RK_TCODE_STRING)
								, rk_plain_destructor, ss))) {
			free(ss);
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
		}
	}
	RP_RETURN();
}

rk_object rp_setsymv_proc;
static rk_object
setsymv(void)
{
	rk_object sym;

	RP_ASSERTARG(2);
	sym = RP_CAR(rk_eval_register[1]);
	if (!RK_ISSYMBOL(sym))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	RkWriteCell(&((rk_object *)sym)[1], rk_eval_register[0]);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_getsymv_proc;
static rk_object
getsymv(void)
{
	rk_object sym, defv;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		sym = rk_eval_register[0];
		defv = RK_SOBJ_UNSPEC;
		break;
	case 2:
		sym = RP_CAR(rk_eval_register[1]);
		defv = rk_eval_register[0];
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISSYMBOL(sym))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((rk_eval_register[0] = ((rk_object *)sym)[1]) == RK_SOBJ_UNBOUND)
		rk_eval_register[0] = defv;
	RP_RETURN();
}

rk_object rp_symboundp_proc;
static rk_object
symboundp(void)
{
	RP_ASSERTARG(1);
	if (!RK_ISSYMBOL(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] =
		((((rk_object *)rk_eval_register[0])[1] != RK_SOBJ_UNBOUND) ? RK_SOBJ_TRUE : RK_SOBJ_FALSE);
	RP_RETURN();
}

rk_object rp_identity_proc;
static rk_object
identity(void)
{
	RP_ASSERTARG(1);
	RP_RETURN();
}

rk_object rp_synmark_proc;
static rk_object
synmark(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = RP_SOBJ_SYNMARK;
	RP_RETURN();
}

rk_object rp_setsymaux_proc;
static rk_object
setsymaux(void)
{
	rk_object sym;

	RP_ASSERTARG(2);
	sym = RP_CAR(rk_eval_register[1]);
	if (!RK_ISSYMBOL(sym))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	RkWriteCell(&((rk_object *)(RP_CAR(sym) & ~7))[1], rk_eval_register[0]);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_getsymaux_proc;
static rk_object
getsymaux(void)
{
	rk_object sym, defv;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		sym = rk_eval_register[0];
		defv = RK_SOBJ_UNSPEC;
		break;
	case 2:
		sym = RP_CAR(rk_eval_register[1]);
		defv = rk_eval_register[0];
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISSYMBOL(sym))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((rk_eval_register[0] = ((rk_object *)(RP_CAR(sym) & ~7))[1]) == RK_SOBJ_UNBOUND)
		rk_eval_register[0] = defv;
	RP_RETURN();
}

rk_object rp_symauxassp_proc;
static rk_object
symauxassp(void)
{
	RP_ASSERTARG(1);
	if (!RK_ISSYMBOL(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] =
		((((rk_object *)(RP_CAR(rk_eval_register[0]) & ~7))[1] != RK_SOBJ_UNBOUND) ?
			RK_SOBJ_TRUE : RK_SOBJ_FALSE);
	RP_RETURN();
}

rk_object rp_values_proc;
static rk_object
values(void)
{
	rk_valid_register = 3;
	RK_PROCEED();
}

rk_object rp_cwvals_proc;
static rk_object cwvals_conti1_proc;
static rk_object
cwvals(void)
{
	rk_object *cp, obj;

	RP_ASSERTARG(2);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	obj = RP_CAR(rk_eval_register[1]);
	if ((obj & 7) || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[3] = RP_CDR(RP_CAR(obj) & ~7);
	obj = RP_CAR(RP_CAR(obj) & ~7);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cwvals_conti1_proc;
	cp[2] = RP_CAR(rk_eval_register[0]) & ~7;
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	return	obj;
}

static rk_object
cwvals_conti1(void)
{
	rk_object proc;

	if (rk_valid_register != 3) {
		rk_eval_register[1] = RK_SOBJ_NIL;
		rk_eval_register[2] = RK_MAKEINUM(1);
	}
	rk_eval_register[3] = RP_CDR(rk_continuation[2]);
	rk_valid_register = 4;
	proc = RP_CAR(rk_continuation[2]);
	rk_continuation = (rk_object *)rk_continuation[3];
	return	proc;
}

rk_object rp_dynwind_proc;
static rk_object dynwind_conti1_proc, dynwind_conti2_proc, dynwind_conti3_proc;
static rk_object
dynwind(void)
{
	rk_object *cp, proc;

	RP_ASSERTARG(3);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CAR(rk_eval_register[0]) & ~7;
	rk_eval_register[2] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[2] & 7) || (RP_CAR(rk_eval_register[2]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[2] = RP_CAR(rk_eval_register[2]) & ~7;
	rk_eval_register[1] = RP_CAR(RP_CDR(rk_eval_register[1]));
	if ((rk_eval_register[1] & 7) || (RP_CAR(rk_eval_register[1]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]) & ~7;
	cp = RkAllocCells(14);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = dynwind_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rp_dynamic_extent;
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	cp[6] = RK_VECTOR_TAG(4, 0);
	cp[7] = cp[8] = cp[9] = RK_DUMMY_OBJ;
	cp[10] = RK_VECTOR_TAG(4, 0);
	cp[11] = rk_eval_register[1];
	cp[12] = rk_eval_register[0];
	cp[13] = (rk_object)rk_error_catcher;
	rk_continuation = cp;
	rk_eval_register[0] = (rk_object)&cp[6];
	rp_dynamic_extent[1] = RK_SOBJ_TRUE;
	RkWriteCell(&rp_dynamic_extent[2], (rk_object)&cp[10]);
	RkWriteCell(&rp_dynamic_extent[3], rk_eval_register[0]);
	rp_dynamic_extent = (rk_object *)rk_eval_register[0];
	rk_eval_register[3] = RP_CDR(rk_eval_register[1]);
	proc = RP_CAR(rk_eval_register[1]);
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	return	proc;
}

static rk_object
dynwind_conti1(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = dynwind_conti2_proc;
	cp[2] = rk_continuation[3];
	cp[3] = rk_continuation[4];
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(rk_continuation[2]);
	rk_valid_register = 4;
	proc = RP_CAR(rk_continuation[2]);
	rk_continuation = cp;
	return	proc;
}

static rk_object
dynwind_conti2(void)
{
	rk_object *cp, proc;

	rp_dynamic_extent[1] = RK_SOBJ_FALSE;
	RkWriteCell(&rp_dynamic_extent[2], ((rk_object *)rk_continuation[2])[2]);
	RkWriteCell(&rp_dynamic_extent[3], rk_continuation[2]);
	rp_dynamic_extent = (rk_object *)rk_continuation[2];
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = dynwind_conti3_proc;
	cp[2] = rk_eval_register[0];
	if (rk_valid_register != 3) {
		cp[3] = RK_SOBJ_NIL;
		cp[4] = RK_MAKEINUM(1);
	} else {
		cp[3] = rk_eval_register[1];
		cp[4] = rk_eval_register[2];
	}
	cp[5] = rk_continuation[3];
	rk_continuation = cp;
	proc = ((rk_object *)rp_dynamic_extent[2])[2];
	rp_dynamic_extent[2] = rp_dynamic_extent[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(proc);
	rk_valid_register = 4;
	return	RP_CAR(proc);
}

static rk_object
dynwind_conti3(void)
{
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = rk_continuation[3];
	rk_eval_register[2] = rk_continuation[4];
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[5];
	RK_PROCEED();
}

static rk_object windto_conti1_proc, windto_conti2_proc;
rk_object
RpWindTo(rk_object t_extent, rk_object t_conti, rk_object t_errc, rk_object t_evalf)
{
	int i;
	rk_object *cp, proc;

	if ((rk_object *)t_extent == rp_dynamic_extent) {
		rp_eval_car_proc = ((rp_eval_fun = t_evalf) == RK_SOBJ_FALSE)
					? rp_default_eval_car_proc : rp_reflect_eval_proc;
		rk_error_catcher = (rk_object *)t_errc;
		rk_continuation = (rk_object *)t_conti;
		RK_PROCEED();
	}
	rk_eval_register[rk_valid_register] = t_extent;
	rk_eval_register[rk_valid_register+1] = t_conti;
	rk_eval_register[rk_valid_register+2] = t_errc;
	rk_eval_register[rk_valid_register+3] = t_evalf;
	rk_valid_register += 4;
	cp = RkAllocCells((rk_valid_register & ~1) + 4);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = windto_conti2_proc;
	cp[2] = (rk_object)&cp[6];
	cp[3] = rk_eval_register[rk_valid_register-1];
	cp[4] = rk_eval_register[rk_valid_register-2];
	cp[5] = rk_eval_register[rk_valid_register-3];
	cp[6] = RK_VECTOR_TAG(rk_valid_register - 3, 0);
	cp[(rk_valid_register & ~1) + 3] = RK_DUMMY_OBJ;
	for (i = 0; i < rk_valid_register - 4; ++i)
		cp[7+i] = rk_eval_register[i];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[rk_valid_register-4];
	rk_valid_register = 1;
	while ((rk_object *)((rk_object *)rk_eval_register[0])[3] != rp_dynamic_extent) {
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = windto_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = (rk_object)rk_continuation;
		rk_continuation = cp;
		rk_eval_register[0] = ((rk_object *)rk_eval_register[0])[3];
	}
	if (((rk_object *)rk_eval_register[0])[1] == RK_SOBJ_TRUE)
		rp_dynamic_extent[1] = RK_SOBJ_FALSE;
	else
		rp_dynamic_extent[1] = RK_SOBJ_TRUE;
	RkWriteCell(&rp_dynamic_extent[2], ((rk_object *)rk_eval_register[0])[2]);
	RkWriteCell(&rp_dynamic_extent[3], rk_eval_register[0]);
	rp_dynamic_extent = (rk_object *)rk_eval_register[0];
	rp_eval_car_proc = rp_default_eval_car_proc;
	rp_eval_fun = RK_SOBJ_FALSE;
	rk_error_catcher = (rk_object *)((rk_object *)rp_dynamic_extent[2])[3];
	if (rp_dynamic_extent[1] == RK_SOBJ_TRUE)
		proc = ((rk_object *)rp_dynamic_extent[2])[2];
	else
		proc = ((rk_object *)rp_dynamic_extent[2])[1];
	rp_dynamic_extent[2] = rp_dynamic_extent[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(proc);
	rk_valid_register = 4;
	return	RP_CAR(proc);
}

static rk_object
windto_conti1(void)
{
	rk_object proc;

	if (((rk_object *)rk_continuation[2])[1] == RK_SOBJ_TRUE)
		rp_dynamic_extent[1] = RK_SOBJ_FALSE;
	else
		rp_dynamic_extent[1] = RK_SOBJ_TRUE;
	RkWriteCell(&rp_dynamic_extent[2], ((rk_object *)rk_continuation[2])[2]);
	RkWriteCell(&rp_dynamic_extent[3], rk_continuation[2]);
	rp_dynamic_extent = (rk_object *)rk_continuation[2];
	rk_error_catcher = (rk_object *)((rk_object *)rp_dynamic_extent[2])[3];
	rk_continuation = (rk_object *)rk_continuation[3];
	if (rp_dynamic_extent[1] == RK_SOBJ_TRUE)
		proc = ((rk_object *)rp_dynamic_extent[2])[2];
	else
		proc = ((rk_object *)rp_dynamic_extent[2])[1];
	rp_dynamic_extent[2] = rp_dynamic_extent[3] = RK_DUMMY_OBJ;
	rk_eval_register[0] = rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(proc);
	rk_valid_register = 4;
	return	RP_CAR(proc);
}

static rk_object
windto_conti2(void)
{
	int i;
	rk_object *cp;

	cp = (rk_object *)rk_continuation[2];
	rk_valid_register = (cp[0] >> 12) - 1;
	for (i = 0; i < rk_valid_register; ++i)
		rk_eval_register[i] = cp[i+1];
	rp_eval_car_proc = ((rp_eval_fun = rk_continuation[3]) == RK_SOBJ_FALSE)
				? rp_default_eval_car_proc : rp_reflect_eval_proc;
	rk_error_catcher = (rk_object *)rk_continuation[4];
	rk_continuation = (rk_object *)rk_continuation[5];
	RK_PROCEED();
}

rk_object rp_closedenv_proc;
static rk_object
closedenv(void)
{
	rk_object *cp;

	RP_ASSERTARG(2);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]);
	if (!RK_ISVECTOR(rk_eval_register[0]) || !RK_ISVECTOR(rk_eval_register[1])
	 || ((rk_object *)rk_eval_register[0])[0] != ((rk_object *)rk_eval_register[1])[0]
	 || (((rk_object *)rk_eval_register[0])[0] >> 12) == 0)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = rk_eval_register[0];
	cp[2] = rk_eval_register[1];
	cp[3] = RK_SOBJ_NIL;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

int
RpDefineSubr(char const *name, unsigned namelen, rk_object *proc, int index, rk_object (*func)(void))
{
	rk_object *cp;

	if (!(rk_eval_register[0] = RkInternSymbol(name, namelen))) {
		rk_valid_register = 0;
		return	0;
	}
	rk_valid_register = 1;
	cp = RkAllocCells(4);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = rp_evlis_proc;
	*proc = cp[2] = RkRegisterProcedure(index, func);
	cp[3] = RK_DUMMY_OBJ;
	RkWriteCell(&((rk_object *)rk_eval_register[0])[1], (rk_object)cp);
	return	1;
}

int
RpInitializeSubr(int index)
{
	int i;

	if (index != -1) {
#if !(defined(WIN32) && !defined(__CYGWIN32__)) && !defined(__BEOS__)
		gettimeofday(&epoch, NULL);
#elif defined(WIN32) && !defined(__CYGWIN32__)
		time(&epoch);
#else
		epoch = system_time();
#endif
		for (i = 0; i < NSIG; ++i)
			p_handler[i] = (void (*)(int))-1;
		rp_dynamic_extent = RkAllocCells(SIGNAL_VECTOR_SIZE+4);
		rp_dynamic_extent[0] = RK_VECTOR_TAG(4, 0);
		rp_dynamic_extent[1] = rp_dynamic_extent[2] = rp_dynamic_extent[3] = RK_DUMMY_OBJ;
		signal_handlers = rp_dynamic_extent + 4;
		signal_handlers[0] = RK_VECTOR_TAG(SIGNAL_VECTOR_SIZE, 0);
		for (i = 1; i < SIGNAL_VECTOR_SIZE; ++i)
			signal_handlers[i] = RK_DUMMY_OBJ;
		cmd_args = (rk_object *)RK_DUMMY_OBJ;
		p_traverse = rk_traverse_root;
		rk_traverse_root = traverse;
		RP_DEFINESUBR("rp:error", rp_error_proc, index + 0, error);
		RP_DEFINESUBR("rp:error-message", rp_errormess_proc, index + 1, errormess);
		RP_DEFINESUBR("car", rp_car_proc, index + 2, car);
		RP_DEFINESUBR("cdr", rp_cdr_proc, index + 3, cdr);
		RP_DEFINESUBR("cons", rp_cons_proc, index + 4, cons);
		RP_DEFINESUBR("symbol?", rp_symbolp_proc, index + 5, symbolp);
		RP_DEFINESUBR("list", rp_list_proc, index + 6, list);
		RP_DEFINESUBR("map", rp_map_proc, index + 7, map_func);
		map_conti1_proc = RkRegisterProcedure(index + 8, map_conti1);
		RP_DEFINESUBR("rp:void", rp_void_proc, index + 9, void_func);
		RP_DEFINESUBR("null?", rp_nullp_proc, index + 10, nullp);
		RP_DEFINESUBR("eq?", rp_eqp_proc, index + 11, eqp);
		RP_DEFINESUBR("rp:gensym", rp_gensym_proc, index + 12, gensym);
		RP_DEFINESUBR("pair?", rp_pairp_proc, index + 13, pairp);
		RP_DEFINESUBR("vector?", rp_vectorp_proc, index + 14, vectorp);
		RP_DEFINESUBR("vector-length", rp_vectlen_proc, index + 15, vectlen);
		RP_DEFINESUBR("vector-ref", rp_vectref_proc, index + 16, vectref);
		RP_DEFINESUBR("vector", rp_vector_proc, index + 17, vector);
		RP_DEFINESUBR("append", rp_append_proc, index + 18, append);
		RP_DEFINESUBR("apply", rp_apply_proc, index + 19, apply);
		RP_DEFINESUBR("memv", rp_memv_proc, index + 20, memv);
		RP_DEFINESUBR("not", rp_not_proc, index + 21, nnoott);
		RP_DEFINESUBR("boolean?", rp_boolenp_proc, index + 22, booleanp);
		RP_DEFINESUBR("eqv?", rp_eqvp_proc, index + 23, eqvp);
		RP_DEFINESUBR("string?", rp_stringp_proc, index + 24, stringp);
		RP_DEFINESUBR("set-car!", rp_setcarbang_proc, index + 25, setcarbang);
		RP_DEFINESUBR("set-cdr!", rp_setcdrbang_proc, index + 26, setcdrbang);
		rp_cadrs_proc = RkRegisterProcedure(index + 27, cadrs);
		if (!make_cadrs())
			return	-1;
		RP_DEFINESUBR("list?", rp_listp_proc, index + 28, listp);
		RP_DEFINESUBR("length", rp_length_proc, index + 29, length);
		RP_DEFINESUBR("reverse", rp_reverse_proc, index + 30, reverse);
		RP_DEFINESUBR("list-ref", rp_listref_proc, index + 31, listref);
		RP_DEFINESUBR("memq", rp_memq_proc, index + 32, memq);
		RP_DEFINESUBR("assq", rp_assq_proc, index + 33, assq);
		RP_DEFINESUBR("assv", rp_assv_proc, index + 34, assv);
		RP_DEFINESUBR("symbol->string", rp_sym2str_proc, index + 35, sym2str);
		RP_DEFINESUBR("string->symbol", rp_str2sym_proc, index + 36, str2sym);
		RP_DEFINESUBR("char?", rp_charp_proc, index + 37, charp);
		RP_DEFINESUBR("make-vector", rp_mkvect_proc, index + 38, mkvect);
		RP_DEFINESUBR("vector-set!", rp_vectsetbang_proc, index + 39, vectsetbang);
		RP_DEFINESUBR("procedure?", rp_procedurep_proc, index + 40, procedurep);
		RP_DEFINESUBR("for-each", rp_foreach_proc, index + 41, foreach);
		foreach_conti1_proc = RkRegisterProcedure(index + 42, foreach_conti1);
		RP_DEFINESUBR("call-with-current-continuation", rp_cwcc_proc, index + 43, cwcc);
		docont_proc = RkRegisterProcedure(index + 44, docont);
		RP_DEFINESUBR("eof-object?", rp_eofobjp_proc, index + 45, eofobjp);
		RP_DEFINESUBR("rp:set-signal-handler", rp_sighandler_proc, index + 46, sighandler);
		signal_conti1_proc = RkRegisterProcedure(index + 47, signal_conti1);
		RP_DEFINESUBR("rp:signal-message", rp_sigmess_proc, index + 48, sigmess);
		RP_DEFINESUBR("rp:command-line-arguments", rp_cmdarg_proc, index + 49, cmdarg);
		RP_DEFINESUBR("rp:time", rp_time_proc, index + 50, tttime);
		RP_DEFINESUBR("rp:exit", rp_exit_proc, index + 51, eeexit);
		RP_DEFINESUBR("rp:system", rp_system_proc, index + 52, sssystem);
		RP_DEFINESUBR("rp:getenv", rp_getenv_proc, index + 53, gggetenv);
		RP_DEFINESUBR("rp:symbol-value-set!", rp_setsymv_proc, index + 54, setsymv);
		RP_DEFINESUBR("rp:symbol-value", rp_getsymv_proc, index + 55, getsymv);
		RP_DEFINESUBR("rp:symbol-bound?", rp_symboundp_proc, index + 56, symboundp);
		RP_DEFINESUBR("rp:identity", rp_identity_proc, index + 57, identity);
		RP_DEFINESUBR("rp:raise-signal", rp_raisesig_proc, index + 58, raisesig);
		RP_DEFINESUBR("rp:syntax-mark", rp_synmark_proc, index + 59, synmark);
		RP_DEFINESUBR("rp:symbol-aux-datum-set!", rp_setsymaux_proc, index + 60, setsymaux);
		RP_DEFINESUBR("rp:symbol-aux-datum", rp_getsymaux_proc, index + 61, getsymaux);
		RP_DEFINESUBR("rp:symbol-aux-datum-assigned?", rp_symauxassp_proc, index + 62, symauxassp);
		RP_DEFINESUBR("values", rp_values_proc, index + 63, values);
		RP_DEFINESUBR("call-with-values", rp_cwvals_proc, index + 64, cwvals);
		cwvals_conti1_proc = RkRegisterProcedure(index + 65, cwvals_conti1);
		RP_DEFINESUBR("dynamic-wind", rp_dynwind_proc, index + 66, dynwind);
		dynwind_conti1_proc = RkRegisterProcedure(index + 67, dynwind_conti1);
		dynwind_conti2_proc = RkRegisterProcedure(index + 68, dynwind_conti2);
		dynwind_conti3_proc = RkRegisterProcedure(index + 69, dynwind_conti3);
		windto_conti1_proc = RkRegisterProcedure(index + 70, windto_conti1);
		windto_conti2_proc = RkRegisterProcedure(index + 71, windto_conti2);
		RP_DEFINESUBR("rp:make-closed-environment", rp_closedenv_proc, index + 72, closedenv);
		rk_valid_register = 0;
	}
	return  73;
}
