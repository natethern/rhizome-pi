/*
 * Copyright (c) 1996-99 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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
static char rcsid[] = "@(#)$Id: helper.c,v 1.9 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: helper.c,v $
 * Revision 1.9  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.8  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.7  1999/06/29 07:47:50  qfwfq
 * Check duplicate module loading.
 *
 * Revision 1.6  1998/07/31 11:06:39  qfwfq
 * Fasloader support
 *
 * Revision 1.5  1997/10/16 06:24:54  qfwfq
 * Release version 0.40
 *
 * Revision 1.4  1997/05/12 07:21:17  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1997/04/26 13:29:05  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.2  1996/10/10 08:26:56  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.1  1996/09/06 06:11:25  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 */

/*
 * Helper routines for compiled program.
 */
#include "rhiz_pi.h"

#include <string.h>

#define GETSYM(name)    (RkInternSymbol((name), sizeof(name)-1))

static rk_object apply_compiled_closure_r_proc, apply_compiled_closure_nr_proc;
rk_object rp_apply_object_proc;

static rk_object
apply_compiled_closure_r(void)
{
	rk_object *cp, *cp1, obj, proc;
	unsigned n, m, i, j, k;

	n = RK_GETINUM(rk_eval_register[2]);
	obj = rk_eval_register[3];
	proc = ((rk_object *)obj)[1];
	if (n < (i = RK_GETINUM(((rk_object *)obj)[2]) - 1))
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	m = ((i+4)&~1) + (n-i)*2;
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
	cp1 = &cp[j = (n-i)*2];
	cp[m - 1] = RK_DUMMY_OBJ;
	cp1[0] = RK_VECTOR_TAG(i+3, 0);
	cp1[1] = ((rk_object *)rk_eval_register[3])[3];
	if (j > 0) {
		cp[j-1] = rk_eval_register[2];
		cp[j-2] = rk_eval_register[0];
		obj = rk_eval_register[1];
		while ((j -= 2) > 0) {
			cp[j-1] = (rk_object)&cp[j];
			cp[j-2] = RP_CAR(obj);
			obj = RP_CDR(obj);
		}
		cp1[i+2] = (rk_object)cp;
		for (k = i; k > 0; --k) {
			cp1[k+1] = RP_CAR(obj);
			obj = RP_CDR(obj);
		}
	} else {
		cp1[i+2] = rk_eval_register[2];
		if (i > 0) {
			cp1[i+1] = rk_eval_register[0];
			obj = rk_eval_register[1];
			while (--i > 0) {
				cp1[i+1] = RP_CAR(obj);
				obj = RP_CDR(obj);
			}
		}
	}
	rk_eval_register[0] = (rk_object)cp1;
	rk_valid_register = 1;
	return	proc;
}

static rk_object
apply_compiled_closure_nr(void)
{
	rk_object *cp, obj, proc;
	unsigned n, i;

	n = RK_GETINUM(rk_eval_register[2]);
	obj = rk_eval_register[3];
	proc = ((rk_object *)obj)[1];
	if (n != RK_GETINUM(((rk_object *)obj)[2]))
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	cp = RkAllocCells((n+3)&~1);
	cp[0] = RK_VECTOR_TAG(n+2, 0);
	cp[((n+3)&~1) - 1] = RK_DUMMY_OBJ;
	cp[1] = ((rk_object *)rk_eval_register[3])[3];
	if (n > 0) {
		cp[n+1] = rk_eval_register[0];
		obj = rk_eval_register[1];
		for (i = n-1; i > 0; --i) {
			cp[i+1] = RP_CAR(obj);
			obj = RP_CDR(obj);
		}
	}
	rk_eval_register[0] = (rk_object)cp;
	rk_valid_register = 1;
	return	proc;
}

rk_object
RpMakeCompiledClosure(rk_object proc, int vno, int rest_p, rk_object env)
{
	rk_object *cp;

	rk_eval_register[rk_valid_register++] = env;
	cp = RkAllocCells(8);
	cp[0] = (rk_object)&cp[2] | 3;
	cp[1] = rp_evlis_proc;
	cp[2] = rest_p ? apply_compiled_closure_r_proc : apply_compiled_closure_nr_proc;
	cp[3] = (rk_object)&cp[4];
	cp[4] = RK_VECTOR_TAG(4, 0);
	cp[5] = proc;
	cp[6] = RK_MAKEINUM(vno);
	cp[7] = rk_eval_register[--rk_valid_register];
	return	(rk_object)cp;
}

static rk_object
apply_object(void)
{
	rk_object obj = rk_eval_register[3];

	if ((obj & 7) || (RP_CAR(obj) & 7) != 3)
		RK_SIGNAL_ERROR(RP_ERROR_ILLEGALAPPLY, obj);
	obj = RP_CAR(obj) & ~7;
	rk_eval_register[3] = RP_CDR(obj);
	return	RP_CAR(obj);
}

static struct RADESC { int size; rk_object *refarray; } *refarrays = NULL;
static rk_object fas_read_sym;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	int s, i, j;

	if (persistent_too && refarrays)
		for (i = 0; (s = refarrays[i].size) >= 0; ++i)
			for (j = 0; j < s; ++j)
				(*scan_fun)(&refarrays[i].refarray[j], cookie);
	(*scan_fun)(&fas_read_sym, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

static int
make_object(rk_object *dest, struct RP_OBJECT_DESC const *desc, rk_object rbase[])
{
	static unsigned counter = 0;
	rk_object obj;
	int n, m, i;
	char *s, buffer[16];

	switch (desc->rp_ocode) {
	case RP_OTAG_SYMBOL:
		if (!(obj = RkInternSymbol(((struct RP_STRING_DESC const *)desc->rp_odata)->rp_chars
						, ((struct RP_STRING_DESC const *)desc->rp_odata)->rp_length)))
			return	0;
		break;
	case RP_OTAG_GENSYM:
		sprintf(buffer, "#<C.%08X>", counter++);
		if (!(s = malloc(13))) {
			RkScavenge(1);
			if (!(s = malloc(13)))
				return	0;
		}
		memcpy(s, buffer, 13);
		if (!(obj = RkMakeMallocObject(RK_MALLOC_TAG(13, RK_TCODE_STRING), rk_plain_destructor, s))) {
			free(s);
			return	0;
		}
		RkWriteCell(dest, obj);
		obj = (rk_object)RkAllocCells(4);
		((rk_object *)obj)[0] = (rk_object)&((rk_object *)obj)[2] | 5;
		((rk_object *)obj)[1] = RK_SOBJ_UNBOUND;
		((rk_object *)obj)[2] = *dest;
		((rk_object *)obj)[3] = RK_SOBJ_UNBOUND;
		break;
	case RP_OTAG_PAIR:
		obj = (rk_object)RkAllocCells(2);
		((rk_object *)obj)[0] = rbase[((int const *)desc->rp_odata)[0]];
		((rk_object *)obj)[1] = rbase[((int const *)desc->rp_odata)[1]];
		break;
	case RP_OTAG_NULL:
		obj = RK_SOBJ_NIL;
		break;
	case RP_OTAG_STRING:
		n = m = i = ((struct RP_STRING_DESC const *)desc->rp_odata)->rp_length;
		if (n == 0 || n >= (1<<20)) {
			n += 4;
			i = 0;
		}
		if (!(s = malloc(n))) {
			RkScavenge(1);
			if (!(s = malloc(n)))
				return	0;
		}
		if (!(obj = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING), rk_plain_destructor, s)))  {
			free(s);
			return	0;
		}
		if (!i) {
			*(unsigned long *)s = m;
			s += 4;
		}
		memcpy(s, ((struct RP_STRING_DESC const *)desc->rp_odata)->rp_chars, m);
		break;
	case RP_OTAG_BOOLEAN:
		obj = (int)desc->rp_odata ? RK_SOBJ_TRUE : RK_SOBJ_FALSE;
		break;
	case RP_OTAG_CHARACTER:
		obj = RK_MAKEICHAR((int)desc->rp_odata);
		break;
	case RP_OTAG_VECTOR:
		n = ((int const *)desc->rp_odata)[0];
		if (n + 1 < RK_BULK_ALLOC_THRESHOLD) {
			m = (n+2)&~1;
			obj = (rk_object)RkAllocCells(m);
			((rk_object *)obj)[0] = RK_VECTOR_TAG(n+1, RK_TCODE_VECTOR);
			i = 1;
		} else if (n + 1 < (1<<20)) {
			m = n+1;
			if (!(obj = (rk_object)RkAllocVector(m)))
				return	0;
			((rk_object *)obj)[0] = RK_VECTOR_TAG(n+1, RK_TCODE_VECTOR);
			i = 1;
		} else {
			m = n+2;
			if (!(obj = (rk_object)RkAllocVector(m)))
				return	0;
			((rk_object *)obj)[0] = RK_VECTOR_TAG(0, RK_TCODE_VECTOR);
			((rk_object *)obj)[1] = RK_MAKEINUM(n+2);
			i = 2;
		}
		((rk_object *)obj)[m-1] = RK_DUMMY_OBJ;
		for (m = 1; m <= n; ++i, ++m)
			((rk_object *)obj)[i] = rbase[((int const *)desc->rp_odata)[m]];
		break;
	case RP_OTAG_SHORTINT:
		obj = RK_MAKEINUM((int)desc->rp_odata);
		break;
	case RP_OTAG_BIGINT:
		for (i = 1; ((int const *)desc->rp_odata)[i] != -1; ++i) ;
		obj = (rk_object)RkAllocCells((i+1)&~1);
		((rk_object *)obj)[0] = RK_VECTOR_TAG(i, ((int const *)desc->rp_odata)[0]
								? RK_TCODE_BIGINT_POS : RK_TCODE_BIGINT_NEG);
		((rk_object *)obj)[((i+1)&~1) - 1] = RK_DUMMY_OBJ;
		for (i = 1; (n = ((int const *)desc->rp_odata)[i]) != -1; ++i)
			((rk_object *)obj)[i] = (n << 2) | 2;
		break;
	case RP_OTAG_FRACTION:
		obj = (rk_object)RkAllocCells(4);
		((rk_object *)obj)[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
		((rk_object *)obj)[1] = RK_MAKEINUM(rbase[((int const *)desc->rp_odata)[0]] == RK_SOBJ_TRUE);
		((rk_object *)obj)[2] = rbase[((int const *)desc->rp_odata)[1]];
		((rk_object *)obj)[3] = rbase[((int const *)desc->rp_odata)[2]];
		break;
	case RP_OTAG_DBLFLOAT:
		obj = RkStoreFloat(((double const *)desc->rp_odata)[0]);
		break;
	case RP_OTAG_COMPLEX:
		obj = (rk_object)RkAllocCells(4);
		((rk_object *)obj)[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
		((rk_object *)obj)[1] = rbase[((int const *)desc->rp_odata)[0]];
		((rk_object *)obj)[2] = rbase[((int const *)desc->rp_odata)[1]];
		((rk_object *)obj)[3] = RK_DUMMY_OBJ;
		break;
	case RP_OTAG_SYNMARK:
		obj = RP_SOBJ_SYNMARK;
		break;
	}
	RkWriteCell(dest, obj);
	return	1;
}

int
RpCallInitProcs(int index, int nmodules, void (* const *initp)(struct RP_MODULE_INIT *))
{
	static int m_total = -1;
	static char msgbuf[128];
	struct RP_MODULE_INIT m;
	struct RADESC *ra, *new_refarrays;
	int n, i, j;

	if (index == -1) {
		n = 0;
		if (m_total == -1) {
			if (!(ra = refarrays = malloc(sizeof(struct RADESC) * (nmodules+1))))
				RkFatalAbort("Out of memory in initializing compiled modules.\n");
			m_total = 0;
		} else {
			if (!(new_refarrays = realloc(refarrays, sizeof(struct RADESC) * (m_total+nmodules+1))))
				return	-1;
			ra = &(refarrays = new_refarrays)[m_total];
		}
		for (i = 0; i < nmodules; ++i) {
			m.rp_version = 0;
			(*initp[i])(&m);
			if (m.rp_version != 1) {
				if (!m.rp_version)
					sprintf(msgbuf, "Compiler version mismatch.\n");
				else
					sprintf(msgbuf, "Compiler version mismatch in module %s.\n", m.rp_mname);
				RkFatalAbort(msgbuf);
			}
			if (*m.rp_loaded) {
				sprintf(msgbuf, "Module %s was loaded twice.\n", m.rp_mname);
				RkFatalAbort(msgbuf);
			}
			*m.rp_loaded = 1;
			n += m.rp_nprocs;
			for (j = 0; m.rp_odesc[j].rp_ocode >= 0; ++j)
				m.rp_oarray[j] = RK_DUMMY_OBJ;
			ra[i].size = j;
			ra[i].refarray = m.rp_oarray;
		}
		ra[i].size = -1;
		ra[i].refarray = NULL;
		return	n;
	}
	n = 0;
	for (i = 0; i < nmodules; ++i) {
		(*initp[i])(&m);
		for (j = 0; j < m.rp_nprocs; ++j)
			*m.rp_procs[j].rp_paddr = RkRegisterProcedure(index + n + j, m.rp_procs[j].rp_faddr);
		n += m.rp_nprocs;
		for (j = 0; m.rp_odesc[j].rp_ocode >= 0; ++j)
			if (!make_object(&m.rp_oarray[j], &m.rp_odesc[j], m.rp_oarray)) {
				refarrays[m_total].size = -1;
				refarrays[m_total].refarray = NULL;
				return	-1;
			}
	}
	m_total += nmodules;
	return	n;
}

rk_object rp_run_faslcode_proc;
static rk_object runfasl_conti1_proc, runfasl_doeval_proc, runfasl_error_proc, runfasl_getc_proc;
static rk_object runfasl_doeval_conti1_proc;

static rk_object
run_faslcode(void)
{
	rk_object *cp, proc;

	if ((rk_eval_register[1] = ((rk_object *)fas_read_sym)[1]) == RK_SOBJ_UNBOUND)
		RK_SIGNAL_ERROR(RP_ERROR_VARUNBOUND, fas_read_sym);
	rk_valid_register = 2;
	if ((rk_eval_register[1] & 7) || (RP_CAR(rk_eval_register[1]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[1] = RP_CAR(rk_eval_register[1]) & ~7;
	cp = RkAllocCells(20);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = runfasl_conti1_proc;
	cp[2] = (rk_object)&cp[12];
	cp[3] = (rk_object)&cp[4];
	cp[4] = (rk_object)&cp[8];
	cp[5] = (rk_object)&cp[6];
	cp[6] = rk_eval_register[0];
	cp[7] = RK_SOBJ_NIL;
	cp[8] = (rk_object)&cp[10] | 3;
	cp[9] = rp_evlis_proc;
	cp[10] = runfasl_doeval_proc;
	cp[11] = (rk_object)rk_continuation;
	cp[12] = (rk_object)&cp[14] | 3;
	cp[13] = rp_evlis_proc;
	cp[14] = runfasl_error_proc;
	cp[15] = RK_DUMMY_OBJ;
	cp[16] = (rk_object)&cp[18] | 3;
	cp[17] = rp_evlis_proc;
	cp[18] = runfasl_getc_proc;
	cp[19] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	proc = RP_CAR(rk_eval_register[1]);
	rk_eval_register[3] = RP_CDR(rk_eval_register[1]);
	rk_eval_register[2] = RK_MAKEINUM(4);
	rk_eval_register[1] = (rk_object)&cp[2];
	rk_eval_register[0] = (rk_object)&cp[16];
	rk_valid_register = 4;
	return	proc;
}

static rk_object
runfasl_conti1(void)
{
	RK_SIGNAL_ERROR1(RP_ERROR_EVALRET);
}

static rk_object
runfasl_doeval(void)
{
	rk_object *cp;

	RP_ASSERTARG(3);
	rk_eval_register[1] = RP_CAR(RP_CDR(rk_eval_register[1]));
	if (!RP_ISCONS(rk_eval_register[1]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
	if (!RP_ISCONS(rk_eval_register[1])) {
		rk_continuation = (rk_object *)rk_eval_register[3];
		rk_valid_register = 0;
		RK_PROCEED();
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = runfasl_doeval_conti1_proc;
	cp[2] = RP_CDR(rk_eval_register[1]);
	cp[3] = rk_eval_register[3];
	rk_continuation = cp;
	rk_eval_register[0] = RK_DUMMY_OBJ;
	rk_valid_register = 2;
	return  rp_eval_car_proc;
}

static rk_object
runfasl_doeval_conti1(void)
{
	rk_object *cp;

	if (!RP_ISCONS(rk_continuation[2])) {
		rk_continuation = (rk_object *)rk_continuation[3];
		rk_valid_register = 0;
		RK_PROCEED();
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = runfasl_doeval_conti1_proc;
	cp[2] = RP_CDR(rk_continuation[2]);
	cp[3] = rk_continuation[3];
	rk_eval_register[0] = RK_DUMMY_OBJ;
	rk_eval_register[1] = rk_continuation[2];
	rk_valid_register = 2;
	rk_continuation = cp;
	return  rp_eval_car_proc;
}

static rk_object
runfasl_error(void)
{
	rk_object obj;
	int err;

	RP_ASSERTARG(3);
	obj = RP_CAR(rk_eval_register[1]);
	err = ((rk_eval_register[1] = RP_CAR(RP_CDR(rk_eval_register[1]))) == RK_SOBJ_FALSE) ?
		RK_ERROR_READ_SYNTAX : RK_GETINUM(rk_eval_register[1]);
	RK_SIGNAL_ERROR(err, obj);
}

static rk_object
runfasl_getc(void)
{
	rk_object *cp, obj;
	char *p;

	RP_ASSERTARG(3);
	obj = RP_CAR(RP_CDR(rk_eval_register[1]));
	p = (char *)(RK_GETINUM(RP_CDR(obj)) << 16 | RK_GETINUM(RP_CAR(obj)));
	if (*p)
		obj = RK_MAKEICHAR(*p++);
	else {
		if (rk_eval_register[0] != RK_SOBJ_FALSE)
			RK_SIGNAL_ERROR1(RK_ERROR_PRMEOF);
		obj = RK_SOBJ_EOF;
	}
	rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RP_CAR(rk_eval_register[0]) & ~7;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = runfasl_conti1_proc;
	cp[2] = RK_MAKEINUM((unsigned long)p & 0xffff);
	cp[3] = RK_MAKEINUM((unsigned long)p >> 16);
	cp[4] = obj;
	cp[5] = RK_SOBJ_NIL;
	rk_continuation = cp;
	obj = RP_CAR(rk_eval_register[0]);
	rk_eval_register[3] = RP_CDR(rk_eval_register[0]);
	rk_eval_register[2] = RK_MAKEINUM(2);
	rk_eval_register[1] = (rk_object)&cp[4];
	rk_eval_register[0] = (rk_object)&cp[2];
	return	obj;
}

int
RpInitializeHelper(int index)
{
	if (index != -1) {
		if (!(fas_read_sym = GETSYM("rp:fas-read")))
			return	-1;
		p_traverse = rk_traverse_root;
		rk_traverse_root = traverse;
		apply_compiled_closure_r_proc = RkRegisterProcedure(index + 0, apply_compiled_closure_r);
		apply_compiled_closure_nr_proc = RkRegisterProcedure(index + 1, apply_compiled_closure_nr);
		rp_apply_object_proc = RkRegisterProcedure(index + 2, apply_object);
		rp_run_faslcode_proc = RkRegisterProcedure(index + 3, run_faslcode);
		runfasl_conti1_proc = RkRegisterProcedure(index + 4, runfasl_conti1);
		runfasl_doeval_proc = RkRegisterProcedure(index + 5, runfasl_doeval);
		runfasl_doeval_conti1_proc = RkRegisterProcedure(index + 6, runfasl_doeval_conti1);
		runfasl_error_proc = RkRegisterProcedure(index + 7, runfasl_error);
		runfasl_getc_proc = RkRegisterProcedure(index + 8, runfasl_getc);
	}
	return	9;
}
