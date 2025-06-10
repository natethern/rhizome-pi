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
static char rcsid[] = "@(#)$Id: runengin.c,v 1.5 2005/08/30 07:49:25 qfwfq Exp $";
#endif
/*
 * $Log: runengin.c,v $
 * Revision 1.5  2005/08/30 07:49:25  qfwfq
 * New make target install_embed, libraries for embedding into other apps.
 *
 * Revision 1.4  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.3  1997/10/16 06:24:38  qfwfq
 * Release version 0.40
 *
 * Revision 1.2  1996/09/06 06:08:35  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.1  1993/11/08 14:02:20  qfwfq
 * Initial revision
 *
 */

/*
 * Execution engine.
 */
#include "rhizome.h"

rk_object *rk_continuation, *rk_error_catcher, rk_eval_register[RK_EVAL_REGISTER_SIZE];
rk_object (**rk_procs_array)(void);
int rk_valid_register;
int volatile rk_got_signal;

static int nprocs;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	int i;

	(*scan_fun)((rk_object *)&rk_continuation, cookie);
	(*scan_fun)((rk_object *)&rk_error_catcher, cookie);
	for (i = 0; i < rk_valid_register; ++i)
		(*scan_fun)(&rk_eval_register[i], cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

void
RkInitializeRunEngine(int nmodules, int (* const initializor[])(int))
{
	int total, n, i;

	rk_continuation = rk_error_catcher = (rk_object *)RK_DUMMY_OBJ;
	rk_valid_register = 0;
	p_traverse = rk_traverse_root;
	rk_traverse_root = traverse;
	rk_got_signal = 0;
	total = 0;
	for (i = 0; i < nmodules; ++i)
		total += (*initializor[i])(-1);
	if (!(rk_procs_array = malloc((nprocs = total) * sizeof(rk_object (*)(void)))))
		RkFatalAbort("Failed to allocate procedure table.\n");
	total = 0;
	for (i = 0; i < nmodules; ++i) {
		if ((n = (*initializor[i])(total)) == -1)
			RkFatalAbort("Failure in a initialization procedure.\n");
		total += n;
	}
}

int
RkExtendProcsArray(int n)
{
	rk_object (**new_procs)(void);
	int old_nprocs;

	if (!(new_procs = realloc(rk_procs_array, (nprocs + n) * sizeof(rk_object (*)(void)))))
		return	-1;
	rk_procs_array = new_procs;
	old_nprocs = nprocs, nprocs += n;
	return	old_nprocs;
}

rk_object
RkRegisterProcedure(int index, rk_object (*proc)(void))
{
	rk_procs_array[index] = proc;
	return	(index << 4) | 4;
}

void RK_VOLATILE
RkExecute(rk_object init_proc)
{
	register rk_object procedure = init_proc;

	for (; ; ) {
		if (rk_got_signal)
			procedure = RkHandleSignal(procedure);
		procedure = (*rk_procs_array[procedure >> 4])();
	}
}
