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
static char rcsid[] = "@(#)$Id: boot.c,v 1.7 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: boot.c,v $
 * Revision 1.7  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.6  1999/06/29 07:47:50  qfwfq
 * Check duplicate module loading.
 *
 * Revision 1.5  1999/03/15 12:57:25  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.4  1999/02/15 08:37:14  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.3  1998/07/31 10:57:40  qfwfq
 * Adoption to Win32 GUI environment
 *
 * Revision 1.2  1997/04/26 13:29:04  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.1  1996/09/06 06:11:22  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 */

/*
 * Mock compiled module for bootstrap.
 */
#include "rhiz_pi.h"

#include <stdio.h>
#include <string.h>

RP_ARRAY_FORWARD_DECL rk_object rp_refarray[];

static rk_object rp_c_0;

static rk_object
RpC0(void)
{
	extern rk_object rp_load_proc;

	rk_eval_register[0] = rp_refarray[0];
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	return	rp_load_proc;
}

rk_object
RpCbootRun(void)
{
	return	rp_c_0;
}

static struct RP_PROCEDURE_REC const rp_c_procedure_rec[] = {
	RpC0, &rp_c_0,
	NULL, NULL
};

static struct RP_STRING_DESC rp_c_o_0[1];

static struct RP_OBJECT_DESC const rp_c_object_desc[] = {
	RP_OTAG_STRING, rp_c_o_0,
	-1, NULL
};

static rk_object rp_refarray[1];

static int loaded = 0;

void
RpCbootInit(struct RP_MODULE_INIT *p)
{
	p->rp_nprocs = sizeof(rp_c_procedure_rec)/sizeof(rp_c_procedure_rec[0]) - 1;
	p->rp_procs = rp_c_procedure_rec;
	p->rp_odesc = rp_c_object_desc;
	p->rp_oarray = rp_refarray;
	p->rp_version = 1;
	p->rp_loaded = &loaded;
	p->rp_mname = "BOOT";
}

RP_DLLEXPORT int const rp_in_windows_subsystem = 0;

static rk_object (* const run[])() = {RpCbootRun};
static void (* const init[])() = {RpCbootInit};

RP_DLLEXPORT struct RP_PROGRAM_DESC const *
RpProgramDesc(void)
{
	static struct RP_PROGRAM_DESC const prog_d = {1, run, init};

	if ((rp_argc -= 2) < 1) {
		fprintf(stderr, "usage: %s boot-program dir args...\n", rp_argv[0]);
		exit(2);
	}
	rp_c_o_0[0].rp_length = strlen((rp_argv += 2)[-1]);
	rp_c_o_0[0].rp_chars = rp_argv[-1];

	return	&prog_d;
}
