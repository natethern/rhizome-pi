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
static char rcsid[] = "@(#)$Id: read.c,v 1.10 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: read.c,v $
 * Revision 1.10  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.9  2005/08/30 07:49:25  qfwfq
 * New make target install_embed, libraries for embedding into other apps.
 *
 * Revision 1.8  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.7  2002/09/27 11:06:43  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.6  1999/02/15 08:08:52  qfwfq
 * make #\@ special subsequent (r5rs)
 *
 * Revision 1.5  1997/05/12 07:21:13  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.4  1996/10/10 08:26:47  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:08:34  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.2  1993/12/11 16:48:52  qfwfq
 * Make ascii SUB character be whitespace in MSDOS.
 *
 * Revision 1.1  93/11/08  14:02:19  qfwfq
 * Initial revision
 * 
 */

/*
 * Read of S-expression.
 */
#include "rhizome.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#ifdef RK_USE_LOCALE
#	include <wchar.h>
#endif
#if defined(WIN32) || defined(__CYGWIN32__)
#	include <windows.h>
#endif
#if defined(__CYGWIN32__)
WINAPI BOOL IsDBCSLeadByte(BYTE);
#endif

#ifndef MSDOS
#	define	XBLANK	""
#else
#	define	XBLANK	"\32"
#endif

#define BLANKCHARS	"\t\n\v\f\r" XBLANK
#define SPCINITS	"!$%&*/:<=>?~_^"
#define NUMPREFIXES	"bodxie"
#define MAXCHARNAME	16

rk_object rk_read_proc;
int rk_error_code;
rk_object rk_error_obj;

static rk_object read_conti1_proc, read_conti2_proc, read_conti3_proc;
static rk_object cp_illegal_conti1_proc, cp_whitespace_conti1_proc;
static rk_object cp_comment_conti1_proc, cp_comment_conti2_proc;
static rk_object cp_mesh_conti1_proc, cp_initial_conti1_proc, cp_initial_conti2_proc;
static rk_object cp_number_conti1_proc, cp_number_conti2_proc, cp_sign_conti1_proc, cp_dot_conti1_proc;
static rk_object cp_string_conti1_proc, cp_string_conti2_proc, cp_string_conti3_proc, cp_string_conti4_proc;
static rk_object cp_list_conti1_proc, cp_list_conti2_proc, cp_list_conti3_proc, cp_list_conti4_proc;
static rk_object cp_list_conti5_proc, cp_list_conti6_proc, cp_list_conti7_proc, cp_list_conti8_proc;
static rk_object mp_illegal_conti1_proc, mp_char_conti1_proc, mp_char_conti2_proc, mp_char_conti3_proc;
static rk_object mp_vector_conti1_proc, mp_vector_conti2_proc;
static rk_object error_conti1_proc;
static rk_object *char_proc_table, *mesh_proc_table;
static char char_flag_table[256];
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	(*scan_fun)(&rk_error_obj, cookie);
	(*scan_fun)((rk_object *)&char_proc_table, cookie);
	(*scan_fun)((rk_object *)&mesh_proc_table, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

int
RkIsDBCSLeadByte(int c)
{
#if defined(WIN32) || defined(__CYGWIN32__)
	return	IsDBCSLeadByte(c);
#elif defined(RK_USE_LOCALE)
	mbstate_t st;
	char s[1];

	memset(&st, 0, sizeof(st));
	s[0] = c;
	return	mbrlen(s, 1, &st) == (size_t)(-2);
#else
	return	0;
#endif
}

static void
set_synerr_msgobj(char *fmt, ...)
{
	static char msg_str[128];
	va_list ap;
	int nchar;
	char *s;

	va_start(ap, fmt);
	nchar = vsprintf(msg_str, fmt, ap);
	va_end(ap);
	if (!(s = malloc(nchar))) {
		rk_error_code = RK_ERROR_OUTOFSTORAGE;
		rk_error_obj = RK_SOBJ_UNSPEC;
		return;
	}
	memcpy(s, msg_str, nchar);
	if (!(rk_error_obj = RkMakeMallocObject(RK_MALLOC_TAG(nchar, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		rk_error_code = RK_ERROR_OUTOFSTORAGE;
		rk_error_obj = RK_SOBJ_UNSPEC;
		return;
	}
	rk_error_code = RK_ERROR_READ_SYNTAX;
}

static rk_object
sxp_read(void)
{
	rk_object *cp, proc;

	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = read_conti1_proc;
	cp[2] = (rk_object)&cp[4];
	cp[3] = (rk_object)rk_continuation;
	cp[4] = RK_VECTOR_TAG(4, 0);
	cp[5] = proc = rk_eval_register[0];
	cp[6] = rk_eval_register[1];
	cp[7] = rk_eval_register[2];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[3];
	rk_valid_register = 1;
	return	proc;
}

static rk_object
call_char_proc(rk_object opvec)
{
	rk_object *cp;

	rk_eval_register[2] = opvec;
	cp = (rk_object *)char_proc_table[RK_GETICHAR(rk_eval_register[0]) + 1];
	rk_eval_register[3] = cp[1];
	rk_valid_register = 4;
	return	cp[0];
}

static rk_object
read_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = read_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[2]);
}

static rk_object
read_conti2(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = read_conti3_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_continuation[2];
	cp[4] = rk_continuation[3];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	if (rk_eval_register[2] == RK_DUMMY_OBJ) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		RK_PROCEED();
	}
	rk_eval_register[0] = rk_eval_register[2];
	rk_valid_register = 2;
	return	((rk_object *)rk_continuation[3])[2];
}

static rk_object
read_conti3(void)
{
	rk_object *cp, proc;

	if (rk_continuation[2] == RK_SOBJ_RPAR || rk_continuation[2] == RK_SOBJ_DOT) {
		proc = ((rk_object *)rk_continuation[3])[3];
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = cp_illegal_conti1_proc;
		cp[2] = rk_continuation[2] == RK_SOBJ_RPAR ? RK_MAKEICHAR(')') : RK_MAKEICHAR('.');
		cp[3] = rk_continuation[4];
		rk_continuation = cp;
		return	proc;
	}
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = rk_continuation[2];
	rk_valid_register = 2;
	rk_continuation = (rk_object *)rk_continuation[4];
	RK_PROCEED();
}

static rk_object
cp_illegal(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_illegal_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_eval_register[2])[3];
}

static rk_object
cp_illegal_conti1(void)
{
	char cc[8], nn[20];
	int c;
	unsigned lnr;

	c = RK_GETICHAR(rk_continuation[2]);
	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (isascii(c) && isgraph(c))
		(void)sprintf(cc, "%c", c);
	else
		(void)sprintf(cc, "\\x%02X", c);
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Illegal token \"%s\"%s", cc, nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
cp_whitespace(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_whitespace_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
cp_whitespace_conti1(void)
{
	rk_object opvec;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	if (char_flag_table[RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_WHITESPACE) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[2])[1];
	}
	opvec = rk_continuation[2];
	rk_continuation = (rk_object *)rk_continuation[3];
	return	call_char_proc(opvec);
}

static rk_object
cp_comment(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_comment_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
cp_comment_conti1(void)
{
	int c;
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	c = RK_GETICHAR(rk_eval_register[0]);
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	if (c == '\n') {
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = cp_comment_conti2_proc;
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3];
		rk_continuation = cp;
	}
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
cp_comment_conti2(void)
{
	rk_object opvec;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	opvec = rk_continuation[2];
	rk_continuation = (rk_object *)rk_continuation[3];
	return	call_char_proc(opvec);
}

static rk_object
cp_mesh(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_mesh_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
cp_mesh_conti1(void)
{
	rk_object *pp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	rk_eval_register[2] = rk_continuation[2];
	pp = (rk_object *)mesh_proc_table[RK_GETICHAR(rk_eval_register[0]) + 1];
	rk_eval_register[3] = pp[1];
	rk_valid_register = 4;
	rk_continuation = (rk_object *)rk_continuation[3];
	return	pp[0];
}

static rk_object
cp_initial(void)
{
	int c;
	rk_object *cp, proc;

	c = RK_GETICHAR(rk_eval_register[0]);
	if (RkIsDBCSLeadByte(c)) {
		c |= 0x100;
		proc = cp_initial_conti2_proc;
	} else
		proc = cp_initial_conti1_proc;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = proc;
	cp[2] = RK_MAKEINUM(1);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	cp[6] = RK_MAKEINUM(c);
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_initial_conti1(void)
{
	unsigned len, i;
	char *s, *p;
	int c;
	rk_object *cp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION)) {
		if (rk_eval_register[0] == RK_SOBJ_EOF)
			rk_eval_register[2] = RK_DUMMY_OBJ;
		else
			rk_eval_register[2] = rk_eval_register[0];
		rk_valid_register = 3;
		len = RK_GETINUM(rk_continuation[2]);
		if (!(s = malloc(len))) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
			rk_continuation = (rk_object *)rk_continuation[5];
			RK_PROCEED();
		}
		cp = (rk_object *)rk_continuation[3];
		for (i = 0, p = s+len-1; i++ < len; cp = (rk_object *)cp[1]) {
			c = RK_GETINUM(cp[0]);
			if (isascii(c) && isupper(c))
				c = tolower(c);
			*p-- = c;
		}
		if (!(rk_eval_register[0] = RkInternSymbol(s, len))) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		free(s);
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	c = RK_GETICHAR(rk_eval_register[0]);
	if (RkIsDBCSLeadByte(c)) {
		c |= 0x100;
		proc = cp_initial_conti2_proc;
	} else
		proc = cp_initial_conti1_proc;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = RK_MAKEINUM(c);
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_initial_conti2(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF)
		return	cp_initial_conti1_proc;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_initial_conti1_proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = RK_MAKEINUM(RK_GETICHAR(rk_eval_register[0]) | 0x100);
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_digit(void)
{
	rk_object *cp;

	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_number_conti1_proc;
	cp[2] = RK_MAKEINUM(1);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_number_conti1(void)
{
	unsigned len, i;
	char *s, *p;
	int c;
	rk_object *cp, num;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[c = RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION) && c != '#') {
		if (rk_eval_register[0] == RK_SOBJ_EOF)
			rk_eval_register[2] = RK_DUMMY_OBJ;
		else
			rk_eval_register[2] = rk_eval_register[0];
		rk_valid_register = 3;
		len = RK_GETINUM(rk_continuation[2]);
		if (!(s = malloc(len))) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
			rk_continuation = (rk_object *)rk_continuation[5];
			RK_PROCEED();
		}
		cp = (rk_object *)rk_continuation[3];
		for (i = 0, p = s+len-1; i++ < len; cp = (rk_object *)cp[1]) {
			c = RK_GETICHAR(cp[0]);
			if (isascii(c) && isupper(c))
				c = tolower(c);
			*p-- = c;
		}
		if ((rk_eval_register[0] = RkReadNumber(s, len, 10)) == RK_SOBJ_FALSE) {
			free(s);
			num = ((rk_object *)rk_continuation[4])[3];
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, 0);
			cp[1] = cp_number_conti2_proc;
			cp[2] = rk_eval_register[2];
			cp[3] = rk_continuation[5];
			rk_continuation = cp;
			rk_eval_register[0] = rk_eval_register[1];
			rk_valid_register = 1;
			return	num;
		}
		free(s);
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_number_conti1_proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_number_conti2(void)
{
	char nn[20];
	unsigned lnr;

	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Illegal number notation%s", nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = rk_continuation[2];
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
cp_sign(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_sign_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_eval_register[2];
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[3])[1];
}

static void
sign_sym(void)
{
	char s[1];

	s[0] = RK_GETICHAR(rk_continuation[2]);
	if (!(rk_eval_register[0] = RkInternSymbol(s, 1))) {
		rk_eval_register[0] = RK_SOBJ_ERROR;
		rk_error_code = RK_ERROR_OUTOFSTORAGE;
		rk_error_obj = RK_SOBJ_UNSPEC;
	}
}

static rk_object
cp_sign_conti1(void)
{
	rk_object *cp;
	int c;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[c = RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION) && c != '#') {
		if (rk_eval_register[0] == RK_SOBJ_EOF)
			rk_eval_register[2] = RK_DUMMY_OBJ;
		else
			rk_eval_register[2] = rk_eval_register[0];
		rk_valid_register = 3;
		sign_sym();
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	cp = RkAllocCells(10);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_number_conti1_proc;
	cp[2] = RK_MAKEINUM(2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[3];
	cp[5] = rk_continuation[4];
	cp[6] = rk_eval_register[0];
	cp[7] = (rk_object)&cp[8];
	cp[8] = rk_continuation[2];
	cp[9] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_dot(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_dot_conti1_proc;
	cp[2] = RK_MAKEINUM(1);
	cp[3] = rk_eval_register[2];
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[3])[1];
}

static rk_object
cp_dot_conti1(void)
{
	rk_object *cp, proc;
	int c, n, i;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[c = RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION) && c != '#') {
		if (rk_eval_register[0] == RK_SOBJ_EOF)
			rk_eval_register[2] = RK_DUMMY_OBJ;
		else
			rk_eval_register[2] = rk_eval_register[0];
		rk_valid_register = 3;
		switch (RK_GETINUM(rk_continuation[2])) {
		case 1:
			rk_eval_register[0] = RK_SOBJ_DOT;
			break;
		case 3:
			if (!(rk_eval_register[0] = RkInternSymbol("...", 3))) {
				rk_eval_register[0] = RK_SOBJ_ERROR;
				rk_error_code = RK_ERROR_OUTOFSTORAGE;
				rk_error_obj = RK_SOBJ_UNSPEC;
			}
			break;
		default:
			proc = ((rk_object *)rk_continuation[3])[3];
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, 0);
			cp[1] = cp_number_conti2_proc;
			cp[2] = rk_eval_register[2];
			cp[3] = rk_continuation[4];
			rk_continuation = cp;
			rk_eval_register[0] = rk_eval_register[1];
			rk_valid_register = 1;
			return	proc;
		}
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	if (c == '.' && RK_GETINUM(rk_continuation[2]) < 3) {
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = cp_dot_conti1_proc;
		cp[2] = rk_continuation[2] + (1<<2);
		cp[3] = rk_continuation[3];
		cp[4] = rk_continuation[4];
		cp[5] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[3])[1];
	}
	n = RK_GETINUM(rk_continuation[2]);
	cp = RkAllocCells(8 + n*2);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_number_conti1_proc;
	cp[2] = RK_MAKEINUM(n + 1);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[3];
	cp[5] = rk_continuation[4];
	cp[6] = rk_eval_register[0];
	for (i = 0; i < n; ++i) {
		cp[7 + i*2] = (rk_object)&cp[8 + i*2];
		cp[8 + i*2] = RK_MAKEICHAR('.');
	}
	cp[7 + n*2] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_dquote(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_string_conti1_proc;
	cp[2] = RK_MAKEINUM(0);
	cp[3] = RK_DUMMY_OBJ;
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_string_conti1(void)
{
	unsigned len, len0, i;
	char *s, *p;
	int c;
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if ((c = RK_GETICHAR(rk_eval_register[0])) == '"') {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		i = len = len0 = RK_GETINUM(rk_continuation[2]);
		if (len0 == 0 || len0 >= (1<<20)) {
			len += 4;
			i = 0;
		}
		if (!(s = malloc(len))) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
			rk_continuation = (rk_object *)rk_continuation[5];
			RK_PROCEED();
		}
		if (!i)
			((unsigned long *)s)[0] = len0;
		p = s + len - 1;
		len = i;
		cp = (rk_object *)rk_continuation[3];
		for (i = 0; i++ < len0; cp = (rk_object *)cp[1])
			*p-- = RK_GETICHAR(cp[0]);
		if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(len, RK_TCODE_STRING)
								, rk_plain_destructor, s))) {
			free(s);
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (c == '\\') {
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = cp_string_conti2_proc;
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3];
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
	} else {
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = RkIsDBCSLeadByte(c) ? cp_string_conti4_proc : cp_string_conti1_proc;
		cp[2] = rk_continuation[2] + (1<<2);
		cp[3] = (rk_object)&cp[6];
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
		cp[6] = rk_eval_register[0];
		cp[7] = rk_continuation[3];
	}
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_string_conti2(void)
{
	rk_object *cp, proc;
	int c;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if ((c = RK_GETICHAR(rk_eval_register[0])) != '"' && c != '\\') {
		proc = ((rk_object *)rk_continuation[4])[3];
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = cp_string_conti3_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = rk_continuation[5];
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_string_conti1_proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_string_conti3(void)
{
	char cc[8], nn[20];
	int c;
	unsigned lnr;

	c = RK_GETICHAR(rk_continuation[2]);
	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (isascii(c) && isgraph(c))
		(void)sprintf(cc, "%c", c);
	else
		(void)sprintf(cc, "\\x%02X", c);
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Illegal escape sequence \"\\%s\"%s", cc, nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
cp_string_conti4(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF)
		return	cp_string_conti1_proc;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_string_conti1_proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
cp_parenth(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_list_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
cp_list_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = cp_list_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[2]);
}

static rk_object
cp_list_conti2(void)
{
	rk_object *cp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF
	 || rk_eval_register[0] == RK_SOBJ_RPAR) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		} else if (rk_eval_register[0] == RK_SOBJ_RPAR)
			rk_eval_register[0] = RK_SOBJ_NIL;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_DOT) {
		proc = ((rk_object *)rk_continuation[2])[3];
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = error_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = rk_eval_register[2];
		cp[4] = rk_continuation[3];
		cp[5] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = (rk_eval_register[2] == RK_DUMMY_OBJ ? cp_list_conti3_proc : cp_list_conti4_proc);
	cp[2] = (rk_object)&cp[6];
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[2];
	cp[5] = rk_continuation[3];
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	if (rk_eval_register[2] == RK_DUMMY_OBJ) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[4])[1];
	}
	rk_eval_register[0] = rk_eval_register[2];
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
cp_list_conti3(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_list_conti4_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
cp_list_conti4(void)
{
	rk_object *cp, *tail;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF
	 || rk_eval_register[0] == RK_SOBJ_RPAR) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		} else if (rk_eval_register[0] == RK_SOBJ_RPAR) {
			((rk_object *)rk_continuation[3])[1] = RK_SOBJ_NIL;
			rk_eval_register[0] = rk_continuation[2];
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_DOT) {
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = (rk_eval_register[2] == RK_DUMMY_OBJ ? cp_list_conti5_proc : cp_list_conti6_proc);
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3];
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
		rk_continuation = cp;
		if (rk_eval_register[2] == RK_DUMMY_OBJ) {
			rk_eval_register[0] = rk_eval_register[1];
			rk_valid_register = 1;
			return	((rk_object *)rk_continuation[4])[1];
		}
		rk_eval_register[0] = rk_eval_register[2];
		return	call_char_proc(rk_continuation[4]);
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = (rk_eval_register[2] == RK_DUMMY_OBJ ? cp_list_conti3_proc : cp_list_conti4_proc);
	cp[2] = rk_continuation[2];
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	tail = (rk_object *)rk_continuation[3];
	rk_continuation = cp;
	RkWriteCell(&tail[1], rk_continuation[3]);
	if (rk_eval_register[2] == RK_DUMMY_OBJ) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[4])[1];
	}
	rk_eval_register[0] = rk_eval_register[2];
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
cp_list_conti5(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_list_conti6_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
cp_list_conti6(void)
{
	rk_object *cp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_RPAR || rk_eval_register[0] == RK_SOBJ_DOT) {
		proc = ((rk_object *)rk_continuation[4])[3];
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = error_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = rk_eval_register[2];
		cp[4] = rk_continuation[5];
		cp[5] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	proc;
	}
	RkWriteCell(&((rk_object *)rk_continuation[3])[1], rk_eval_register[0]);
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = (rk_eval_register[2] == RK_DUMMY_OBJ ? cp_list_conti7_proc : cp_list_conti8_proc);
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[4];
	cp[4] = rk_continuation[5];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	if (rk_eval_register[2] == RK_DUMMY_OBJ) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[3])[1];
	}
	rk_eval_register[0] = rk_eval_register[2];
	return	call_char_proc(rk_continuation[3]);
}

static rk_object
cp_list_conti7(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_list_conti8_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[3]);
}

static rk_object
cp_list_conti8(void)
{
	rk_object *cp, *pp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF
	 || rk_eval_register[0] == RK_SOBJ_RPAR) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		} else if (rk_eval_register[0] == RK_SOBJ_RPAR)
			rk_eval_register[0] = rk_continuation[2];
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	proc = ((rk_object *)rk_continuation[3])[3];
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = error_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_eval_register[2];
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	proc;
}

static rk_object
cp_rpar(void)
{
	rk_eval_register[0] = RK_SOBJ_RPAR;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	RK_PROCEED();
}

static rk_object
mp_illegal(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = mp_illegal_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_eval_register[2])[3];
}

static rk_object
mp_illegal_conti1(void)
{
	char cc[8], nn[20];
	int c;
	unsigned lnr;

	c = RK_GETICHAR(rk_continuation[2]);
	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (isascii(c) && isgraph(c))
		(void)sprintf(cc, "%c", c);
	else
		(void)sprintf(cc, "\\x%02X", c);
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Illegal token \"#%s\"%s", cc, nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
mp_boolean(void)
{
	int c;

	rk_eval_register[0] = (c = RK_GETICHAR(rk_eval_register[0])) == 'f' || c == 'F' ? RK_SOBJ_FALSE : RK_SOBJ_TRUE;
	rk_eval_register[2] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	RK_PROCEED();
}

static rk_object
mp_char(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = mp_char_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[2])[1];
}

static rk_object
mp_char_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION)) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[3];
		RK_PROCEED();
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = mp_char_conti2_proc;
	cp[2] = RK_MAKEINUM(1);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[2];
	cp[5] = rk_continuation[3];
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
mp_char_conti2(void)
{
	static struct {char name[MAXCHARNAME]; rk_object cobj;} table[] = {
		{"space", RK_MAKEICHAR(' ')},
		{"newline", RK_MAKEICHAR('\n')},
	};
	char name[MAXCHARNAME];
	unsigned len, i;
	int c;
	rk_object *cp, proc;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_EOF
	 || (char_flag_table[RK_GETICHAR(rk_eval_register[0])] & RK_CFLAG_PUNCTUATION)) {
		if (rk_eval_register[0] == RK_SOBJ_EOF)
			rk_eval_register[2] = RK_DUMMY_OBJ;
		else
			rk_eval_register[2] = rk_eval_register[0];
		rk_valid_register = 3;
		if ((len = RK_GETINUM(rk_continuation[2])) == 1) {
			rk_eval_register[0] = ((rk_object *)rk_continuation[3])[0];
			rk_continuation = (rk_object *)rk_continuation[5];
			RK_PROCEED();
		}
		if (len < MAXCHARNAME) {
			name[len] = '\0';
			cp = (rk_object *)rk_continuation[3];
			for (i = 0; i < len; cp = (rk_object *)cp[1]) {
				c = RK_GETICHAR(cp[0]);
				if (isascii(c) && isupper(c))
					c = tolower(c);
				name[len - (++i)] = c;
			}
			for (i = 0; i < sizeof table/sizeof table[1]; ++i)
				if (!strcmp(table[i].name, name)) {
					rk_eval_register[0] = table[i].cobj;
					rk_continuation = (rk_object *)rk_continuation[5];
					RK_PROCEED();
				}
		}
		proc = ((rk_object *)rk_continuation[4])[3];
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = mp_char_conti3_proc;
		cp[2] = rk_eval_register[2];
		cp[3] = rk_continuation[5];
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = mp_char_conti2_proc;
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
mp_char_conti3(void)
{
	char nn[20];
	unsigned lnr;

	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Unknown character name%s", nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = rk_continuation[2];
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[3];
	RK_PROCEED();
}

static rk_object
mp_vector(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = mp_vector_conti1_proc;
	cp[2] = RK_MAKEINUM(0);
	cp[3] = RK_DUMMY_OBJ;
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
mp_vector_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_eval_register[2] = RK_DUMMY_OBJ;
		rk_valid_register = 3;
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = mp_vector_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
mp_vector_conti2(void)
{
	rk_object *cp, *pp, proc;
	unsigned len, i, s;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		if (rk_eval_register[0] == RK_SOBJ_EOF) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_PRMEOF;
			rk_error_obj = RK_SOBJ_UNSPEC;
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_RPAR) {
		if ((len = RK_GETINUM(rk_continuation[2]))+1 < RK_BULK_ALLOC_THRESHOLD) {
			cp = RkAllocCells((len+2)&~1);
			cp[0] = RK_VECTOR_TAG(len+1, RK_TCODE_VECTOR);
			cp[((len+2)&~1) - 1] = RK_DUMMY_OBJ;
			s = 1;
			pp = (rk_object *)rk_continuation[3];
			for (i = 0; i < len; ) {
				cp[len + s - (++i)] = pp[0];
				pp = (rk_object *)pp[1];
			}
			rk_eval_register[0] = (rk_object)cp;
		} else {
			if (!(cp = RkAllocVector(len+1 + (len+1 >= (1<<20))))) {
				rk_eval_register[0] = RK_SOBJ_ERROR;
				rk_error_code = RK_ERROR_OUTOFSTORAGE;
				rk_error_obj = RK_SOBJ_UNSPEC;
				rk_continuation = (rk_object *)rk_continuation[5];
				RK_PROCEED();
			}
			if (len+1 >= (1<<20)) {
				cp[0] = RK_VECTOR_TAG(0, RK_TCODE_VECTOR);
				cp[1] = RK_MAKEINUM(len+2);
				s = 2;
			} else {
				cp[0] = RK_VECTOR_TAG(len+1, RK_TCODE_VECTOR);
				s = 1;
			}
			for (i = 0; i < len; ++i)
				cp[i + s] = RK_DUMMY_OBJ;
			rk_eval_register[0] = (rk_object)cp;
			for (i = 0; i < len; ) {
				RkWriteCell(&((rk_object *)rk_eval_register[0])[len + s - (++i)]
						, ((rk_object *)rk_continuation[3])[0]);
				RkWriteCell(&rk_continuation[3], ((rk_object *)rk_continuation[3])[1]);
			}
		}
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (rk_eval_register[0] == RK_SOBJ_DOT) {
		proc = ((rk_object *)rk_continuation[4])[3];
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = error_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = rk_eval_register[2];
		cp[4] = rk_continuation[5];
		cp[5] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = (rk_eval_register[2] == RK_DUMMY_OBJ ? mp_vector_conti1_proc : mp_vector_conti2_proc);
	cp[2] = rk_continuation[2] + (1<<2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_eval_register[0];
	cp[7] = rk_continuation[3];
	rk_continuation = cp;
	if (rk_eval_register[2] == RK_DUMMY_OBJ) {
		rk_eval_register[0] = rk_eval_register[1];
		rk_valid_register = 1;
		return	((rk_object *)rk_continuation[4])[1];
	}
	rk_eval_register[0] = rk_eval_register[2];
	return	call_char_proc(rk_continuation[4]);
}

static rk_object
mp_numprefix(void)
{
	rk_object *cp;

	cp = RkAllocCells(10);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = cp_number_conti1_proc;
	cp[2] = RK_MAKEINUM(2);
	cp[3] = (rk_object)&cp[6];
	cp[4] = rk_eval_register[2];
	cp[5] = (rk_object)rk_continuation;
	cp[6] = rk_eval_register[0];
	cp[7] = (rk_object)&cp[8];
	cp[8] = RK_MAKEICHAR('#');
	cp[9] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	return	((rk_object *)rk_continuation[4])[1];
}

static rk_object
error_conti1(void)
{
	char *cc, nn[20];
	unsigned lnr;

	switch (rk_continuation[2]) {
	case RK_SOBJ_RPAR:	cc = "\")\"";				break;
	case RK_SOBJ_DOT:	cc = "\".\"";				break;
	default:		cc = "(anything other than \")\")";	break;
	}
	lnr = (unsigned)RK_GETINUM(rk_eval_register[0]) & 0x3fffffff;
	if (lnr)
		(void)sprintf(nn, " at line %d", lnr);
	else
		nn[0] = '\0';
	set_synerr_msgobj("Illegal token %s%s", cc, nn);
	rk_eval_register[0] = RK_SOBJ_ERROR;
	rk_eval_register[2] = rk_continuation[3];
	rk_valid_register = 3;
	rk_continuation = (rk_object *)rk_continuation[4];
	RK_PROCEED();
}

void
RkMakeCharTableEntry(int ccode, rk_object *entry, int flag)
{
	RkWriteCell(&char_proc_table[ccode+1], (rk_object)entry);
	char_flag_table[ccode] = flag;
}

static void
make_char_table_entry(int ccode, int index, rk_object (*proc)(void), int flag)
{
	rk_object *cell;

	cell = RkAllocCells(2);
	cell[0] = RkRegisterProcedure(index, proc);
	cell[1] = RK_DUMMY_OBJ;
	RkWriteCell(&char_proc_table[ccode+1], (rk_object)cell);
	char_flag_table[ccode] = flag;
}

void
RkCopyCharTableEntry(int src, int dst)
{
	RkWriteCell(&char_proc_table[dst+1], char_proc_table[src+1]);
	char_flag_table[dst] = char_flag_table[src];
}

void
RkMakeMeshTableEntry(int ccode, rk_object *entry)
{
	RkWriteCell(&mesh_proc_table[ccode+1], (rk_object)entry);
}

static void
make_mesh_table_entry(int ccode, int index, rk_object (*proc)(void))
{
	rk_object *cell;

	cell = RkAllocCells(2);
	cell[0] = RkRegisterProcedure(index, proc);
	cell[1] = RK_DUMMY_OBJ;
	RkWriteCell(&mesh_proc_table[ccode+1], (rk_object)cell);
}

void
RkCopyMeshTableEntry(int src, int dst)
{
	RkWriteCell(&mesh_proc_table[dst+1], mesh_proc_table[src+1]);
}

int
RkInitializeRead(int index)
{
	int i;

	if (index != -1) {
		rk_error_obj = RK_SOBJ_UNSPEC;
		char_proc_table = RkAllocCells(516);
		char_proc_table[0] = RK_VECTOR_TAG(257, RK_TCODE_VECTOR);
		for (i = 1; i < 258; ++i)
			char_proc_table[i] = RK_DUMMY_OBJ;
		mesh_proc_table = &char_proc_table[258];
		mesh_proc_table[0] = RK_VECTOR_TAG(257, RK_TCODE_VECTOR);
		for (i = 1; i < 258; ++i)
			mesh_proc_table[i] = RK_DUMMY_OBJ;
		p_traverse = rk_traverse_root;
		rk_traverse_root = traverse;
		rk_read_proc = RkRegisterProcedure(index + 0, sxp_read);
		make_char_table_entry(0, index + 1, cp_illegal, 0x80|RK_CFLAG_PUNCTUATION);
		for (i = 1; i < 256; ++i)
			RkCopyCharTableEntry(0, i);
		make_char_table_entry(' ', index + 2, cp_whitespace, RK_CFLAG_WHITESPACE|RK_CFLAG_PUNCTUATION);
		for (i = 0; BLANKCHARS[i]; ++i)
			RkCopyCharTableEntry(' ', BLANKCHARS[i]);
		make_char_table_entry(';', index + 3, cp_comment, RK_CFLAG_PUNCTUATION);
		make_char_table_entry('#', index + 4, cp_mesh, RK_CFLAG_PUNCTUATION);
		make_char_table_entry('A', index + 5, cp_initial, 0);
		for (i = 'B'; i <= 'Z'; ++i)
			RkCopyCharTableEntry('A', i);
		for (i = 'a'; i <= 'z'; ++i)
			RkCopyCharTableEntry('A', i);
		for (i = 0; SPCINITS[i]; ++i)
			RkCopyCharTableEntry('A', SPCINITS[i]);
		make_char_table_entry('0', index + 6, cp_digit, 0);
		for (i = '1'; i <= '9'; ++i)
			RkCopyCharTableEntry('0', i);
		RkCopyCharTableEntry('0', '@');
		make_char_table_entry('+', index + 7, cp_sign, 0);
		RkCopyCharTableEntry('+', '-');
		make_char_table_entry('.', index + 8, cp_dot, 0);
		make_char_table_entry('"', index + 9, cp_dquote, RK_CFLAG_PUNCTUATION);
		make_char_table_entry('(', index + 10, cp_parenth, RK_CFLAG_PUNCTUATION);
		make_char_table_entry(')', index + 11, cp_rpar, RK_CFLAG_PUNCTUATION);
		for (i = 0; i < 256; ++i)
			if (char_flag_table[i] & 0x80)
				if (RkIsDBCSLeadByte(i))
					RkCopyCharTableEntry('A', i);
				else
					char_flag_table[i] &= ~0x80;
		make_mesh_table_entry(0, index + 12, mp_illegal);
		for (i = 1; i < 256; ++i)
			RkCopyMeshTableEntry(0, i);
		make_mesh_table_entry('t', index + 13, mp_boolean);
		RkCopyMeshTableEntry('t', 'f');
		RkCopyMeshTableEntry('t', 'T');
		RkCopyMeshTableEntry('t', 'F');
		make_mesh_table_entry('\\', index + 14, mp_char);
		make_mesh_table_entry('(', index + 15, mp_vector);
		make_mesh_table_entry('b', index + 16, mp_numprefix);
		for (i = 1; NUMPREFIXES[i]; ++i)
			RkCopyMeshTableEntry('b', NUMPREFIXES[i]);
		for (i = 0; NUMPREFIXES[i]; ++i)
			RkCopyMeshTableEntry('b', toupper(NUMPREFIXES[i]));
		read_conti1_proc = RkRegisterProcedure(index + 17, read_conti1);
		read_conti2_proc = RkRegisterProcedure(index + 18, read_conti2);
		read_conti3_proc = RkRegisterProcedure(index + 19, read_conti3);
		cp_illegal_conti1_proc = RkRegisterProcedure(index + 20, cp_illegal_conti1);
		cp_whitespace_conti1_proc = RkRegisterProcedure(index + 21, cp_whitespace_conti1);
		cp_comment_conti1_proc = RkRegisterProcedure(index + 22, cp_comment_conti1);
		cp_comment_conti2_proc = RkRegisterProcedure(index + 23, cp_comment_conti2);
		cp_mesh_conti1_proc = RkRegisterProcedure(index + 24, cp_mesh_conti1);
		cp_initial_conti1_proc = RkRegisterProcedure(index + 25, cp_initial_conti1);
		cp_initial_conti2_proc = RkRegisterProcedure(index + 26, cp_initial_conti2);
		cp_number_conti1_proc = RkRegisterProcedure(index + 27, cp_number_conti1);
		cp_number_conti2_proc = RkRegisterProcedure(index + 28, cp_number_conti2);
		cp_sign_conti1_proc = RkRegisterProcedure(index + 29, cp_sign_conti1);
		cp_dot_conti1_proc = RkRegisterProcedure(index + 30, cp_dot_conti1);
		cp_string_conti1_proc = RkRegisterProcedure(index + 31, cp_string_conti1);
		cp_string_conti2_proc = RkRegisterProcedure(index + 32, cp_string_conti2);
		cp_string_conti3_proc = RkRegisterProcedure(index + 33, cp_string_conti3);
		cp_string_conti4_proc = RkRegisterProcedure(index + 34, cp_string_conti4);
		cp_list_conti1_proc = RkRegisterProcedure(index + 35, cp_list_conti1);
		cp_list_conti2_proc = RkRegisterProcedure(index + 36, cp_list_conti2);
		cp_list_conti3_proc = RkRegisterProcedure(index + 37, cp_list_conti3);
		cp_list_conti4_proc = RkRegisterProcedure(index + 38, cp_list_conti4);
		cp_list_conti5_proc = RkRegisterProcedure(index + 39, cp_list_conti5);
		cp_list_conti6_proc = RkRegisterProcedure(index + 40, cp_list_conti6);
		cp_list_conti7_proc = RkRegisterProcedure(index + 41, cp_list_conti7);
		cp_list_conti8_proc = RkRegisterProcedure(index + 42, cp_list_conti8);
		mp_illegal_conti1_proc = RkRegisterProcedure(index + 43, mp_illegal_conti1);
		mp_char_conti1_proc = RkRegisterProcedure(index + 44, mp_char_conti1);
		mp_char_conti2_proc = RkRegisterProcedure(index + 45, mp_char_conti2);
		mp_char_conti3_proc = RkRegisterProcedure(index + 46, mp_char_conti3);
		mp_vector_conti1_proc = RkRegisterProcedure(index + 47, mp_vector_conti1);
		mp_vector_conti2_proc = RkRegisterProcedure(index + 48, mp_vector_conti2);
		error_conti1_proc = RkRegisterProcedure(index + 49, error_conti1);
	}
	return	50;
}
