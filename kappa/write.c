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
static char rcsid[] = "@(#)$Id: write.c,v 1.7 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: write.c,v $
 * Revision 1.7  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.6  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.5  1998/07/31 10:34:08  qfwfq
 * Add symbol-aux-datum
 *
 * Revision 1.4  1997/05/12 07:21:14  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1996/10/10 08:26:49  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.2  1996/09/06 06:08:36  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.1  1993/11/08 14:02:22  qfwfq
 * Initial revision
 *
 */

/*
 * Write of S-expression.
 */
#include "rhizome.h"

#include <stdio.h>
#include <string.h>

rk_object rk_write_proc, rk_display_proc;

static rk_object default_conti1_proc, symbol_conti1_proc;
static rk_object list_conti1_proc, list_conti2_proc, list_conti3_proc;
static rk_object vector_conti1_proc, vector_conti2_proc, vector_conti3_proc, vector_conti4_proc;
static rk_object string_conti1_proc, string_conti2_proc;

static rk_object
sxp_write_aux(rk_object display)
{
	rk_object *cp, obj, proc;
	char buf[16], *str, *s;
	unsigned len, len0, i;

	if (!RK_ISCELL(obj = rk_eval_register[2])) {
		if (obj & 2)
			sprintf(buf, "%d", RK_GETINUM(obj));
		else if ((obj & 0xfc) == 0x0c)
			switch (obj >> 8) {
			case 3:		sprintf(buf, "%s", "#<eof>");	break;
			case 6:		sprintf(buf, "%s", "()");	break;
			case 7:		sprintf(buf, "%s", "#f");	break;
			case 8:		sprintf(buf, "%s", "#t");	break;
			case 9:		sprintf(buf, "%s", "#<done>");	break;
			default:	sprintf(buf, "#<%08X>", obj);	break;
			}
		else if ((obj & 0xfc) == 0x1c) {
			if (display == RK_SOBJ_TRUE) {
				proc = rk_eval_register[0];
				rk_eval_register[0] = obj;
				rk_valid_register = 2;
				return	proc;
			} else
				switch (RK_GETICHAR(obj)) {
				case '\n':	sprintf(buf, "%s", "#\\newline");		break;
				case ' ':	sprintf(buf, "%s", "#\\space");			break;
				default:	sprintf(buf, "#\\%c", RK_GETICHAR(obj));	break;
				}
		} else
			sprintf(buf, "#<%08X>", obj);
	} else if (!((cp = (rk_object *)obj)[0] & 1) || (cp[0] & 7) == 1) {
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(8, 0);
		cp[1] = list_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = ((rk_object *)rk_eval_register[2])[0] & ~1;
		cp[4] = ((rk_object *)rk_eval_register[2])[1];
		cp[5] = display;
		cp[6] = (rk_object)rk_continuation;
		cp[7] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		rk_eval_register[0] = RK_MAKEICHAR('(');
		rk_valid_register = 2;
		return	rk_continuation[2];
	} else if ((cp[0] & 7) == 5) {
		i = ((rk_object *)(obj = ((rk_object *)(((rk_object *)rk_eval_register[2])[0] & ~7))[0]))[0] >> 12;
		str = RkGetMallocObject(obj);
		if (!i && !((unsigned long *)str)[0]) {
			rk_eval_register[0] = RK_DUMMY_OBJ;
			rk_valid_register = 2;
			RK_PROCEED();
		}
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = symbol_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = RK_MAKEINUM(1);
		cp[4] = ((rk_object *)(((rk_object *)rk_eval_register[2])[0] & ~7))[0];
		cp[5] = (rk_object)rk_continuation;
		rk_continuation = cp;
		rk_eval_register[0] = RK_MAKEICHAR(str[i ? 0 : 4]);
		rk_valid_register = 2;
		return	rk_continuation[2];
	} else if ((cp[0] & 0x0f) == 7) {
		switch ((cp[0] >> 4) & 0xff) {
		case RK_TCODE_VECTOR:
			cp = RkAllocCells(6);
			cp[0] = RK_VECTOR_TAG(6, 0);
			cp[1] = vector_conti1_proc;
			cp[2] = rk_eval_register[0];
			cp[3] = rk_eval_register[2];
			cp[4] = display;
			cp[5] = (rk_object)rk_continuation;
			rk_continuation = cp;
			rk_eval_register[0] = RK_MAKEICHAR('#');
			rk_valid_register = 2;
			return	rk_continuation[2];
		case RK_TCODE_BIGINT_POS:
		case RK_TCODE_BIGINT_NEG:
		case RK_TCODE_FRACTION:
		case RK_TCODE_FLONUM:
		case RK_TCODE_COMPLEX:
			if (!(str = RkPrintNumber(obj, 10))) {
				rk_eval_register[0] = RK_SOBJ_ERROR;
				rk_error_code = RK_ERROR_OUTOFSTORAGE;
				rk_error_obj = RK_SOBJ_UNSPEC;
				rk_valid_register = 2;
				RK_PROCEED();
			}
			len = len0 = i = strlen(str);
			if (len == 0 || len >= (1 << 20)) {
				len += 4;
				i = 0;
			}
			if (!(s = malloc(len))) {
				RkScavenge(1);
				if (!(s = malloc(len))) {
					rk_eval_register[0] = RK_SOBJ_ERROR;
					rk_error_code = RK_ERROR_OUTOFSTORAGE;
					rk_error_obj = RK_SOBJ_UNSPEC;
					rk_valid_register = 2;
					RK_PROCEED();
				}
			}
			if (!(rk_eval_register[2] = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING)
									, rk_plain_destructor, s))) {
				free(s);
				rk_eval_register[0] = RK_SOBJ_ERROR;
				rk_error_code = RK_ERROR_OUTOFSTORAGE;
				rk_error_obj = RK_SOBJ_UNSPEC;
				rk_valid_register = 2;
				RK_PROCEED();
			}
			if (!i) {
				*(unsigned long *)s = len0;
				s += 4;
			}
			for (i = 0; i < len0; ++i)
				s[i] = str[len0 - i - 1];
			cp = RkAllocCells(6);
			cp[0] = RK_VECTOR_TAG(6, 0);
			cp[1] = symbol_conti1_proc;
			cp[2] = rk_eval_register[0];
			cp[3] = RK_MAKEINUM(1);
			cp[4] = rk_eval_register[2];
			cp[5] = (rk_object)rk_continuation;
			rk_continuation = cp;
			rk_eval_register[0] = RK_MAKEICHAR(s[i ? 0 : 4]);
			rk_valid_register = 2;
			return	rk_continuation[2];
		default:		sprintf(buf, "#<%08X>", obj);	break;
		}
	} else if ((cp[0] & 0x0f) == 0x0f) {
		switch ((cp[0] >> 4) & 0xff) {
		case RK_TCODE_STRING:
			if (display == RK_SOBJ_TRUE) {
				str = RkGetMallocObject((rk_object)cp);
				if (!(i = cp[0] >> 12) && !((unsigned long *)str)[0]) {
					rk_eval_register[0] = RK_DUMMY_OBJ;
					rk_valid_register = 2;
					RK_PROCEED();
				}
				cp = RkAllocCells(6);
				cp[0] = RK_VECTOR_TAG(6, 0);
				cp[1] = symbol_conti1_proc;
				cp[2] = rk_eval_register[0];
				cp[3] = RK_MAKEINUM(1);
				cp[4] = rk_eval_register[2];
				cp[5] = (rk_object)rk_continuation;
				rk_continuation = cp;
				rk_eval_register[0] = RK_MAKEICHAR(str[i ? 0 : 4]);
				rk_valid_register = 2;
				return	rk_continuation[2];
			} else {
				cp = RkAllocCells(6);
				cp[0] = RK_VECTOR_TAG(6, 0);
				cp[1] = string_conti1_proc;
				cp[2] = rk_eval_register[0];
				cp[3] = RK_MAKEINUM(0);
				cp[4] = rk_eval_register[2];
				cp[5] = (rk_object)rk_continuation;
				rk_continuation = cp;
				rk_eval_register[0] = RK_MAKEICHAR('"');
				rk_valid_register = 2;
				return	rk_continuation[2];
			}
		default:		sprintf(buf, "#<%08X>", obj);	break;
		}
	} else
		sprintf(buf, "#<%08X>", obj);
	cp = RkAllocCells(36);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = default_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)&cp[6];
	cp[4] = (rk_object)rk_continuation;
	cp[5] = RK_DUMMY_OBJ;
	cp[6] = RK_MAKEICHAR(buf[1]);
	for (i = 2; i < 16; ++i) {
		cp[7+(i-2)*2] = (rk_object)&cp[8+(i-2)*2];
		cp[8+(i-2)*2] = RK_MAKEICHAR(buf[i]);
	}
	cp[35] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = RK_MAKEICHAR(buf[0]);
	rk_valid_register = 2;
	return	rk_continuation[2];
}

static rk_object
sxp_write(void)
{
	return	sxp_write_aux(RK_SOBJ_FALSE);
}

static rk_object
sxp_display(void)
{
	return	sxp_write_aux(RK_SOBJ_TRUE);
}

static rk_object
default_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || !RK_GETICHAR(((rk_object *)rk_continuation[3])[0])) {
		rk_continuation = (rk_object *)rk_continuation[4];
		RK_PROCEED();
	}
	rk_eval_register[0] = ((rk_object *)rk_continuation[3])[0];
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = default_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = ((rk_object *)rk_continuation[3])[1];
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rk_continuation[2];
}

static rk_object
symbol_conti1(void)
{
	rk_object *cp;
	unsigned i, len;
	char *str;

	i = RK_GETINUM(rk_continuation[3]) & 0x3fffffff;
	len = ((rk_object *)rk_continuation[4])[0] >> 12;
	str = RkGetMallocObject(rk_continuation[4]);
	if (rk_eval_register[0] == RK_SOBJ_ERROR || i == (len ? len : ((unsigned long *)str)[0])) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	rk_eval_register[0] = RK_MAKEICHAR(str[len ? i : i + 4]);
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = symbol_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3] + (1<<2);
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	rk_continuation[2];
}

static rk_object
list_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[6];
		RK_PROCEED();
	}
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[2] = rk_continuation[3];
	rk_valid_register = 3;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = list_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[4];
	cp[4] = rk_continuation[5];
	cp[5] = rk_continuation[6];
	rk_continuation = cp;
	return	rk_continuation[4] == RK_SOBJ_TRUE ? rk_display_proc : rk_write_proc;
}

static rk_object
list_conti2(void)
{
	rk_object *cp, obj;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if (!RK_ISCELL(obj = rk_continuation[3]) || ((cp = (rk_object *)obj)[0] & 1) && (cp[0] & 7) != 1) {
	 	if (obj == RK_SOBJ_NIL) {
	 		obj = rk_continuation[2];
	 		rk_eval_register[0] = RK_MAKEICHAR(')');
	 		rk_continuation = (rk_object *)rk_continuation[5];
	 		return	obj;
		}
		rk_eval_register[0] = RK_MAKEICHAR(' ');
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(8, 0);
		cp[1] = list_conti3_proc;
		cp[2] = rk_continuation[2];
		cp[3] = RK_MAKEINUM(0);
		cp[4] = rk_continuation[3];
		cp[5] = rk_continuation[4];
		cp[6] = rk_continuation[5];
		cp[7] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		return	rk_continuation[2];
	}
	rk_eval_register[0] = RK_MAKEICHAR(' ');
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = list_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = ((rk_object *)rk_continuation[3])[0] & ~1;
	cp[4] = ((rk_object *)rk_continuation[3])[1];
	cp[5] = rk_continuation[4];
	cp[6] = rk_continuation[5];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rk_continuation[2];
}

static rk_object
list_conti3(void)
{
	rk_object *cp;
	int i;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[6];
		RK_PROCEED();
	}
	switch (i = RK_GETINUM(rk_continuation[3])) {
	case 0: case 1:
		rk_eval_register[0] = RK_MAKEICHAR(". "[i]);
		cp = RkAllocCells(8);
		cp[0] = RK_VECTOR_TAG(8, 0);
		cp[1] = list_conti3_proc;
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3] + (1<<2);
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
		cp[6] = rk_continuation[6];
		cp[7] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		return	rk_continuation[2];
	default:
		rk_eval_register[0] = rk_continuation[2];
		rk_eval_register[2] = rk_continuation[4];
		rk_valid_register = 3;
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = list_conti2_proc;
		cp[2] = rk_continuation[2];
		cp[3] = RK_SOBJ_NIL;
		cp[4] = rk_continuation[5];
		cp[5] = rk_continuation[6];
		rk_continuation = cp;
		return	rk_continuation[4] == RK_SOBJ_TRUE ? rk_display_proc : rk_write_proc;
	}
}

static rk_object
vector_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = vector_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	rk_eval_register[0] = RK_MAKEICHAR('(');
	return	rk_continuation[2];
}

static rk_object
vector_conti2(void)
{
	rk_object *cp, proc;
	unsigned len;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	if ((len = ((rk_object *)rk_continuation[3])[0] >> 12) == 1) {
		rk_eval_register[0] = RK_MAKEICHAR(')');
		proc = rk_continuation[2];
		rk_continuation = (rk_object *)rk_continuation[5];
		return	proc;
	}
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[2] = ((rk_object *)rk_continuation[3])[len ? 1 : 2];
	rk_valid_register = 3;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = vector_conti3_proc;
	cp[2] = rk_continuation[2];
	cp[3] = RK_MAKEINUM(1);
	cp[4] = rk_continuation[3];
	cp[5] = rk_continuation[4];
	cp[6] = rk_continuation[5];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rk_continuation[5] == RK_SOBJ_TRUE ? rk_display_proc : rk_write_proc;
}

static rk_object
vector_conti3(void)
{
	rk_object *cp, proc;
	unsigned i, len;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[6];
		RK_PROCEED();
	}
	i = RK_GETINUM(rk_continuation[3]) & 0x3fffffff;
	len = ((rk_object *)rk_continuation[4])[0] >> 12;
	if (i == (len ? len-1 : (RK_GETINUM(((rk_object *)rk_continuation[4])[1]) & 0x3fffffff)-2)) {
		rk_eval_register[0] = RK_MAKEICHAR(')');
		proc = rk_continuation[2];
		rk_continuation = (rk_object *)rk_continuation[6];
		return	proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = vector_conti4_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_continuation[6];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = RK_MAKEICHAR(' ');
	return	rk_continuation[2];
}

static rk_object
vector_conti4(void)
{
	rk_object *cp;
	unsigned i, len;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[6];
		RK_PROCEED();
	}
	i = RK_GETINUM(rk_continuation[3]) & 0x3fffffff;
	len = ((rk_object *)rk_continuation[4])[0] >> 12;
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[2] = ((rk_object *)rk_continuation[4])[len ? i+1 : i+2];
	rk_valid_register = 3;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, 0);
	cp[1] = vector_conti3_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3] + (1<<2);
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	cp[6] = rk_continuation[6];
	cp[7] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rk_continuation[5] == RK_SOBJ_TRUE ? rk_display_proc : rk_write_proc;
}

static rk_object
string_conti1(void)
{
	rk_object *cp, proc;
	unsigned i, len;
	char *str;
	int c;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	i = RK_GETINUM(rk_continuation[3]) & 0x3fffffff;
	len = ((rk_object *)rk_continuation[4])[0] >> 12;
	str = RkGetMallocObject(rk_continuation[4]);
	if (!len)
		len = ((unsigned long *)(str += 4))[-1];
	if (i == len) {
		rk_eval_register[0] = RK_MAKEICHAR('"');
		proc = rk_continuation[2];
		rk_continuation = (rk_object *)rk_continuation[5];
		return	proc;
	}
	c = str[i];
	if (i + 1 < len && RkIsDBCSLeadByte(c)) {
		rk_eval_register[0] = RK_MAKEICHAR(c);
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = string_conti2_proc;
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3] + (1<<2);
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
		rk_continuation = cp;
		return	rk_continuation[2];
	}
	if (c == '"' || c == '\\') {
		rk_eval_register[0] = RK_MAKEICHAR('\\');
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = string_conti2_proc;
		cp[2] = rk_continuation[2];
		cp[3] = rk_continuation[3];
		cp[4] = rk_continuation[4];
		cp[5] = rk_continuation[5];
		rk_continuation = cp;
		return	rk_continuation[2];
	}
	rk_eval_register[0] = RK_MAKEICHAR(c);
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = string_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3] + (1<<2);
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	rk_continuation[2];
}

static rk_object
string_conti2(void)
{
	rk_object *cp;
	unsigned i, len;
	char *str;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		rk_continuation = (rk_object *)rk_continuation[5];
		RK_PROCEED();
	}
	i = RK_GETINUM(rk_continuation[3]) & 0x3fffffff;
	len = ((rk_object *)rk_continuation[4])[0] >> 12;
	str = RkGetMallocObject(rk_continuation[4]);
	rk_eval_register[0] = RK_MAKEICHAR(str[len ? i : i + 4]);
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = string_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3] + (1<<2);
	cp[4] = rk_continuation[4];
	cp[5] = rk_continuation[5];
	rk_continuation = cp;
	return	rk_continuation[2];
}

int
RkInitializeWrite(int index)
{
	if (index != -1) {
		rk_write_proc = RkRegisterProcedure(index + 0, sxp_write);
		rk_display_proc = RkRegisterProcedure(index + 1, sxp_display);
		default_conti1_proc = RkRegisterProcedure(index + 2, default_conti1);
		symbol_conti1_proc = RkRegisterProcedure(index + 3, symbol_conti1);
		list_conti1_proc = RkRegisterProcedure(index + 4, list_conti1);
		list_conti2_proc = RkRegisterProcedure(index + 5, list_conti2);
		list_conti3_proc = RkRegisterProcedure(index + 6, list_conti3);
		vector_conti1_proc = RkRegisterProcedure(index + 7, vector_conti1);
		vector_conti2_proc = RkRegisterProcedure(index + 8, vector_conti2);
		vector_conti3_proc = RkRegisterProcedure(index + 9, vector_conti3);
		vector_conti4_proc = RkRegisterProcedure(index + 10, vector_conti4);
		string_conti1_proc = RkRegisterProcedure(index + 11, string_conti1);
		string_conti2_proc = RkRegisterProcedure(index + 12, string_conti2);
	}
	return	13;
}
