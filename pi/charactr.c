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
static char rcsid[] = "@(#)$Id: charactr.c,v 1.6 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: charactr.c,v $
 * Revision 1.6  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.5  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.4  1999/02/15 08:37:15  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.3  1997/05/12 07:21:16  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.2  1996/09/06 06:11:23  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:47  qfwfq
 * Initial revision
 *
 */

/*
 * Character related functions.
 */
#include "rhiz_pi.h"

#include <ctype.h>
#include <string.h>

rk_object rp_chareq_proc;
static rk_object
chareq(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (obj1 == obj2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charlt_proc;
static rk_object
charlt(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (obj1 < obj2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_chargt_proc;
static rk_object
chargt(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (obj1 > obj2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charle_proc;
static rk_object
charle(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (obj1 <= obj2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charge_proc;
static rk_object
charge(void)
{
	rk_object obj1, obj2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (obj1 >= obj2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charcieq_proc;
static rk_object
charcieq(void)
{
	rk_object obj1, obj2;
	int c1, c2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c1 = RK_GETICHAR(obj1);
	c2 = RK_GETICHAR(obj2);
	if (isascii(c1) && isupper(c1))
		c1 = tolower(c1);
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	if (c1 == c2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charcilt_proc;
static rk_object
charcilt(void)
{
	rk_object obj1, obj2;
	int c1, c2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c1 = RK_GETICHAR(obj1);
	c2 = RK_GETICHAR(obj2);
	if (isascii(c1) && isupper(c1))
		c1 = tolower(c1);
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	if (c1 < c2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charcigt_proc;
static rk_object
charcigt(void)
{
	rk_object obj1, obj2;
	int c1, c2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c1 = RK_GETICHAR(obj1);
	c2 = RK_GETICHAR(obj2);
	if (isascii(c1) && isupper(c1))
		c1 = tolower(c1);
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	if (c1 > c2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charcile_proc;
static rk_object
charcile(void)
{
	rk_object obj1, obj2;
	int c1, c2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c1 = RK_GETICHAR(obj1);
	c2 = RK_GETICHAR(obj2);
	if (isascii(c1) && isupper(c1))
		c1 = tolower(c1);
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	if (c1 <= c2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_charcige_proc;
static rk_object
charcige(void)
{
	rk_object obj1, obj2;
	int c1, c2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISICHAR(obj1) || !RK_ISICHAR(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c1 = RK_GETICHAR(obj1);
	c2 = RK_GETICHAR(obj2);
	if (isascii(c1) && isupper(c1))
		c1 = tolower(c1);
	if (isascii(c2) && isupper(c2))
		c2 = tolower(c2);
	if (c1 >= c2)
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_alphap_proc;
static rk_object
alphap(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && isalpha(c))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_digitp_proc;
static rk_object
digitp(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && isdigit(c))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_spacep_proc;
static rk_object
spacep(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && isspace(c))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_upperp_proc;
static rk_object
upperp(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && isupper(c))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_lowerp_proc;
static rk_object
lowerp(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && islower(c))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_char2int_proc;
static rk_object
char2int(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	rk_eval_register[0] = RK_MAKEINUM(c);
	RP_RETURN();
}

rk_object rp_int2char_proc;
static rk_object
int2char(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETINUM(rk_eval_register[0]);
	if (c < 0 || c > 255)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_MAKEICHAR(c);
	RP_RETURN();
}

rk_object rp_upcase_proc;
static rk_object
upcase(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && islower(c))
		c = toupper(c);
	rk_eval_register[0] = RK_MAKEICHAR(c);
	RP_RETURN();
}

rk_object rp_downcase_proc;
static rk_object
downcase(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (isascii(c) && isupper(c))
		c = tolower(c);
	rk_eval_register[0] = RK_MAKEICHAR(c);
	RP_RETURN();
}

static char *
create_string(int len)
{
	int len0, i;
	char *s;

	i = len0 = len;
	if (len == 0 || len >= (1<<20)) {
		len += 4;
		i = 0;
	}
	if (!(s = malloc(len))) {
		RkScavenge(1);
		if (!(s = malloc(len)))
			return	NULL;
	}
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		return	NULL;
	}
	if (!i) {
		*(unsigned long *)s = len0;
		return	s + 4;
	} else
		return	s;
}

rk_object rp_makestr_proc;
static rk_object
makestr(void)
{
	rk_object obj;
	int len, i, c;
	char *s;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		c = '\0';
		obj = rk_eval_register[0];
		break;
	case 2:
		if (!RK_ISICHAR(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		c = RK_GETICHAR(rk_eval_register[0]);
		obj = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISINUM(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = RK_GETINUM(obj);
	if (len < 0)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(s = create_string(len)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	for (i = 0; i < len; ++i)
		s[i] = c;
	RP_RETURN();
}

rk_object rp_crstring_proc;
static rk_object
crstring(void)
{
	int len;
	char *s;
	rk_object obj;

	len = RK_GETINUM(rk_eval_register[2]);
	if (len > 0 && !RK_ISICHAR(obj = rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(s = create_string(len)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (len > 0) {
		s[--len] = RK_GETICHAR(obj);
		for (obj = rk_eval_register[1]; len > 0; obj = RP_CDR(obj)) {
			if (!RK_ISICHAR(RP_CAR(obj)))
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			s[--len] = RK_GETICHAR(RP_CAR(obj));
		}
	}
	RP_RETURN();
}

rk_object rp_stringlength_proc;
static rk_object
stringlength(void)
{
	int len;

	RP_ASSERTARG(1);
	if (!RK_ISSTRING(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(len = RP_CAR(rk_eval_register[0]) >> 12))
		len = *(unsigned long *)RkGetMallocObject(rk_eval_register[0]);
	rk_eval_register[0] = RK_MAKEINUM(len);
	RP_RETURN();
}

static unsigned char *
get_stringdata(rk_object obj, int *len)
{
	char *s;

	if (!(*len = RP_CAR(obj) >> 12)) {
		s = RkGetMallocObject(obj);
		*len = *(unsigned long *)s;
		return	(unsigned char *)(s+4);
	}
	return	(unsigned char *)RkGetMallocObject(obj);
}

rk_object rp_stringref_proc;
static rk_object
stringref(void)
{
	int len, index;
	unsigned char *s;

	RP_ASSERTARG(2);
	if (!RK_ISSTRING(RP_CAR(rk_eval_register[1])) || !RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = get_stringdata(RP_CAR(rk_eval_register[1]), &len);
	index = RK_GETINUM(rk_eval_register[0]);
	if (index < 0 || index >= len)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = RK_MAKEICHAR(s[index]);
	RP_RETURN();
}

rk_object rp_stringsetbang_proc;
static rk_object
stringsetbang(void)
{
	int len, index, c;
	unsigned char *s;

	RP_ASSERTARG(3);
	if (!RK_ISSTRING(RP_CAR(RP_CDR(rk_eval_register[1]))) || !RK_ISINUM(RP_CAR(rk_eval_register[1]))
	 || !RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = get_stringdata(RP_CAR(RP_CDR(rk_eval_register[1])), &len);
	index = RK_GETINUM(RP_CAR(rk_eval_register[1]));
	c = RK_GETICHAR(rk_eval_register[0]);
	if (index < 0 || index >= len)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s[index] = c;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_stringeqp_proc;
static rk_object
stringeqp(void)
{
	rk_object obj1, obj2;
	int len1, len2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	if (len1 == len2 && !memcmp(s1, s2, len1))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringcieqp_proc;
static rk_object
stringcieqp(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	if (len1 != len2) {
		rk_eval_register[0] = RK_SOBJ_FALSE;
		RP_RETURN();
	}
	for (i = 0; i < len1; ++i) {
		c1 = s1[i];
		if (isascii(c1) && isupper(c1))
			c1 = tolower(c1);
		c2 = s2[i];
		if (isascii(c2) && isupper(c2))
			c2 = tolower(c2);
		if (c1 != c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_stringlt_proc;
static rk_object
stringlt(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len2; ++i) {
		if (i == len1) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if ((c1 = s1[i]) > (c2 = s2[i])) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringgt_proc;
static rk_object
stringgt(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len1; ++i) {
		if (i == len2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if ((c1 = s1[i]) > (c2 = s2[i])) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringle_proc;
static rk_object
stringle(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len1; ++i) {
		if (i == len2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if ((c1 = s1[i]) > (c2 = s2[i])) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_stringge_proc;
static rk_object
stringge(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len2; ++i) {
		if (i == len1) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if ((c1 = s1[i]) > (c2 = s2[i])) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_stringcilt_proc;
static rk_object
stringcilt(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len2; ++i) {
		if (i == len1) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		c1 = s1[i];
		if (isascii(c1) && isupper(c1))
			c1 = tolower(c1);
		c2 = s2[i];
		if (isascii(c2) && isupper(c2))
			c2 = tolower(c2);
		if (c1 > c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringcigt_proc;
static rk_object
stringcigt(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len1; ++i) {
		if (i == len2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		c1 = s1[i];
		if (isascii(c1) && isupper(c1))
			c1 = tolower(c1);
		c2 = s2[i];
		if (isascii(c2) && isupper(c2))
			c2 = tolower(c2);
		if (c1 > c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_stringcile_proc;
static rk_object
stringcile(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len1; ++i) {
		if (i == len2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		c1 = s1[i];
		if (isascii(c1) && isupper(c1))
			c1 = tolower(c1);
		c2 = s2[i];
		if (isascii(c2) && isupper(c2))
			c2 = tolower(c2);
		if (c1 > c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_stringcige_proc;
static rk_object
stringcige(void)
{
	rk_object obj1, obj2;
	int len1, len2, i, c1, c2;
	unsigned char *s1, *s2;

	RP_ASSERTARG(2);
	obj1 = RP_CAR(rk_eval_register[1]);
	obj2 = rk_eval_register[0];
	if (!RK_ISSTRING(obj1) || !RK_ISSTRING(obj2))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(obj1, &len1);
	s2 = get_stringdata(obj2, &len2);
	for (i = 0; i < len2; ++i) {
		if (i == len1) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
		c1 = s1[i];
		if (isascii(c1) && isupper(c1))
			c1 = tolower(c1);
		c2 = s2[i];
		if (isascii(c2) && isupper(c2))
			c2 = tolower(c2);
		if (c1 > c2) {
			rk_eval_register[0] = RK_SOBJ_TRUE;
			RP_RETURN();
		}
		if (c1 < c2) {
			rk_eval_register[0] = RK_SOBJ_FALSE;
			RP_RETURN();
		}
	}
	rk_eval_register[0] = RK_SOBJ_TRUE;
	RP_RETURN();
}

rk_object rp_substring_proc;
static rk_object
substring(void)
{
	rk_object obj;
	int len, start, end;
	unsigned char *s1, *s2;

	RP_ASSERTARG(3);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	end = RK_GETINUM(rk_eval_register[0]);
	obj = rk_eval_register[1];
	if (!RK_ISINUM(RP_CAR(obj)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	start = RK_GETINUM(RP_CAR(obj));
	obj = RP_CDR(obj);
	if (!RK_ISSTRING(RP_CAR(obj)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s1 = get_stringdata(RP_CAR(obj), &len);
	if (start < 0 || start > end || end > len)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if (!(s2 = create_string(end - start)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	memcpy(s2, &s1[start], end - start);
	RP_RETURN();
}

rk_object rp_strappend_proc;
static rk_object
strappend(void)
{
	rk_object obj;
	int n, i, totlen, len;
	unsigned char *s1, *s2;

	n = RK_GETINUM(rk_eval_register[2]);
	totlen = 0;
	if (n > 0) {
		if (!RK_ISSTRING(rk_eval_register[0]))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		(void)get_stringdata(rk_eval_register[0], &len);
		totlen += len;
		for (obj = rk_eval_register[1], i = 1; i < n; obj = RP_CDR(obj), ++i) {
			if (!RK_ISSTRING(RP_CAR(obj)))
				RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
			(void)get_stringdata(RP_CAR(obj), &len);
			totlen += len;
		}
	}
	rk_eval_register[2] = rk_eval_register[0];
	if (!(s2 = create_string(totlen)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (n > 0) {
		while (--n > 0) {
			for (obj = rk_eval_register[1], i = 1; i < n; obj = RP_CDR(obj), ++i) ;
			s1 = get_stringdata(RP_CAR(obj), &len);
			memcpy(s2, s1, len);
			s2 += len;
		}
		s1 = get_stringdata(rk_eval_register[2], &len);
		memcpy(s2, s1, len);
	}
	RP_RETURN();
}

rk_object rp_dbcsp_proc;
static rk_object
dbcsp(void)
{
	int c;

	RP_ASSERTARG(1);
	if (!RK_ISICHAR(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	c = RK_GETICHAR(rk_eval_register[0]);
	rk_eval_register[0] = RkIsDBCSLeadByte(c) ? RK_SOBJ_TRUE : RK_SOBJ_FALSE;
	RP_RETURN();
}

int
RpInitializeCharacters(int index)
{
	if (index != -1) {
		RP_DEFINESUBR("char=?", rp_chareq_proc, index + 0, chareq);
		RP_DEFINESUBR("char<?", rp_charlt_proc, index + 1, charlt);
		RP_DEFINESUBR("char>?", rp_chargt_proc, index + 2, chargt);
		RP_DEFINESUBR("char<=?", rp_charle_proc, index + 3, charle);
		RP_DEFINESUBR("char>=?", rp_charge_proc, index + 4, charge);
		RP_DEFINESUBR("char-ci=?", rp_charcieq_proc, index + 5, charcieq);
		RP_DEFINESUBR("char-ci<?", rp_charcilt_proc, index + 6, charcilt);
		RP_DEFINESUBR("char-ci>?", rp_charcigt_proc, index + 7, charcigt);
		RP_DEFINESUBR("char-ci<=?", rp_charcile_proc, index + 8, charcile);
		RP_DEFINESUBR("char-ci>=?", rp_charcige_proc, index + 9, charcige);
		RP_DEFINESUBR("char-alphabetic?", rp_alphap_proc, index + 10, alphap);
		RP_DEFINESUBR("char-numeric?", rp_digitp_proc, index + 11, digitp);
		RP_DEFINESUBR("char-whitespace?", rp_spacep_proc, index + 12, spacep);
		RP_DEFINESUBR("char-upper-case?", rp_upperp_proc, index + 13, upperp);
		RP_DEFINESUBR("char-lower-case?", rp_lowerp_proc, index + 14, lowerp);
		RP_DEFINESUBR("char->integer", rp_char2int_proc, index + 15, char2int);
		RP_DEFINESUBR("integer->char", rp_int2char_proc, index + 16, int2char);
		RP_DEFINESUBR("char-upcase", rp_upcase_proc, index + 17, upcase);
		RP_DEFINESUBR("char-downcase", rp_downcase_proc, index + 18, downcase);
		RP_DEFINESUBR("make-string", rp_makestr_proc, index + 19, makestr);
		RP_DEFINESUBR("string", rp_crstring_proc, index + 20, crstring);
		RP_DEFINESUBR("string-length", rp_stringlength_proc, index + 21, stringlength);
		RP_DEFINESUBR("string-ref", rp_stringref_proc, index + 22, stringref);
		RP_DEFINESUBR("string-set!", rp_stringsetbang_proc, index + 23, stringsetbang);
		RP_DEFINESUBR("string=?", rp_stringeqp_proc, index + 24, stringeqp);
		RP_DEFINESUBR("string-ci=?", rp_stringcieqp_proc, index + 25, stringcieqp);
		RP_DEFINESUBR("string<?", rp_stringlt_proc, index + 26, stringlt);
		RP_DEFINESUBR("string>?", rp_stringgt_proc, index + 27, stringgt);
		RP_DEFINESUBR("string<=?", rp_stringle_proc, index + 28, stringle);
		RP_DEFINESUBR("string>=?", rp_stringge_proc, index + 29, stringge);
		RP_DEFINESUBR("string-ci<?", rp_stringcilt_proc, index + 30, stringcilt);
		RP_DEFINESUBR("string-ci>?", rp_stringcigt_proc, index + 31, stringcigt);
		RP_DEFINESUBR("string-ci<=?", rp_stringcile_proc, index + 32, stringcile);
		RP_DEFINESUBR("string-ci>=?", rp_stringcige_proc, index + 33, stringcige);
		RP_DEFINESUBR("substring", rp_substring_proc, index + 34, substring);
		RP_DEFINESUBR("string-append", rp_strappend_proc, index + 35, strappend);
		RP_DEFINESUBR("rp:char-dbcs-lead-byte?", rp_dbcsp_proc, index + 36, dbcsp);
		rk_valid_register = 0;
	}
	return	37;
}
