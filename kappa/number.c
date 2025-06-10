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
static char rcsid[] = "@(#)$Id: number.c,v 1.9 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: number.c,v $
 * Revision 1.9  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.8  2002/09/27 11:06:43  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.7  2002/07/12 06:40:26  qfwfq
 * Workaround gcc 2.96.x overoptimization.
 *
 * Revision 1.6  1999/02/15 08:07:27  qfwfq
 * treatment of infinite and not-a-number
 *
 * Revision 1.5  1998/07/31 10:35:23  qfwfq
 * Add bitwise operation
 *
 * Revision 1.4  1996/10/10 08:26:47  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:08:33  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.2  1996/06/05 05:29:16  qfwfq
 * Add switches concerning IEEE floating point functions.
 *
 * Revision 1.1  93/11/08  14:02:19  qfwfq
 * Initial revision
 * 
 */

/*
 * Code to handle bignum, floating points, ....
 */
#include "rhizome.h"

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <string.h>

#define BIGNUM_BUFFER_SIZE	(((RK_BULK_ALLOC_THRESHOLD-1)*30)/16 + 1)
#define BIGNUM_BUFFER_COUNT	8

jmp_buf *rk_sigfpe_catcher;

static unsigned short bignum_register[BIGNUM_BUFFER_COUNT][BIGNUM_BUFFER_SIZE];
static unsigned short bignum_register_size[BIGNUM_BUFFER_COUNT];
static char bignum_register_sign[BIGNUM_BUFFER_COUNT];

void
RkLoadBigInt(rk_object num, unsigned reg)
{
	unsigned i, n, residue, shift, pos;
	rk_object *cp;

	cp = (rk_object *)num;
	bignum_register_sign[reg] = ((cp[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG));
	n = cp[0] >> 12;
	residue = 0;
	shift = 0;
	pos = 0;
	for (i = 1; i < n; ++i) {
		residue |= ((cp[i] >> 2) << shift);
		bignum_register[reg][pos++] = residue;
		residue = cp[i] >> (18 - shift);
		if ((shift += 14) >= 16) {
			bignum_register[reg][pos++] = residue;
			shift -= 16;
			residue = shift ? cp[i] >> (32 - shift) : 0;
		}
	}
	if (residue)
		bignum_register[reg][pos++] = residue;
	else
		while (!bignum_register[reg][pos-1])
			--pos;
	bignum_register_size[reg] = pos;
}

void
RkLoadShortInt(long n, unsigned reg)
{
	if (bignum_register_sign[reg] = (n < 0))
		n = -n;
	bignum_register[reg][0] = n;
	if (n = (n >> 16)) {
		bignum_register[reg][1] = n;
		bignum_register_size[reg] = 2;
	} else
		bignum_register_size[reg] = 1;
}

void
RkLoadInteger(rk_object num, unsigned reg)
{
	if (RK_ISINUM(num))
		RkLoadShortInt(RK_GETINUM(num), reg);
	else
		RkLoadBigInt(num, reg);
}

rk_object
RkStoreInt(unsigned reg)
{
	rk_object tag, *cp;
	unsigned size, i, pos;
	unsigned long n;
	int shift;

	if (!bignum_register_sign[reg]) {
		if (bignum_register_size[reg] == 1)
			return	((unsigned long)bignum_register[reg][0] << 2) | 2;
		else if (bignum_register_size[reg] == 2 && !(bignum_register[reg][1] & 0xe000))
			return	((unsigned long)bignum_register[reg][1] << 18)
			      | ((unsigned long)bignum_register[reg][0] << 2) | 2;
		else
			tag = RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS);
	} else {
		if (bignum_register_size[reg] == 1)
			return	((-(long)bignum_register[reg][0]) << 2) | 2;
		else if (bignum_register_size[reg] == 2) {
			n = ((unsigned long)bignum_register[reg][1] << 16) | ((unsigned long)bignum_register[reg][0]);
			if (n <= 0x20000000)
				return	((-n) << 2) | 2;
		}
		tag = RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG);
	}
	size = bignum_register_size[reg] * 16;
	n = 0x10000;
	while (!(bignum_register[reg][bignum_register_size[reg] - 1] & (n >>= 1)))
		--size;
	size = (size + 29) / 30 + 1;
	cp = RkAllocCells((size+1)&~1);
	cp[0] = tag | (size << 12);
	cp[((size+1)&~1) - 1] = RK_DUMMY_OBJ;
	n = 0;
	shift = 0;
	pos = 1;
	for (i = 0; i < bignum_register_size[reg]; ++i) {
		n |= (bignum_register[reg][i] << shift);
		if (shift >= 14) {
			cp[pos++] = (n << 2) | 2;
			if (pos == size)
				return	(rk_object)cp;
			shift -= 30;
			n = bignum_register[reg][i] >> (-shift);
		}
		shift += 16;
	}
	cp[pos] = (n << 2) | 2;
	return	(rk_object)cp;
}

double
RkLoadFloat(rk_object num)
{
	double volatile buf;
	rk_object *cp = (rk_object *)num;

	((unsigned short volatile *)&buf)[0] = cp[1] >> 16;
	((unsigned char volatile *)&buf)[2] = cp[1] >> 8;
	((unsigned char volatile *)&buf)[3] = cp[2] >> 24;
	((unsigned short volatile *)&buf)[2] = cp[2] >> 8;
	((unsigned short volatile *)&buf)[3] = cp[3] >> 16;
	return	buf;
}

rk_object
RkStoreFloat(double d)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FLONUM);
	cp[1] = ((unsigned long)((unsigned short *)&d)[0] << 16) | ((unsigned long)((unsigned char *)&d)[2] << 8) | 2;
	cp[2] = ((unsigned long)((unsigned char *)&d)[3] << 24) | ((unsigned long)((unsigned short *)&d)[2] << 8) | 2;
	cp[3] = ((unsigned long)((unsigned short *)&d)[3] << 16) | 2;
	return	(rk_object)cp;
}

#define GET_DIGITS	1
#define GET_MESH	2
#define GET_POINT	4
#define GET_EXPONENT	8
#define GET_ERROR	-1

static int
scan_uint_or_decimal(unsigned r0, unsigned r1, char **s, unsigned *len, int *exp, int radix)
{
	int c, xsign, flags = 0;

	RkLoadShortInt(0, r0);
	RkLoadShortInt(1, r1);
	while (*len > 0) {
		if ((c = (*s)[0]) == '#') {
			if (!(flags & GET_DIGITS))
				return	GET_ERROR;
			if (!(flags & GET_POINT))
				if (!RkMultiplyIntByUshort(r0, radix))
					return	GET_ERROR;
			flags |= GET_MESH;
		} else if (c == '.') {
			if (flags & GET_POINT)
				return	GET_ERROR;
			flags |= GET_POINT;
		} else {
			c = (isascii(c) && isupper(c)) ? tolower(c) : c;
			c = (isascii(c) && isdigit(c)) ? c - '0' : (c < 'a') ? radix : c - 'a' + 10;
			if (c >= radix)
				break;
			if (flags & GET_MESH)
				return	GET_ERROR;
			if (!RkMultiplyIntByUshort(r0, radix) || !RkAddIntByUshort(r0, c))
				return	GET_ERROR;
			if (flags & GET_POINT)
				if (!RkMultiplyIntByUshort(r1, radix))
					return	GET_ERROR;
			flags |= GET_DIGITS;
		}
		++(*s);
		--(*len);
	}
	if (*len == 0)
		return	((flags & GET_POINT) && !(flags & GET_DIGITS)) ? GET_ERROR : flags;
	c = (isascii((*s)[0]) && isupper((*s)[0])) ? tolower((*s)[0]) : (*s)[0];
	if (c == 'e' || c == 's' || c == 'f' || c == 'd' || c == 'l') {
		if (!(flags & GET_DIGITS))
			return	GET_ERROR;
		++(*s);
		if (--(*len) == 0)
			return	GET_ERROR;
		xsign = 0;
		if ((*s)[0] == '+' || (*s)[0] == '-') {
			if ((*s)[0] == '-')
				xsign = 1;
			++(*s);
			--(*len);
		}
		*exp = 0;
		while (*len > 0) {
			if (!isdigit((*s)[0]))
				break;
			*exp = *exp * 10 + (*s)[0] - '0';
			if (*exp >= BIGNUM_BUFFER_SIZE*16)
				return	GET_ERROR;
			flags |= GET_EXPONENT;
			++(*s);
			--(*len);
		}
		if (!(flags & GET_EXPONENT))
			return	GET_ERROR;
		if (xsign)
			*exp = -(*exp);
	}
	return	((flags & GET_POINT) && !(flags & GET_DIGITS)) ? GET_ERROR : flags;
}

static rk_object
scan_ureal(char **s, unsigned *len, int radix, int sign, int exactness)
{
	int flags, flags1, exp;
	unsigned xr, nr, dr, *rp, wr, ww;
	double x, a, b;
	jmp_buf fpe;

	if ((flags = scan_uint_or_decimal(0, 1, s, len, &exp, radix)) == GET_ERROR)
		return	RK_SOBJ_FALSE;
	if (*len > 0 && (*s)[0] == '/') {
		++(*s);
		--(*len);
		if (!(flags & GET_DIGITS) || (flags & (GET_POINT | GET_EXPONENT)))
			return	RK_SOBJ_FALSE;
		if ((flags1 = scan_uint_or_decimal(1, 2, s, len, &exp, radix)) == GET_ERROR)
			return	RK_SOBJ_FALSE;
		if (!(flags1 & GET_DIGITS) || (flags1 & (GET_POINT | GET_EXPONENT))
		 || bignum_register_size[1] == 1 && bignum_register[1][0] == 0)
			return	RK_SOBJ_FALSE;
		flags |= flags1;
	}
	if (!(flags & GET_DIGITS))
		return	RK_DUMMY_OBJ;
	if (sign == -1)
		bignum_register_sign[0] = 1;
	if (exactness == 1 || exactness == 0 && (flags & (GET_MESH | GET_POINT | GET_EXPONENT))) {
		if (setjmp(fpe)) {
			rk_sigfpe_catcher = NULL;
			return	RK_SOBJ_FALSE;
		}
		rk_sigfpe_catcher = &fpe;
		a = RkIntToFloat(0);
		b = RkIntToFloat(1);
		x = a / b;
		if (flags & GET_EXPONENT)
			x *= pow((double)radix, (double)exp);
		rk_sigfpe_catcher = NULL;
		return	RkStoreFloat(x);
	}
	nr = 0;
	dr = 1;
	if (flags & GET_EXPONENT) {
		RkLoadShortInt(radix, 2);
		xr = 2;
		wr = 3;
		if (exp < 0) {
			exp = -exp;
			rp = &dr;
		} else
			rp = &nr;
		while (exp) {
			if (exp & 1) {
				if (!RkMultiplyIntByInt(*rp, xr, wr))
					return	RK_SOBJ_FALSE;
				ww = *rp, *rp = wr, wr = ww;
			}
			if (exp >>= 1) {
				if (!RkMultiplyIntByInt(xr, xr, wr))
					return	RK_SOBJ_FALSE;
				ww = xr, xr = wr, wr = ww;
			}
		}
	}
	return	RkMakeExactFraction(nr, dr);
}

rk_object
RkReadNumber(char *s, unsigned len, int radix)
{
	static struct { int c, exactness, radix; } prefixes[] = {
		{'i', 1, 0}, {'e', 2, 0}, {'b', 0, 2}, {'o', 0, 8}, {'d', 0, 10}, {'x', 0, 16}, {'\0', -1, -1},
	};
	int c, i, exactness, xradix, sign;
	rk_object obj, *cp;
	double magnitude, angle, re, im;
	jmp_buf fpe;

	exactness = 0;
	xradix = 0;
	while (len > 0 && s[0] == '#') {
		if (len == 1)
			return	RK_SOBJ_FALSE;
		c = (isascii(s[1]) && isupper(s[1])) ? tolower(s[1]) : s[1];
		prefixes[sizeof(prefixes)/sizeof(prefixes[0])-1].c = c;
		for (i = 0; prefixes[i].c != c; ++i) ;
		if (prefixes[i].exactness == -1)
			return	RK_SOBJ_FALSE;
		if (prefixes[i].exactness) {
			if (exactness)
				return	RK_SOBJ_FALSE;
			exactness = prefixes[i].exactness;
		}
		if (prefixes[i].radix) {
			if (xradix)
				return	RK_SOBJ_FALSE;
			xradix = prefixes[i].radix;
		}
		s += 2;
		len -= 2;
	}
	if (xradix)
		radix = xradix;
	if (!radix)
		radix = 10;
	if (len == 0)
		return	RK_SOBJ_FALSE;
	if (s[0] == '+' || s[0] == '-') {
		sign = (s[0] == '+') ? 1 : -1;
		++s;
		if (!(--len))
			return	RK_SOBJ_FALSE;
	} else
		sign = 0;
	if ((obj = scan_ureal(&s, &len, radix, sign, exactness)) == RK_SOBJ_FALSE)
		return	RK_SOBJ_FALSE;
	if (len == 0)
		return	(obj == RK_DUMMY_OBJ) ? RK_SOBJ_FALSE : obj;
	if (s[0] == 'i' || s[0] == 'I') {
		if (len != 1 || !sign)
			return	RK_SOBJ_FALSE;
		if (obj == RK_DUMMY_OBJ)
			if (exactness == 1)
				obj = RkStoreFloat((double)sign);
			else
				obj = RK_MAKEINUM(sign);
		if (RkZeroP(obj))
			return	obj;
		rk_eval_register[rk_valid_register++] = obj;
		if (RkExactP(obj)) {
			obj = RK_MAKEINUM(0);
			rk_eval_register[rk_valid_register++] = obj;
		} else {
			obj = RkStoreFloat(0.0);
			rk_eval_register[rk_valid_register++] = obj;
		}
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
		cp[1] = rk_eval_register[rk_valid_register - 1];
		cp[2] = rk_eval_register[rk_valid_register - 2];
		cp[3] = RK_DUMMY_OBJ;
		rk_valid_register -= 2;
		return	(rk_object)cp;
	}
	if (obj == RK_DUMMY_OBJ)
		return	RK_SOBJ_FALSE;
	if (s[0] == '+' || s[0] == '-') {
		sign = (s[0] == '+') ? 1 : -1;
		++s;
		if (!(--len))
			return	RK_SOBJ_FALSE;
		rk_eval_register[rk_valid_register++] = obj;
		if ((obj = scan_ureal(&s, &len, radix, sign, exactness)) == RK_SOBJ_FALSE
		 || len != 1 || s[0] != 'i' && s[0] != 'I') {
			--rk_valid_register;
			return	RK_SOBJ_FALSE;
		}
		if (obj == RK_DUMMY_OBJ)
			if (exactness == 1)
				obj = RkStoreFloat((double)sign);
			else
				obj = RK_MAKEINUM(sign);
		rk_eval_register[rk_valid_register++] = obj;
		if (setjmp(fpe)) {
			rk_sigfpe_catcher = NULL;
			rk_valid_register -= 2;
			return	RK_SOBJ_FALSE;
		}
		rk_sigfpe_catcher = &fpe;
		if (!RkExactP(rk_eval_register[rk_valid_register - 2])
		 && RkExactP(rk_eval_register[rk_valid_register - 1])) {
			magnitude = RkConvertToFloat(rk_eval_register[rk_valid_register - 1]);
			rk_eval_register[rk_valid_register - 1] = RkStoreFloat(magnitude);
		} else if (!RkExactP(rk_eval_register[rk_valid_register - 1])
			&& RkExactP(rk_eval_register[rk_valid_register - 2])) {
			magnitude = RkConvertToFloat(rk_eval_register[rk_valid_register - 2]);
			rk_eval_register[rk_valid_register - 2] = RkStoreFloat(magnitude);
		}
		rk_sigfpe_catcher = NULL;
		if (RkZeroP(rk_eval_register[rk_valid_register - 1])) {
			obj = rk_eval_register[rk_valid_register - 2];
			rk_valid_register -= 2;
			return	obj;
		}
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
		cp[1] = rk_eval_register[rk_valid_register - 2];
		cp[2] = rk_eval_register[rk_valid_register - 1];
		cp[3] = RK_DUMMY_OBJ;
		rk_valid_register -= 2;
		return	(rk_object)cp;
	}
	if (s[0] != '@')
		return	RK_SOBJ_FALSE;
	++s;
	if (!(--len))
		return	RK_SOBJ_FALSE;
	if (setjmp(fpe)) {
		rk_sigfpe_catcher = NULL;
		return	RK_SOBJ_FALSE;
	}
	rk_sigfpe_catcher = &fpe;
	magnitude = RkConvertToFloat(obj);
	rk_sigfpe_catcher = NULL;
	if (s[0] == '+' || s[0] == '-') {
		sign = (s[0] == '+') ? 1 : -1;
		++s;
		if (!(--len))
			return	RK_SOBJ_FALSE;
	} else
		sign = 0;
	if ((obj = scan_ureal(&s, &len, radix, sign, 1)) == RK_SOBJ_FALSE || obj == RK_DUMMY_OBJ || len != 0)
		return	RK_SOBJ_FALSE;
	angle = RkLoadFloat(obj);
	if (setjmp(fpe)) {
		rk_sigfpe_catcher = NULL;
		return	RK_SOBJ_FALSE;
	}
	rk_sigfpe_catcher = &fpe;
	re = magnitude * cos(angle);
	im = magnitude * sin(angle);
	rk_sigfpe_catcher = NULL;
	if (exactness == 2) {
		if (!RK_FINITE(re) || !RK_FINITE(im))
			return	RK_SOBJ_FALSE;
		if (sign = (re < 0.0))
			re = -re;
		RkConvertToFraction(re, 0, 1);
		if ((obj = RkStoreInt(1)) == RK_MAKEINUM(1)) {
			if (sign)
				RkNegateInt(0);
			obj = RkStoreInt(0);
			rk_eval_register[rk_valid_register++] = obj;
			rk_eval_register[rk_valid_register++] = RK_DUMMY_OBJ;
		} else {
			rk_eval_register[rk_valid_register++] = obj;
			obj = RkStoreInt(0);
			rk_eval_register[rk_valid_register++] = obj;
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
			cp[1] = RK_MAKEINUM(sign);
			cp[2] = rk_eval_register[rk_valid_register - 1];
			cp[3] = rk_eval_register[rk_valid_register - 2];
			rk_eval_register[rk_valid_register - 2] = (rk_object)cp;
		}
		if (sign = (im < 0.0))
			im = -im;
		RkConvertToFraction(im, 0, 1);
		if ((obj = RkStoreInt(1)) == RK_MAKEINUM(1)) {
			if (sign)
				RkNegateInt(0);
			obj = RkStoreInt(0);
			rk_eval_register[rk_valid_register - 1] = RkStoreInt(0);
		} else {
			rk_eval_register[rk_valid_register - 1] = obj;
			obj = RkStoreInt(0);
			rk_eval_register[rk_valid_register++] = obj;
			cp = RkAllocCells(4);
			cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
			cp[1] = RK_MAKEINUM(sign);
			cp[2] = rk_eval_register[rk_valid_register - 1];
			cp[3] = rk_eval_register[rk_valid_register - 2];
			rk_eval_register[rk_valid_register - 2] = (rk_object)cp;
			--rk_valid_register;
		}
	} else {
		obj = RkStoreFloat(re);
		rk_eval_register[rk_valid_register++] = obj;
		obj = RkStoreFloat(im);
		rk_eval_register[rk_valid_register++] = obj;
	}
	if (RkZeroP(rk_eval_register[rk_valid_register - 1])) {
		obj = rk_eval_register[rk_valid_register - 2];
		rk_valid_register -= 2;
		return	obj;
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_COMPLEX);
	cp[1] = rk_eval_register[rk_valid_register - 2];
	cp[2] = rk_eval_register[rk_valid_register - 1];
	cp[3] = RK_DUMMY_OBJ;
	rk_valid_register -= 2;
	return	(rk_object)cp;
}

static char *cnv_buf = NULL, *cnv_buf_tail, *cnv_buf_ptr;
static unsigned cnv_buf_size;

static int
add_character(int c)
{
	if (!cnv_buf) {
		if (!(cnv_buf = malloc(cnv_buf_size = 256)))
			return	0;
		cnv_buf_ptr = cnv_buf;
		cnv_buf_tail = cnv_buf + cnv_buf_size;
	}
	if (cnv_buf_ptr == cnv_buf_tail) {
		if (!(cnv_buf = realloc(cnv_buf, cnv_buf_size * 2))) {
			cnv_buf = cnv_buf_tail - cnv_buf_size;
			return	0;
		}
		cnv_buf_ptr = cnv_buf + cnv_buf_size;
		cnv_buf_tail = cnv_buf + (cnv_buf_size *= 2);
	}
	*cnv_buf_ptr++ = c;
	return	1;
}

static int
print_inum(int n, int radix)
{
	int d, s;

	s = 0;
	if (n < 0) {
		s = 1;
		n = -n;
	}
	do {
		d = n % radix;
		n = n / radix;
		if (!add_character(d < 10 ? d + '0' : d - 10 + 'a'))
			return	0;
	} while (n);
	if (s)
		return	add_character('-');
	return	1;
}

static int
print_integer(rk_object num, int radix)
{
	int d;

	if (RK_ISINUM(num))
		return	print_inum(RK_GETINUM(num), radix);
	RkLoadBigInt(num, 0);
	do {
		d = RkDivideIntByUshort(0, radix);
		if (!add_character(d < 10 ? d + '0' : d - 10 + 'a'))
			return	0;
	} while (bignum_register_size[0] > 1 || bignum_register[0][0]);
	if (bignum_register_sign[0])
		return	add_character('-');
	return	1;
}

static int
print_exact_real(rk_object num, int radix)
{
	if (!RK_ISINUM(num) && (((rk_object *)num)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_FRACTION)) {
		if (!print_integer(((rk_object *)num)[3], radix))
			return	0;
		if (!add_character('/'))
			return	0;
		if (!print_integer(((rk_object *)num)[2], radix))
			return	0;
		if (RK_GETINUM(((rk_object *)num)[1]))
			return	add_character('-');
		return	1;
	}
	return	print_integer(num, radix);
}

static int
print_real(rk_object num, int radix)
{
	static char buf[64];
	double x;
	int i;

	if (!RK_ISINUM(num) && (((rk_object *)num)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_FLONUM)) {
		x = RkLoadFloat(num);
		if (RK_FINITE(x)) {
			sprintf(buf, "%.16g", x);
			if (!index(buf, '.') && !index(buf, 'e'))
				sprintf(&buf[strlen(buf)], "%c", '.');
		} else
			sprintf(buf, "@%s@", RK_ISNAN(x) ? "NaN" : "Inf");
		i = strlen(buf);
		while (--i >= 0)
			if (!add_character(buf[i]))
				return	0;
		return	1;
	}
	return	print_exact_real(num, radix);
}

char *
RkPrintNumber(rk_object num, int radix)
{
	cnv_buf_ptr = cnv_buf;

	if (!RK_ISINUM(num) && (((rk_object *)num)[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_COMPLEX)) {
		if (!add_character('i'))
			return	NULL;
		if (!print_real(RkAbsoluteValue(((rk_object *)num)[2]), radix))
			return	NULL;
		if (!add_character(RkSignature(((rk_object *)num)[2]) ? '-' : '+'))
			return	NULL;
		if (!print_real(((rk_object *)num)[1], radix))
			return	NULL;
	} else
		if (!print_real(num, radix))
			return	NULL;
	if (!add_character('\0'))
		return	NULL;
	return	cnv_buf;
}

rk_object
RkAbsoluteValue(rk_object num)
{
	rk_object *cp;
	double x;

	if (RK_ISINUM(num))
		if (RK_GETINUM(num) < 0) {
			RkLoadShortInt(RK_GETINUM(num), 0);
			bignum_register_sign[0] = 0;
			return	RkStoreInt(0);
		} else
			return	num;
	switch (((rk_object *)num)[0] & 0xfff) {
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
		return	num;
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		RkLoadBigInt(num, 0);
		bignum_register_sign[0] = 0;
		return	RkStoreInt(0);
	case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		if (!RK_GETINUM(((rk_object *)num)[1]))
			return	num;
		rk_eval_register[rk_valid_register++] = num;
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
		cp[1] = RK_MAKEINUM(0);
		cp[2] = ((rk_object *)rk_eval_register[rk_valid_register - 1])[2];
		cp[3] = ((rk_object *)rk_eval_register[rk_valid_register - 1])[3];
		--rk_valid_register;
		return	(rk_object)cp;
	case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		x = RkLoadFloat(num);
		return	RkStoreFloat(fabs(x));
	default:
		return	num;
	}
}

int
RkSignature(rk_object num)
{
	if (RK_ISINUM(num))
		return	RK_GETINUM(num) < 0;
	switch (((rk_object *)num)[0] & 0xfff) {
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
	default:
		return	0;
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		return	1;
	case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		return	 RK_GETINUM(((rk_object *)num)[1]);
	case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		return	RkLoadFloat(num) < 0;
	}
}

int
RkZeroP(rk_object num)
{
	if (RK_ISINUM(num))
		return	RK_GETINUM(num) == 0;
	switch (((rk_object *)num)[0] & 0xfff) {
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
	case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
	default:
		return	0;
	case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		return	RkLoadFloat(num) == 0.0;
	}
}

int
RkExactP(rk_object num)
{
	if (RK_ISINUM(num))
		return	1;
	switch (((rk_object *)num)[0] & 0xfff) {
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
	case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		return	1;
	case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
	default:
		return	0;
	case RK_VECTOR_TAG(0, RK_TCODE_COMPLEX):
		return	RkExactP(((rk_object *)num)[1]);
	}
}

void
RkCopyInt(unsigned sr, unsigned tr)
{
	int i;

	bignum_register_sign[tr] = bignum_register_sign[sr];
	for (i = 0; i < bignum_register_size[sr]; ++i)
		bignum_register[tr][i] = bignum_register[sr][i];
	bignum_register_size[tr] = bignum_register_size[sr];
}

int
RkAddIntByUshort(unsigned reg, unsigned long n)
{
	int i;

	i = 0;
	while (i < bignum_register_size[reg]) {
		n += bignum_register[reg][i];
		bignum_register[reg][i++] = n;
		if ((n >>= 16) == 0)
			return	1;
	}
	if (i == BIGNUM_BUFFER_SIZE)
		return	0;
	bignum_register[reg][i] = n;
	++bignum_register_size[reg];
	return	1;
}

int
RkMultiplyIntByUshort(unsigned reg, unsigned long n)
{
	int i;
	unsigned long m;

	if (n == 0) {
		bignum_register_sign[reg] = 0;
		bignum_register_size[reg] = 1;
		bignum_register[reg][0] = 0;
		return	1;
	}
	i = 0;
	m = 0;
	while (i < bignum_register_size[reg]) {
		m += bignum_register[reg][i] * n;
		bignum_register[reg][i++] = m;
		m >>= 16;
	}
	if (m) {
		if (i == BIGNUM_BUFFER_SIZE)
			return	0;
		bignum_register[reg][i] = m;
		++bignum_register_size[reg];
	}
	return	1;
}

unsigned
RkDivideIntByUshort(unsigned reg, unsigned long n)
{
	int i;
	unsigned long m;

	i = bignum_register_size[reg];
	m = 0;
	while (--i >= 0) {
		m = (m << 16) + bignum_register[reg][i];
		bignum_register[reg][i] = m / n;
		m = m % n;
	}
	if (bignum_register_size[reg] > 1 && bignum_register[reg][bignum_register_size[reg] - 1] == 0)
		--bignum_register_size[reg];
	return	m;
}

void
RkNegateInt(unsigned reg)
{
	bignum_register_sign[reg] = !bignum_register_sign[reg];
}

int
RkIntegerIsZero(unsigned reg)
{
	return	bignum_register_size[reg] == 1 && bignum_register[reg][0] == 0;
}

int
RkIntegerIsNegative(unsigned reg)
{
	return	bignum_register_sign[reg];
}

int
RkCompareIntByInt(unsigned sr1, unsigned sr2)
{
	int i;

	if (!bignum_register_sign[sr1]) {
		if (bignum_register_sign[sr2])
			return	1;
		if (bignum_register_size[sr1] > bignum_register_size[sr2])
			return	1;
		if (bignum_register_size[sr1] < bignum_register_size[sr2])
			return	-1;
		for (i = bignum_register_size[sr1] - 1; i >= 0; --i) {
			if (bignum_register[sr1][i] > bignum_register[sr2][i])
				return	1;
			if (bignum_register[sr1][i] < bignum_register[sr2][i])
				return	-1;
		}
		return	0;
	} else {
		if (!bignum_register_sign[sr2])
			return	-1;
		if (bignum_register_size[sr1] > bignum_register_size[sr2])
			return	-1;
		if (bignum_register_size[sr1] < bignum_register_size[sr2])
			return	1;
		for (i = bignum_register_size[sr1] - 1; i >= 0; --i) {
			if (bignum_register[sr1][i] > bignum_register[sr2][i])
				return	-1;
			if (bignum_register[sr1][i] < bignum_register[sr2][i])
				return	1;
		}
		return	0;
	}
}

static int
add_uint(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i;
	unsigned long n;

	if (bignum_register_size[sr2] > bignum_register_size[sr1])
		return	add_uint(sr2, sr1, tr);
	bignum_register_sign[tr] = 0;
	n = 0;
	for (i = 0; i < bignum_register_size[sr2]; ++i) {
		n += bignum_register[sr1][i] + bignum_register[sr2][i];
		bignum_register[tr][i] = n;
		n >>= 16;
	}
	while (i < bignum_register_size[sr1]) {
		n += bignum_register[sr1][i];
		bignum_register[tr][i++] = n;
		n >>= 16;
	}
	if (n) {
		if (bignum_register_size[sr1] == BIGNUM_BUFFER_SIZE)
			return	0;
		bignum_register[tr][i] = n;
		bignum_register_size[tr] = i + 1;
		return	1;
	}
	bignum_register_size[tr] = i;
	return	1;
}

static void
subtract_uint(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i;
	unsigned long n;

	bignum_register_sign[tr] = 0;
	n = 0;
	for (i = 0; i < bignum_register_size[sr2]; ++i) {
		n += bignum_register[sr2][i];
		if (bignum_register[sr1][i] >= n) {
			bignum_register[tr][i] = bignum_register[sr1][i] - n;
			n = 0;
		} else {
			n -= bignum_register[sr1][i];
			bignum_register[tr][i] = -n;
			if (n & 0xffff)
				n = (n >> 16) + 1;
			else
				n = (n >> 16);
		}
	}
	while (i < bignum_register_size[sr1]) {
		if (bignum_register[sr1][i] >= n) {
			bignum_register[tr][i] = bignum_register[sr1][i] - n;
			n = 0;
		} else {
			n -= bignum_register[sr1][i];
			bignum_register[tr][i] = -n;
			if (n & 0xffff)
				n = (n >> 16) + 1;
			else
				n = (n >> 16);
		}
		++i;
	}
	for (i = bignum_register_size[sr1] - 1; i >= 1; --i)
		if (bignum_register[tr][i]) {
			bignum_register_size[tr] = i + 1;
			return;
		}
	bignum_register_size[tr] = 1;
}

int
RkAddIntByInt(unsigned sr1, unsigned sr2, unsigned tr)
{
	if (!bignum_register_sign[sr1]) {
		if (bignum_register_sign[sr2]) {
			bignum_register_sign[sr2] = 0;
			switch (RkCompareIntByInt(sr1, sr2)) {
			case 1:
				subtract_uint(sr1, sr2, tr);
				return	1;
			case 0:
				bignum_register_sign[tr] = 0;
				bignum_register[tr][0] = 0;
				bignum_register_size[tr] = 1;
				return 1;
			case -1:
				subtract_uint(sr2, sr1, tr);
				bignum_register_sign[tr] = 1;
				return	1;
			}
		}
		return	add_uint(sr1, sr2, tr);
	} else {
		if (!bignum_register_sign[sr2]) {
			bignum_register_sign[sr1] = 0;
			switch (RkCompareIntByInt(sr1, sr2)) {
			case 1:
				subtract_uint(sr1, sr2, tr);
				bignum_register_sign[tr] = 1;
				return	1;
			case 0:
				bignum_register_sign[tr] = 0;
				bignum_register[tr][0] = 0;
				bignum_register_size[tr] = 1;
				return 1;
			case -1:
				subtract_uint(sr2, sr1, tr);
				return	1;
			}
		}
		if (!add_uint(sr1, sr2, tr))
			return	0;
		bignum_register_sign[tr] = 1;
		return	1;
	}
}

int
RkMultiplyIntByInt(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i, j;
	unsigned long n;

	if (bignum_register_size[sr1] == 1 && bignum_register[sr1][0] == 0
	 || bignum_register_size[sr2] == 1 && bignum_register[sr2][0] == 0) {
		bignum_register_sign[tr] = 0;
		bignum_register_size[tr] = 1;
		bignum_register[tr][0] = 0;
		return	1;
	}
	bignum_register_size[tr] = bignum_register_size[sr1] + bignum_register_size[sr2];
	if (bignum_register_size[tr] > BIGNUM_BUFFER_SIZE)
		return	0;
	for (i = 0; i < bignum_register_size[sr1]; ++i)
		bignum_register[tr][i] = 0;
	for (j = 0; j < bignum_register_size[sr2]; ++j) {
		n = 0;
		for (i = 0; i < bignum_register_size[sr1]; ++i) {
			n += bignum_register[tr][i + j] + bignum_register[sr1][i] * bignum_register[sr2][j];
			bignum_register[tr][i + j] = n;
			n >>= 16;
		}
		bignum_register[tr][i + j] = n;
	}
	if (!n)
		--bignum_register_size[tr];
	bignum_register_sign[tr] = (bignum_register_sign[sr1] + bignum_register_sign[sr2]) & 1;
	return	1;
}

void
RkDivideIntByInt(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i, j, shift;
	unsigned long m, n, l;

	if (bignum_register_size[sr2] == 1) {
		n = bignum_register[sr2][0];
		i = bignum_register_size[sr1];
		m = 0;
		while (--i >= 0) {
			m = (m << 16) + bignum_register[sr1][i];
			bignum_register[tr][i] = m / n;
			m = m % n;
		}
		bignum_register_size[tr] = bignum_register_size[sr1];
		if (bignum_register_size[tr] > 1 && bignum_register[tr][bignum_register_size[tr] - 1] == 0)
			--bignum_register_size[tr];
		bignum_register_size[sr1] = 1;
		bignum_register[sr1][0] = m;
	} else if (bignum_register_size[sr1] < bignum_register_size[sr2]) {
		bignum_register[tr][0] = 0;
		bignum_register_size[tr] = 1;
	} else {
		n = bignum_register[sr2][bignum_register_size[sr2] - 1];
		for (shift = 0; !(n & 0x8000); ++shift)
			n <<= 1;
		m = 0;
		for (i = 0; i < bignum_register_size[sr2]; ++i) {
			m |= bignum_register[sr2][i] << shift;
			bignum_register[sr2][i] = m;
			m >>= 16;
		}
		m = 0;
		for (i = 0; i < bignum_register_size[sr1]; ++i) {
			m |= bignum_register[sr1][i] << shift;
			bignum_register[sr1][i] = m;
			m >>= 16;
		}
		for (j = bignum_register_size[sr1] - bignum_register_size[sr2]; j >= 0; --j) {
			if (m == bignum_register[sr2][bignum_register_size[sr2] - 1])
				n = 0xffff;
			else
				n = (((m << 16) | bignum_register[sr1][j + bignum_register_size[sr2] - 1])
				   / bignum_register[sr2][bignum_register_size[sr2] - 1]);
			for (; ; ) {
				l = (((m << 16) | bignum_register[sr1][j + bignum_register_size[sr2] - 1])
				     - n * bignum_register[sr2][bignum_register_size[sr2] - 1]);
				if (l >> 16)
					break;
				if (n * bignum_register[sr2][bignum_register_size[sr2] - 2]
				 <= (l << 16) | bignum_register[sr1][j + bignum_register_size[sr2] - 2])
					break;
				--n;
			}
			l = 0;
			for (i = 0; i < bignum_register_size[sr2]; ++i) {
				l += bignum_register[sr2][i] * n;
				if (l <= bignum_register[sr1][i + j]) {
					bignum_register[sr1][i + j] -= l;
					l = 0;
				} else {
					l -= bignum_register[sr1][i + j];
					bignum_register[sr1][i + j] = -l;
					if (l & 0xffff)
						l = (l >> 16) + 1;
					else
						l = (l >> 16);
				}
			}
			if (l > m) {
				--n;
				l = 0;
				for (i = 0; i < bignum_register_size[sr2]; ++i) {
					l += bignum_register[sr2][i] + bignum_register[sr1][i + j];
					bignum_register[sr1][i + j] = l;
					l >>= 16;
				}
			}
			bignum_register[tr][j] = n;
			m = bignum_register[sr1][j + bignum_register_size[sr2] - 1];
		}
		j = bignum_register_size[sr1] - bignum_register_size[sr2];
		if (j == 0 || bignum_register[tr][j])
			bignum_register_size[tr] = j + 1;
		else
			bignum_register_size[tr] = j;
		for (i = 0; i < bignum_register_size[sr2] - 1; ++i)
			bignum_register[sr1][i] = ((bignum_register[sr1][i + 1] << 16) | bignum_register[sr1][i])
						 >> shift;
		bignum_register[sr1][i] >>= shift;
		for ( ; i > 0 && !bignum_register[sr1][i]; --i) ;
		bignum_register_size[sr1] = i + 1;
	}
	bignum_register_sign[tr] = (bignum_register_sign[sr1] + bignum_register_sign[sr2]) & 1;
}

unsigned
RkComputeGCD(unsigned sr1, unsigned sr2, unsigned wr1, unsigned wr2)
{
	unsigned ww;

	bignum_register_sign[sr1] = bignum_register_sign[sr2] = 0;
	while (bignum_register_size[sr2] > 1 || bignum_register[sr2][0]) {
		RkCopyInt(sr2, wr1);
		RkDivideIntByInt(sr1, sr2, wr2);
		ww = wr1, wr1 = sr2, sr2 = sr1, sr1 = ww;
	}
	return	sr1;
}

rk_object
RkMakeExactFraction(unsigned nr, unsigned dr)
{
	unsigned r1, r2, r3, r4, gr, gr1;
	int sign;
	rk_object *cp, obj;

	sign = (bignum_register_sign[nr] + bignum_register_sign[dr]) & 1;
	bignum_register_sign[nr] = 0;
	bignum_register_sign[dr] = 0;
	for (r1 = 0; r1 == nr || r1 == dr; ++r1) ;
	for (r2 = r1 + 1; r2 == nr || r2 == dr; ++r2) ;
	for (r3 = r2 + 1; r3 == nr || r3 == dr; ++r3) ;
	for (r4 = r3 + 1; r4 == nr || r4 == dr; ++r4) ;
	RkCopyInt(nr, r1);
	RkCopyInt(dr, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	if (gr == r1)
		gr1 = r2;
	else
		gr1 = r1;
	RkCopyInt(gr, gr1);
	RkDivideIntByInt(dr, gr1, r4);
	if (bignum_register_size[r4] == 1 && bignum_register[r4][0] == 1) {
		RkDivideIntByInt(nr, gr, r4);
		bignum_register_sign[r4] = sign;
		return	RkStoreInt(r4);
	}
	obj = RkStoreInt(r4);
	rk_eval_register[rk_valid_register++] = obj;
	RkDivideIntByInt(nr, gr, r4);
	obj = RkStoreInt(r4);
	rk_eval_register[rk_valid_register++] = obj;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
	cp[1] = RK_MAKEINUM(sign);
	cp[2] = rk_eval_register[rk_valid_register - 1];
	cp[3] = rk_eval_register[rk_valid_register - 2];
	rk_valid_register -= 2;
	return	(rk_object)cp;
}

int
RkCompareFraction(unsigned n1, unsigned d1, unsigned n2, unsigned d2)
{
	unsigned r1, r2, r3, r4, gr;

	if (!bignum_register_sign[n1] && bignum_register_sign[n2])
		return	1;
	if (bignum_register_sign[n1] && !bignum_register_sign[n2])
		return	-1;
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	if (RkMultiplyIntByInt(n1, d2, r1) && RkMultiplyIntByInt(n2, d1, r2))
		return	RkCompareIntByInt(r1, r2);
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
	for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
	RkCopyInt(n1, r1);
	RkCopyInt(n2, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2 || r2 == gr; ++r2) ;
	RkCopyInt(gr, r1);
	RkDivideIntByInt(n1, r1, r2);
	RkDivideIntByInt(n2, gr, r1);
	n1 = r2;
	n2 = r1;
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
	for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
	RkCopyInt(d1, r1);
	RkCopyInt(d2, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2 || r2 == gr; ++r2) ;
	RkCopyInt(gr, r1);
	RkDivideIntByInt(d1, r1, r2);
	RkDivideIntByInt(d2, gr, r1);
	d1 = r2;
	d2 = r1;
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	if (RkMultiplyIntByInt(n1, d2, r1) && RkMultiplyIntByInt(n2, d1, r2))
		return	RkCompareIntByInt(r1, r2);
	return	2;
}

int
RkAddFraction(unsigned n1, unsigned d1, unsigned n2, unsigned d2)
{
	unsigned r1, r2, r3, r4, gr;
	int sign;
	rk_object *cp, obj;

	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
	for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
	RkCopyInt(d1, r1);
	RkCopyInt(d2, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	if (bignum_register_size[gr] == 1 && bignum_register[gr][0] == 1) {
		if (!RkMultiplyIntByInt(n1, d2, r1) || !RkMultiplyIntByInt(n2, d1, r2) || !RkAddIntByInt(r1, r2, n1)
		 || !RkMultiplyIntByInt(d1, d2, n2))
			return	RK_SOBJ_ERROR;
	} else {
		for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
		for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2 || r2 == gr; ++r2) ;
		for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2 || r3 == gr; ++r3) ;
		RkCopyInt(gr, r1);
		RkDivideIntByInt(d1, r1, r2);
		RkCopyInt(gr, r1);
		RkCopyInt(d2, d1);
		RkDivideIntByInt(d1, r1, r3);
		if (!RkMultiplyIntByInt(n1, r3, r1) || !RkMultiplyIntByInt(n2, r2, r3) || !RkAddIntByInt(r1, r3, n1))
			return	RK_SOBJ_ERROR;
		n2 = gr, d1 = r2;
		for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
		for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
		for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
		for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
		RkCopyInt(n1, r1);
		RkCopyInt(n2, r2);
		gr = RkComputeGCD(r1, r2, r3, r4);
		if (bignum_register_size[gr] == 1 && bignum_register[gr][0] == 1) {
			if (!RkMultiplyIntByInt(d1, d2, n2))
				return	RK_SOBJ_ERROR;
		} else {
			for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
			RkCopyInt(gr, r1);
			RkDivideIntByInt(n1, r1, n2);
			RkDivideIntByInt(d2, gr, n1);
			if (!RkMultiplyIntByInt(d1, n1, d2))
				return	RK_SOBJ_ERROR;
			if (bignum_register_size[d2] == 1 && bignum_register[d2][0] == 1)
				return	RkStoreInt(n2);
			r2 = n2, n2 = d2, n1 = r2;
		}
	}
	sign = bignum_register_sign[n1];
	bignum_register_sign[n1] = 0;
	obj = RkStoreInt(n1);
	rk_eval_register[rk_valid_register++] = obj;
	obj = RkStoreInt(n2);
	rk_eval_register[rk_valid_register++] = obj;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
	cp[1] = RK_MAKEINUM(sign);
	cp[2] = rk_eval_register[rk_valid_register - 2];
	cp[3] = rk_eval_register[rk_valid_register - 1];
	rk_valid_register -= 2;
	return	(rk_object)cp;
}

int
RkMultiplyFraction(unsigned n1, unsigned d1, unsigned n2, unsigned d2)
{
	unsigned r1, r2, r3, r4, gr;
	int sign;
	rk_object *cp, obj;

	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
	for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
	RkCopyInt(n1, r1);
	RkCopyInt(d2, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2 || r2 == gr; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2 || r3 == gr; ++r3) ;
	RkCopyInt(gr, r1);
	RkDivideIntByInt(n1, r1, r2);
	RkDivideIntByInt(d2, gr, r3);
	n1 = r2, d2 = r3;
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2; ++r3) ;
	for (r4 = r3 + 1; r4 == n1 || r4 == d1 || r4 == n2 || r4 == d2; ++r4) ;
	RkCopyInt(n2, r1);
	RkCopyInt(d1, r2);
	gr = RkComputeGCD(r1, r2, r3, r4);
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2 || r1 == gr; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2 || r2 == gr; ++r2) ;
	for (r3 = r2 + 1; r3 == n1 || r3 == d1 || r3 == n2 || r3 == d2 || r3 == gr; ++r3) ;
	RkCopyInt(gr, r1);
	RkDivideIntByInt(n2, r1, r2);
	RkDivideIntByInt(d1, gr, r3);
	n2 = r2, d1 = r3;
	for (r1 = 0; r1 == n1 || r1 == d1 || r1 == n2 || r1 == d2; ++r1) ;
	for (r2 = r1 + 1; r2 == n1 || r2 == d1 || r2 == n2 || r2 == d2; ++r2) ;
	if (!RkMultiplyIntByInt(n1, n2, r1) || !RkMultiplyIntByInt(d1, d2, r2))
		return	RK_SOBJ_ERROR;
	if (bignum_register_size[r2] == 1 && bignum_register[r2][0] == 1)
		return	RkStoreInt(r1);
	sign = bignum_register_sign[r1];
	bignum_register_sign[r1] = 0;
	obj = RkStoreInt(r1);
	rk_eval_register[rk_valid_register++] = obj;
	obj = RkStoreInt(r2);
	rk_eval_register[rk_valid_register++] = obj;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, RK_TCODE_FRACTION);
	cp[1] = RK_MAKEINUM(sign);
	cp[2] = rk_eval_register[rk_valid_register - 2];
	cp[3] = rk_eval_register[rk_valid_register - 1];
	rk_valid_register -= 2;
	return	(rk_object)cp;
}

#ifdef RK_NO_IEEE754_SUPPORT
# ifdef _MSC_VER
#  define scalbn(x, n)	_scalb(x, n)
# else
static double
scalbn(double x, int n)
{
	double s;

	if (n >= 0)
		s = 2.0;
	else {
		s = 0.5;
		n = -n;
	}
	while (n) {
		if (n & 1)
			x *= s;
		n >>= 1;
		s = s * s;
	}
	return	x;
}
# endif
#endif

#ifdef __BORLANDC__
int
RkFinite(double x)
{
	return	(((long *)&x)[1] & 0x7ff00000) != 0x7ff00000;
}

int
RkIsNaN(double x)
{
	return	(((long *)&x)[1] & 0x7ff00000) == 0x7ff00000
	     && ((((long *)&x)[1] & 0x000fffff) != 0 || ((long *)&x)[0] != 0);
}

/*ARGSUSED*/
int
_matherr(struct exception *e)
{
	return	1;
}
#endif

double
RkIntToFloat(unsigned reg)
{
	int i, rsz;
	long double x;

	rsz = bignum_register_size[reg];
	x = 0.0;
	for (i = 0; i < 6; ++i) {
		if (i >= rsz)
			return	bignum_register_sign[reg] ? -x : x;
		x = x * 65536.0L + (long double)bignum_register[reg][rsz - i - 1];
	}
	x = scalbn(x, (rsz - 6) * 16);
	return	bignum_register_sign[reg] ? -x : x;
}

double
RkConvertToFloat(rk_object num)
{
	double x, y;

	if (RK_ISINUM(num))
		return	(double)RK_GETINUM(num);
	switch (((rk_object *)num)[0] & 0xfff) {
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_POS):
	case RK_VECTOR_TAG(0, RK_TCODE_BIGINT_NEG):
		RkLoadBigInt(num, 0);
		return	RkIntToFloat(0);
	case RK_VECTOR_TAG(0, RK_TCODE_FRACTION):
		x = RkConvertToFloat(((rk_object *)num)[2]) / RkConvertToFloat(((rk_object *)num)[3]);
		return	RK_GETINUM(((rk_object *)num)[1]) ? -x : x;
	case RK_VECTOR_TAG(0, RK_TCODE_FLONUM):
		return	RkLoadFloat(num);
	default:
		return	0.0;
	}
}

#ifdef RK_MY_RINT
#define rint RkRint

double
rint(double x)
{
	double y;

	if (x >= 0.0) {
		y = floor(x);
		if (x - y == 0.5)
			return	(floor(y/2)*2 == y) ? y : y+1;
		return	floor(x+0.5);
	}
	return	-rint(-x);
}
#endif

void
RkConvertToFraction(double x, unsigned nreg, unsigned dreg)
{
	int i, k, l;
	double y;

	if (bignum_register_sign[nreg] = (x < 0.0))
		x = -x;
	bignum_register_sign[dreg] = 0;
	l = 0;
	if (x != floor(x)) {
#if !defined(RK_NO_IEEE754_SUPPORT) || defined(_MSC_VER)
# if !defined(_MSC_VER)
		l = ilogb(x);
# else
		l = (int)_logb(x);
# endif
		l = (l < 0) ? -l : 0;
#else
		l = 0;
#endif
		k = 64;
		while (y = scalbn(x, l), y != floor(y))
			l += k;
		while (k >>= 1) {
			y = scalbn(x, l-k);
			if (y == floor(y))
				l -= k;
		}
	}
	x = scalbn(x, l);
	i = 0;
	do {
		y = floor(x/65536.0);
		bignum_register[nreg][i++] = (unsigned short)rint(x - y*65536.0);
		x = y;
	} while (x != 0.0);
	bignum_register_size[nreg] = i;
	i = 0;
	while (l >= 16) {
		bignum_register[dreg][i++] = 0;
		l -= 16;
	}
	bignum_register[dreg][i++] = (1 << l);
	bignum_register_size[dreg] = i;
}

static void
bitwise_invert_clear_sign(unsigned reg)
{
	int i;

	for (i = 0; !bignum_register[reg][i]; ++i)
		bignum_register[reg][i] = 0xffff;
	if (!--bignum_register[reg][i]
	 && bignum_register_size[reg] == i+1
	 && bignum_register_size[reg] > 1)
		--bignum_register_size[reg];
}

static int
bitwise_invert_set_sign(unsigned reg)
{
	bignum_register_sign[reg] = 1;
	return	RkAddIntByUshort(reg, 1);
}

static void
bitwise_and(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i, s;

	if (bignum_register_size[sr2] > bignum_register_size[sr1]) {
		bitwise_and(sr2, sr1, tr);
		return;
	}
	for (s = i = 0; i < bignum_register_size[sr2]; ++i)
		if (bignum_register[tr][i] = bignum_register[sr1][i] & bignum_register[sr2][i])
			s = i;
	bignum_register_size[tr] = s+1;
}

static void
bitwise_neg_and(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i, s;

	if (bignum_register_size[sr2] > bignum_register_size[sr1]) {
		for (i = 0; i < bignum_register_size[sr1]; ++i)
			bignum_register[tr][i] = ~bignum_register[sr1][i] & bignum_register[sr2][i];
		for (; i < bignum_register_size[sr2]; ++i)
			bignum_register[tr][i] = bignum_register[sr2][i];
		bignum_register_size[tr] = bignum_register_size[sr2];
	} else {
		for (s = i = 0; i < bignum_register_size[sr2]; ++i)
			if (bignum_register[tr][i] = ~bignum_register[sr1][i] & bignum_register[sr2][i])
				s = i;
		bignum_register_size[tr] = s+1;
	}
}

static void
bitwise_or(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i;

	if (bignum_register_size[sr2] > bignum_register_size[sr1]) {
		bitwise_or(sr2, sr1, tr);
		return;
	}
	for (i = 0; i < bignum_register_size[sr2]; ++i)
		bignum_register[tr][i] = bignum_register[sr1][i] | bignum_register[sr2][i];
	for (; i < bignum_register_size[sr1]; ++i)
		bignum_register[tr][i] = bignum_register[sr1][i];
	bignum_register_size[tr] = bignum_register_size[sr1];
}

static void
bitwise_xor(unsigned sr1, unsigned sr2, unsigned tr)
{
	int i, s;

	if (bignum_register_size[sr2] > bignum_register_size[sr1]) {
		bitwise_xor(sr2, sr1, tr);
		return;
	}
	if (bignum_register_size[sr2] == bignum_register_size[sr1]) {
		for (s = i = 0; i < bignum_register_size[sr2]; ++i)
			if (bignum_register[tr][i] = bignum_register[sr1][i] ^ bignum_register[sr2][i])
				s = i;
		bignum_register_size[tr] = s+1;
	} else {
		for (i = 0; i < bignum_register_size[sr2]; ++i)
			bignum_register[tr][i] = bignum_register[sr1][i] ^ bignum_register[sr2][i];
		for (; i < bignum_register_size[sr1]; ++i)
			bignum_register[tr][i] = bignum_register[sr1][i];
		bignum_register_size[tr] = bignum_register_size[sr1];
	}
}

int
RkBitwiseAnd(unsigned sr1, unsigned sr2, unsigned tr)
{
	if (!bignum_register_sign[sr1]) {
		if (!bignum_register_sign[sr2]) {
			bitwise_and(sr1, sr2, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		} else {
			bitwise_invert_clear_sign(sr2);
			bitwise_neg_and(sr2, sr1, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		}
	} else {
		if (!bignum_register_sign[sr2]) {
			bitwise_invert_clear_sign(sr1);
			bitwise_neg_and(sr1, sr2, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		} else {
			bitwise_invert_clear_sign(sr1);
			bitwise_invert_clear_sign(sr2);
			bitwise_or(sr1, sr2, tr);
			return	bitwise_invert_set_sign(tr);
		}
	}
}

int
RkBitwiseOr(unsigned sr1, unsigned sr2, unsigned tr)
{
	if (!bignum_register_sign[sr1]) {
		if (!bignum_register_sign[sr2]) {
			bitwise_or(sr1, sr2, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		} else {
			bitwise_invert_clear_sign(sr2);
			bitwise_neg_and(sr1, sr2, tr);
			return  bitwise_invert_set_sign(tr);
		}
	} else {
		if (!bignum_register_sign[sr2]) {
			bitwise_invert_clear_sign(sr1);
			bitwise_neg_and(sr2, sr1, tr);
			return  bitwise_invert_set_sign(tr);
		} else {
			bitwise_invert_clear_sign(sr1);
			bitwise_invert_clear_sign(sr2);
			bitwise_and(sr1, sr2, tr);
			return  bitwise_invert_set_sign(tr);
		}
	}
}

int
RkBitwiseXor(unsigned sr1, unsigned sr2, unsigned tr)
{
	if (!bignum_register_sign[sr1]) {
		if (!bignum_register_sign[sr2]) {
			bitwise_xor(sr1, sr2, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		} else {
			bitwise_invert_clear_sign(sr2);
			bitwise_xor(sr1, sr2, tr);
			return  bitwise_invert_set_sign(tr);
		}
	} else {
		if (!bignum_register_sign[sr2]) {
			bitwise_invert_clear_sign(sr1);
			bitwise_xor(sr1, sr2, tr);
			return  bitwise_invert_set_sign(tr);
		} else {
			bitwise_invert_clear_sign(sr1);
			bitwise_invert_clear_sign(sr2);
			bitwise_xor(sr1, sr2, tr);
			bignum_register_sign[tr] = 0;
			return	1;
		}
	}
}
