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

/* @(#)$Id: rhizome.h,v 1.13 2005/11/10 08:47:34 qfwfq Exp $ */
/*
 * $Log: rhizome.h,v $
 * Revision 1.13  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.12  2004/08/06 05:48:05  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.11  2002/09/27 10:55:35  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.10  1999/06/15 07:43:05  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.9  1999/03/15 12:57:05  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.8  1999/02/15 07:59:24  qfwfq
 * altarnatives of finite and isnan for some environments
 *
 * Revision 1.7  1998/07/31 10:24:14  qfwfq
 * Add bitwise operation
 *
 * Revision 1.6  1997/10/16 06:24:27  qfwfq
 * Release version 0.40
 *
 * Revision 1.5  1997/05/12 07:21:12  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.4  1996/10/10 08:26:38  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:08:13  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed config.h to rhiz_cnf.h for compiler support.
 *
 * Revision 1.2  1996/06/05 05:20:12  qfwfq
 * Add switches concerning IEEE floating point functions.
 *
 * Revision 1.1  93/11/08  13:55:42  qfwfq
 * Initial revision
 * 
 */

#if defined(__CYGWIN__) && !defined(__CYGWIN32__)
#	define __CYGWIN32__	__CYGWIN__
#endif

#include "rhiz_cnf.h"

#include <stddef.h>
#include <stdlib.h>
#include <setjmp.h>

#ifdef GO32
# ifndef MSDOS
#	define MSDOS
# endif
#	define RK_NO_IEEE754_SUPPORT
#endif

#ifdef __GNUC__
#	define RK_INLINE	inline
#	define RK_VOLATILE	__attribute__((noreturn))
#else
#	define RK_INLINE
#	define RK_VOLATILE
#endif

#ifdef __BEOS__
#	define RK_LOAD_NEGATIVE
#endif

#ifdef WIN32
#	define RK_USEDECLSPEC
#endif
#ifdef __CYGWIN32__
#	define RK_USEDECLSPEC
#endif
#ifdef __BEOS__
#	define RK_USEDECLSPEC
#endif

#ifndef RK_USEDECLSPEC
#	define RK_DLLENTRY
#else
# ifdef RK_BIND_EXPORT
#	define RK_DLLENTRY __declspec(dllexport)
# else
#  ifdef RK_BIND_STATIC
#	define RK_DLLENTRY
#  else
#	define RK_DLLENTRY __declspec(dllimport)
#  endif
# endif
#endif

typedef unsigned long rk_object;

#define RK_ISCELL(x)	(!((x)&6))

#define RK_MAKEINUM(n)	(((unsigned long)(n)<<2)|2)
#define RK_GETINUM(x)	((long)(x)>>2)
#define RK_ISINUM(x)	((x) & 2)

#define RK_SOBJ(x)	(0x0c+((x)<<8))
#define RK_DUMMY_OBJ	RK_SOBJ(0)
#define RK_SOBJ_UNBOUND	RK_SOBJ(1)
#define RK_SOBJ_ERROR	RK_SOBJ(2)
#define RK_SOBJ_EOF	RK_SOBJ(3)
#define RK_SOBJ_RPAR	RK_SOBJ(4)
#define RK_SOBJ_DOT	RK_SOBJ(5)
#define RK_SOBJ_NIL	RK_SOBJ(6)
#define RK_SOBJ_FALSE	RK_SOBJ(7)
#define RK_SOBJ_TRUE	RK_SOBJ(8)
#define RK_SOBJ_UNSPEC	RK_SOBJ(9)
#define RK_SOBJ_MAX	10

#define RK_MAKEICHAR(c)	(((unsigned long)(c)<<8)|0x1c)
#define RK_GETICHAR(x)	(((unsigned long)(x)>>8)&0xff)
#define RK_ISICHAR(x)	(((x) & 0xff) == 0x1c)

#define RK_ISSYMBOL(x)		(!((x) & 7) && (((rk_object *)(x))[0] & 7) == 5)

#define RK_VECTOR_TAG(len, tcode)	(((len)<<12)|((tcode)<<4)|7)
#define RK_TCODE_VECTOR		1
#define RK_ISVECTOR(x)		(!((x) & 7) && (((rk_object *)(x))[0] & 0xfff) == RK_VECTOR_TAG(0, RK_TCODE_VECTOR))
#define RK_TCODE_BIGINT_POS	2
#define RK_TCODE_BIGINT_NEG	3
#define RK_TCODE_FRACTION	4
#define RK_TCODE_FLONUM		5
#define RK_TCODE_COMPLEX	6
#define RK_TCODE_SHAREDOBJ	7
#define RK_TCODE_EXTPROC	8
#define RK_TCODE_EXTARGV	9
#define RK_TCODE_VECTMAX	10

#define RK_MALLOC_TAG(aux, tcode)	(((aux)<<12)|((tcode)<<4)|0xf)
#define RK_TCODE_STRING		1
#define RK_ISSTRING(x)		(!((x) & 7) && (((rk_object *)(x))[0] & 0xfff) == RK_MALLOC_TAG(0, RK_TCODE_STRING))
#define RK_TCODE_MALLOCMAX	2

#define RK_PROCEED()		do { return rk_continuation[1]; } while (0)
#define RK_SIGNAL_ERROR(en, ed)	do { \
					rk_eval_register[0] = (ed); \
					rk_eval_register[1] = RK_MAKEINUM(en); \
					rk_valid_register = 2; \
					rk_continuation = rk_error_catcher; \
					return	rk_continuation[1]; \
				} while (0)
#define RK_SIGNAL_ERROR1(en)	RK_SIGNAL_ERROR(en, RK_SOBJ_UNSPEC)

#define RK_ERROR_OSERROR	1
#define RK_ERROR_READ_SYNTAX	2
#define RK_ERROR_PRMEOF		3
#define RK_ERROR_OUTOFSTORAGE	4
#define RK_ERROR_OVERFLOW	5
#define RK_ERROR_DIVIDEBYZERO	6
#define RK_ERROR_DYNLOAD	7

#define RK_CFLAG_WHITESPACE	0x01
#define RK_CFLAG_PUNCTUATION	0x02

/* Handling fatal situation */
void RK_VOLATILE RkFatalAbort(char const *);

/* Signal handling */
extern int volatile rk_got_signal;
rk_object RkHandleSignal(rk_object);

/* Heap management routines */
extern void (*rk_traverse_root)(int, void (*)(rk_object *, void *), void *);
void RkInitializeHeap(void);
void RkScavenge(int);
RK_DLLENTRY rk_object *RkAllocCells(unsigned);
rk_object *RkAllocVector(unsigned);
RK_DLLENTRY void RkWriteCell(rk_object *, rk_object);
#ifdef RK_FUNCTIONS_ALIGNED
void rk_plain_destructor(void *);
#else
# define rk_plain_destructor free
#endif
rk_object RkMakeMallocObject(unsigned long, void (*)(void *), void *);
void *RkGetMallocObject(rk_object);
void RkSetMallocObject(rk_object, void *);
void RkUnsetDestructor(rk_object);

/* Symbol management routines */
void RkInitializeSymbol(void);
rk_object RkInternSymbol(char const *, unsigned);

/* Run Engine */
extern RK_DLLENTRY rk_object *rk_continuation;
extern RK_DLLENTRY rk_object *rk_error_catcher;
extern RK_DLLENTRY rk_object rk_eval_register[];
extern RK_DLLENTRY int rk_valid_register;
void RkInitializeRunEngine(int, int (* const *)(int));
int RkExtendProcsArray(int);
rk_object RkRegisterProcedure(int, rk_object (*)(void));
void RK_VOLATILE RkExecute(rk_object);

/* Read */
extern int rk_error_code;
extern rk_object rk_error_obj;
extern rk_object rk_read_proc;
int RkInitializeRead(int);
void RkMakeCharTableEntry(int, rk_object *, int);
void RkCopyCharTableEntry(int, int);
void RkMakeMeshTableEntry(int, rk_object *);
void RkCopyMeshTableEntry(int, int);
int RkIsDBCSLeadByte(int);

/* Write */
extern rk_object rk_write_proc;
extern rk_object rk_display_proc;
int RkInitializeWrite(int);

/* Number */
extern jmp_buf *rk_sigfpe_catcher;
void RkLoadBigInt(rk_object, unsigned);
void RkLoadShortInt(long, unsigned);
void RkLoadInteger(rk_object, unsigned);
rk_object RkStoreInt(unsigned);
double RkLoadFloat(rk_object);
rk_object RkStoreFloat(double);
rk_object RkReadNumber(char *, unsigned, int);
char *RkPrintNumber(rk_object, int);
rk_object RkAbsoluteValue(rk_object);
int RkSignature(rk_object);
int RkZeroP(rk_object);
int RkExactP(rk_object);
void RkCopyInt(unsigned, unsigned);
int RkAddIntByUshort(unsigned, unsigned long);
int RkMultiplyIntByUshort(unsigned, unsigned long);
unsigned RkDivideIntByUshort(unsigned, unsigned long);
void RkNegateInt(unsigned);
int RkIntegerIsZero(unsigned);
int RkIntegerIsNegative(unsigned);
int RkCompareIntByInt(unsigned, unsigned);
int RkAddIntByInt(unsigned, unsigned, unsigned);
int RkMultiplyIntByInt(unsigned, unsigned, unsigned);
void RkDivideIntByInt(unsigned, unsigned, unsigned);
unsigned RkComputeGCD(unsigned, unsigned, unsigned, unsigned);
rk_object RkMakeExactFraction(unsigned, unsigned);
int RkCompareFraction(unsigned, unsigned, unsigned, unsigned);
int RkAddFraction(unsigned, unsigned, unsigned, unsigned);
int RkMultiplyFraction(unsigned, unsigned, unsigned, unsigned);
double RkIntToFloat(unsigned);
#if !defined(RK_NO_IEEE754_SUPPORT) || defined(RK_HAS_FINITE)
# define RK_FINITE(x)	finite(x)
#else
# ifdef _MSC_VER
#  define RK_FINITE(x)	_finite(x)
# else
#  ifdef __BORLANDC__
int RkFinite(double);
#   define RK_FINITE(x)	RkFinite(x)
#  else
#   define RK_FINITE(x)	((x) == (x))
#  endif
# endif
#endif
#if !defined(RK_NO_IEEE754_SUPPORT)
# define RK_ISNAN(x)	isnan(x)
#else
# ifdef _MSC_VER
#  define RK_ISNAN(x)	_isnan(x)
# else
#  ifdef __BORLANDC__
int RkIsNaN(double);
#   define RK_ISNAN(x)	RkIsNaN(x)
#  else
#   define RK_ISNAN(x)	(!((x) > 0 || (x) <= 0))
#  endif
# endif
#endif
double RkConvertToFloat(rk_object);
#if defined(GO32) || defined(WIN32) && !defined(__CYGWIN32__) && !defined(__LCC__)
# define RK_MY_RINT
#endif
#ifdef RK_MY_RINT
double RkRint(double);
#endif
void RkConvertToFraction(double, unsigned, unsigned);
int RkBitwiseAnd(unsigned, unsigned, unsigned);
int RkBitwiseOr(unsigned, unsigned, unsigned);
int RkBitwiseXor(unsigned, unsigned, unsigned);

/* LdSo */
extern rk_object rk_call_external_proc;
int RkInitializeLdSo(int);
void *RkLoadSharedObj(char *);
void RkUnloadSharedObj(void *);
void *(*RkGetObjEntry(void *, char *))(void);
void RK_VOLATILE RkStartExecution(void RK_VOLATILE (*)(rk_object), rk_object);
void *(*RkMakeCallbackEntry(rk_object, int, int *))(void);
void RkDestroyCallbackEntry(void *(*)(void), int);
