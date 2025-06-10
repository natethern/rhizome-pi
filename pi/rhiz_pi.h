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

/* @(#)$Id: rhiz_pi.h,v 1.12 2004/08/06 05:48:06 qfwfq Exp $ */
/*
 * $Log: rhiz_pi.h,v $
 * Revision 1.12  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.11  2002/09/27 12:07:57  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.10  1999/06/29 07:47:51  qfwfq
 * Check duplicate module loading.
 *
 * Revision 1.9  1999/06/15 07:43:43  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.8  1999/03/15 12:57:27  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.7  1999/02/15 08:47:40  qfwfq
 * r5rs -- multiple values, dynamic-wind and eval
 *
 * Revision 1.6  1998/07/31 11:06:39  qfwfq
 * Fasloader support
 *
 * Revision 1.5  1997/10/16 06:24:55  qfwfq
 * Release version 0.40
 *
 * Revision 1.4  1997/05/12 07:21:21  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1997/04/26 13:29:07  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.2  1996/10/10 08:27:01  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.1  1996/09/06 06:11:31  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.2  1993/11/13 16:14:53  qfwfq
 * Add procedure-port feature.
 *
 * Revision 1.1  93/11/08  14:09:49  qfwfq
 * Initial revision
 * 
 */

#include "rhizome.h"

#define RP_CAR(x)	(((rk_object *)(x))[0])
#define RP_CDR(x)	(((rk_object *)(x))[1])
#define RP_ISCONS(x)	(!(x & 7) && !(RP_CAR(x) & 1))

#define RP_SOBJ_SYNMARK	RK_SOBJ(RK_SOBJ_MAX+0)
#define RP_SOBJ_MAX	(RK_SOBJ_MAX+1)

#define RP_MAKEVLOC0(f, v)	(((unsigned long)(f)<<24)|((unsigned long)(v)<<8)|0x2c)
#define RP_GETFRAME0(x)		(((unsigned long)(x)>>24)&0xff)
#define RP_GETVNUM0(x)		(((unsigned long)(x)>>8)&0xffff)

#define RP_TCODE_VLOC1		(RK_TCODE_VECTMAX+0)
#define RP_GETFRAME1(x)		RK_GETINUM(((rk_object *)(x))[1])
#define RP_GETVNUM1(x)		RK_GETINUM(((rk_object *)(x))[2])
#define RP_TCODE_PORT		(RK_TCODE_VECTMAX+1)
#define RP_TCODE_VECTMAX	(RK_TCODE_VECTMAX+2)

#define RP_TCODE_FILE		(RK_TCODE_MALLOCMAX+0)
#define RP_TCODE_STRPORT	(RK_TCODE_MALLOCMAX+1)
#define RP_TCODE_MALLOCMAX	(RK_TCODE_MALLOCMAX+2)

#define RP_ASSERTARG(n)	do { \
				if (rk_eval_register[2] != RK_MAKEINUM(n)) \
					RK_SIGNAL_ERROR(RP_ERROR_ARGNO, RK_SOBJ_UNSPEC); \
			} while (0)
#define RP_RETURN()	do { rk_valid_register = 1; RK_PROCEED(); } while (0)

#define RP_ERROR_ILLEGALEVAL	101
#define RP_ERROR_VARUNBOUND	102
#define RP_ERROR_ILLEGALAPPLY	103
#define RP_ERROR_ILLEGALARG	104
#define RP_ERROR_PRIMSYNTAX	105
#define RP_ERROR_TOOMANYFORMALS	106
#define RP_ERROR_ARGNO		107
#define RP_ERROR_ILLEGALDEF	108
#define RP_ERROR_PROGRAM	109
#define RP_ERROR_MAPLENGTH	110
#define RP_ERROR_EVALRET	111
#define RP_ERROR_PORTBUSY	112
#define RP_ERROR_PORTPROC	113
#define RP_ERROR_ROENV		114

#define RP_DEFINESUBR(name, proc, index, func)	if (!RpDefineSubr(name, sizeof(name)-1, &proc, (index), (func))) \
							return -1

#if !defined(_MSC_VER) && !defined(__LCC__)
#	define RP_ARRAY_FORWARD_DECL	static
#else
#	define RP_ARRAY_FORWARD_DECL	extern
#endif

#ifndef RK_USEDECLSPEC
#	define RP_DLLEXPORT
#else
#	define RP_DLLEXPORT	__declspec(dllexport)
#endif

#ifndef RP_MODLIB
#	define RP_MODEXPORT
#else
#	define RP_MODEXPORT	RP_DLLEXPORT
#endif

#if !defined(WIN32) && !defined(__CYGWIN32__)
#	define RP_STATICONLY
#endif

struct RP_PROCEDURE_REC { rk_object (*rp_faddr)(void); rk_object *rp_paddr; };
struct RP_OBJECT_DESC { int rp_ocode; void const *rp_odata; };
struct RP_STRING_DESC { int rp_length; char const *rp_chars; };

struct RP_MODULE_INIT {
	int rp_nprocs;
	struct RP_PROCEDURE_REC const *rp_procs;
	struct RP_OBJECT_DESC const *rp_odesc;
	rk_object *rp_oarray;
	int rp_version;
	int *rp_loaded;
	char *rp_mname;
};

struct RP_PROGRAM_DESC {
	int rp_nmodules;
	rk_object (* const *rp_runprocs)(void);
	void (* const *rp_initprocs)(struct RP_MODULE_INIT *);
};

#define RP_OTAG_SYMBOL		0
#define RP_OTAG_GENSYM		1
#define RP_OTAG_PAIR		2
#define RP_OTAG_NULL		3
#define RP_OTAG_STRING		4
#define RP_OTAG_BOOLEAN		5
#define RP_OTAG_CHARACTER	6
#define RP_OTAG_VECTOR		7
#define RP_OTAG_SHORTINT	8
#define RP_OTAG_BIGINT		9
#define RP_OTAG_FRACTION	10
#define RP_OTAG_DBLFLOAT	11
#define RP_OTAG_COMPLEX		12
#define RP_OTAG_SYNMARK		13

/* Main */
extern int rp_argc;
extern char **rp_argv;
int RpInWindowsSubsystem(void);

/* Eval */
extern rk_object rp_default_eval_car_proc;
extern rk_object rp_eval_car_proc;
extern rk_object rp_evlis_proc;
extern rk_object rp_noapply_proc;
int RpInitializeEval(int);

/* Primitives */
int RpInitializePrimitives(int);

/* Abbreviation */
int RpInitializeAbbreviation(int);

/* Subr */
extern rk_object *rp_dynamic_extent;
rk_object RpWindTo(rk_object, rk_object, rk_object, rk_object);
int RpDefineSubr(char const *, unsigned, rk_object *, int, rk_object (*)(void));
int RpInitializeSubr(int);

/* Port */
extern jmp_buf *rp_io_signal_catcher;
void RpResetStdioPorts(void);
int RpInitializePort(int);

/* Numeric */
int RpEqvP(rk_object, rk_object);
int RpInitializeNumeric(int);

/* Characters */
int RpInitializeCharacters(int);

/* Reflection */
extern rk_object rp_reflect_eval_proc;
extern rk_object rp_eval_fun;
int RpInitializeReflection(int);

/* Helper */
extern RK_DLLENTRY rk_object rp_apply_object_proc;
extern RK_DLLENTRY rk_object rp_run_faslcode_proc;
RK_DLLENTRY rk_object RpMakeCompiledClosure(rk_object, int, int, rk_object);
int RpCallInitProcs(int, int, void (* const *)());
int RpInitializeHelper(int);

/* Dynamic loading */
int RpInitializeDynload(int);
