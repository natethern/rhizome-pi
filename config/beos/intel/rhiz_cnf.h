/* @(#)$Header: /u/master/rhizome/config/beos/intel/rhiz_cnf.h,v 1.4 2005/11/10 08:47:34 qfwfq Exp $ */
/*
 * $Log: rhiz_cnf.h,v $
 * Revision 1.4  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.3  2002/09/27 10:05:22  qfwfq
 * Add support of linux and lcc-win32.
 *
 * Revision 1.2  1999/07/08 02:23:20  qfwfq
 * increase RK_HEAP_CHUNK_SIZE value
 *
 * Revision 1.1  1999/06/15 07:14:06  qfwfq
 * Rearrange configuration files.
 *
 */

/* Constants used by heap management routines. */
#define RK_HEAP_CHUNK_SIZE	0x10000
#define RK_BULK_ALLOC_THRESHOLD	0x400

/* Size of array rk_eval_register */
#define RK_EVAL_REGISTER_SIZE	16

/* Uncomment following if your system's signal handling is broken. */
/* #define RK_OLD_SYSV_SIGNAL */

/* Uncomment following if your libm.a does not provide IEEE 754 support functions */
/* #define RK_NO_IEEE754_SUPPORT */
/* And uncomment following if finite() is available */
/* #define RK_HAS_FINITE */

/* [BASE,BASE+SIZE*2) must fit in process' address space */
#if defined(GO32) || defined(WIN32) || defined(__CYGWIN32__)
#	define RK_PERSISTENT_ALLOC_SIZE	0x10000000
#	define RK_PERSISTENT_ALLOC_BASE 0x20000000
#	define RK_MALLOC_NO_PAGE_ALIGN
#endif

/* Uncomment following to use mblen() to test double byte character */
/* #define RK_USE_LOCALE */

/* Use mmap to allocate cell space */
/* #define RK_USE_MMAP	*/

/* Use function pointer alignment in GC */
/* #define RK_FUNCTIONS_ALIGNED */

/* For support of shared object (called DLL in Win32) */
#define RK_LDSO_DLFCN		/* use dlopen() */
# define RK_NO_LEADING_UNDERSCORE	/* underscore added to C symbols? */

#define RK_JB_I386BSD		/* xBSD i386 _setjmp */
/* #define RK_JB_PTHREAD	/* use pthread */
/* # define RK_ARCH_I386	/* select one */

/* And somewhat trivial issue :-) */
#if 0
#	define index strchr
#endif
