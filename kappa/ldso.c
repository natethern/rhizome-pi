/*
 * Copyright (c) 1996-99,2002,04 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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
static char rcsid[] = "@(#)$Id: ldso.c,v 1.10 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: ldso.c,v $
 * Revision 1.10  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.9  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.8  2004/07/23 06:49:20  qfwfq
 * Use pthreads in cygwin.
 * Fix signal handling when pthreads is in use.
 *
 * Revision 1.7  2004/04/09 05:26:37  qfwfq
 * Make compliant with W^X.
 *
 * Revision 1.6  2002/09/27 11:06:43  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.5  2002/07/12 06:39:20  qfwfq
 * Linux port with pthread code.
 *
 * Revision 1.4  1999/06/15 07:43:17  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.3  1999/02/15 08:04:06  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.2  1998/07/31 10:38:21  qfwfq
 * In Win32, make method of calling API somewhat more 'legal'
 * Hoping that it makes program more robust.
 *
 * Revision 1.1  1997/10/16 06:24:37  qfwfq
 * Release version 0.40
 *
 */

/*
 * Support for loading shared objects.
 */
#include "rhizome.h"

#include <string.h>

#define ALT_STACK_SIZE		1024	/* this should be sufficient for any architecture */
#define CALLBACK_OBJ_CHUNK	1024	/* somewhat arbitrary parameter */
#define STUB_CODE_SIZE		32	/* max size of stub code of callback function */

#if defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__)
#	undef RK_LDSO_DLFCN
#	undef RK_JB_I386BSD
#	undef RK_JB_PTHREAD
#	undef RK_ARCH_I386
#else	/* neither windows nor beos */
# if !defined(RK_LDSO_DLFCN)
#	define NO_LDSO_METHOD
# endif
# if !defined(RK_ARCH_I386)
#	undef RK_JB_PTHREAD
# endif
# if !defined(RK_JB_I386BSD) && !defined(RK_JB_PTHREAD)
#	define JB_UNKNOWN
# endif
#endif

rk_object rk_call_external_proc;

static void
prep_errobj(char const *msg)
{
	int len;
	char *s;

	if (!msg || (len = strlen(msg)) == 0 || !(s = malloc(len))) {
		rk_error_obj = RK_SOBJ_UNSPEC;
		return;
	}
	memcpy(s, msg, len);
	if (!(rk_error_obj = RkMakeMallocObject(RK_MALLOC_TAG(len, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		rk_error_obj = RK_SOBJ_UNSPEC;
	}
}

#if defined(NO_LDSO_METHOD) || defined(JB_UNKNOWN)
static char *notimpl_msg = "Feature is not implemented";
#endif

#ifdef NO_LDSO_METHOD
void *
RkLoadSharedObj(char *mname)
{
	prep_errobj(notimpl_msg);
	return	NULL;
}

void
RkUnloadSharedObj(void *handle)
{
	/* Nothing to do */
}

void *
(*RkGetObjEntry(void *handle, char *sym))(void)
{
	prep_errobj(NULL);
	return	NULL;
}
#endif

#ifdef RK_LDSO_DLFCN
#include <dlfcn.h>

void *
RkLoadSharedObj(char *mname)
{
	void *handle;

	if (handle = dlopen(mname, RTLD_LAZY))
		return	handle;
	prep_errobj(dlerror());
	return	NULL;
}

void
RkUnloadSharedObj(void *handle)
{
	dlclose(handle);
}

void *
(*RkGetObjEntry(void *handle, char *sym))(void)
{
	void *entry;

	if (entry = dlsym(handle, sym))
		return	(void *(*)(void))entry;
	prep_errobj(dlerror());
	return	NULL;
}
#endif

#if defined(WIN32) || defined(__CYGWIN32__)
#include <stdio.h>
#include <stdarg.h>
#include <windows.h>

static void
win_err(void)
{
	char buf[11];

	sprintf(buf, "#x%08lX", GetLastError());
	prep_errobj(buf);
}

void *
RkLoadSharedObj(char *mname)
{
	void *handle;

	if (handle = LoadLibraryA(mname))
		return	handle;
	win_err();
	return	NULL;
}

void
RkUnloadSharedObj(void *handle)
{
	FreeLibrary(handle);
}

void *
(*RkGetObjEntry(void *handle, char *sym))(void)
{
	FARPROC entry;

	if (entry = GetProcAddress(handle, sym))
		return	(void *(*)(void))entry;
	win_err();
	return	NULL;
}
#endif

#ifdef __BEOS__
#include <be/kernel/image.h>

static void
be_err(status_t err)
{
	char buf[11];

	sprintf(buf, "#x%08lX", (unsigned)err);
	prep_errobj(buf);
}

void *
RkLoadSharedObj(char *mname)
{
	image_id id;

	if ((id = load_add_on(mname)) >= B_OK)
		return	(void *)id;
	be_err((status_t)id);
	return	NULL;
}

void
RkUnloadSharedObj(void *handle)
{
	unload_add_on((image_id)handle);
}

void *
(*RkGetObjEntry(void *handle, char *sym))(void)
{
	void *entry;
	status_t err;

	if ((err = get_image_symbol((image_id)handle, sym, B_SYMBOL_TYPE_ANY, &entry)) == B_OK)
		return	(void *(*)(void))entry;
	be_err(err);
	return	NULL;
}
#endif

static int (*cfa_func)();
static int cfa_argc;
static long cfa_argv[RK_BULK_ALLOC_THRESHOLD/2];
static int cfa_val;

static rk_object *callback_objects = NULL;
static int callback_obj_alloc = 0, callback_obj_max = 0;
static int callback_obj_free = -1;

static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	int i;

	if (persistent_too)
		for (i = 0; i < callback_obj_max; ++i)
			(*scan_fun)(&callback_objects[i], cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

static int
register_callback_object(void)
{
	rk_object *tmp_array;
	int i;

	if (callback_obj_free == -1) {
		if (!callback_obj_alloc) {
			tmp_array = malloc(CALLBACK_OBJ_CHUNK * sizeof(rk_object));
			p_traverse = rk_traverse_root;
			rk_traverse_root = traverse;
		} else
			tmp_array = realloc(callback_objects
						, (callback_obj_alloc + CALLBACK_OBJ_CHUNK) * sizeof(rk_object));
		if (!tmp_array)
			return	-1;
		tmp_array = &(callback_objects = tmp_array)[callback_obj_alloc];
		for (i = CALLBACK_OBJ_CHUNK; i--; ) {
			tmp_array[i] = RK_MAKEINUM(callback_obj_free);
			callback_obj_free = callback_obj_alloc + i;
		}
		callback_obj_alloc += CALLBACK_OBJ_CHUNK;
	}
	i = callback_obj_free;
	callback_obj_free = RK_GETINUM(callback_objects[i]);
	if (i >= callback_obj_max)
		callback_obj_max = i+1;
	RkWriteCell(&callback_objects[i], rk_eval_register[0]);
	return	i;
}

#ifndef RK_W_XOR_X
# define alloc_stub()	malloc(STUB_CODE_SIZE)
# define free_stub(p)	free(p)
# define enable_stub(p)	((void)0)
#else
# include <sys/param.h>
# include <sys/types.h>
# include <sys/mman.h>

static void *stub_next = NULL, *stub_limit, *stub_freed = NULL;

static void *alloc_stub(void)
{
	void *p;

	if (stub_freed) {
		p = stub_freed;
		stub_freed = *(void **)stub_freed;
		mprotect(p, STUB_CODE_SIZE, PROT_READ|PROT_WRITE);
		return	p;
	}
	if (!stub_next) {
		if (!(stub_next = malloc(PAGE_SIZE)))
			return	NULL;
		stub_limit = (void *)((char *)stub_next + PAGE_SIZE);
	}
	p = stub_next;
	stub_next = (void *)((char *)stub_next + STUB_CODE_SIZE);
	if ((void *)((char *)stub_next + STUB_CODE_SIZE) > stub_limit)
		stub_next = NULL;
	mprotect(p, STUB_CODE_SIZE, PROT_READ|PROT_WRITE);
	return	p;
}

static void free_stub(void *p)
{
	mprotect(p, STUB_CODE_SIZE, PROT_READ|PROT_WRITE);
	*(void **)p = stub_freed;
	mprotect(p, STUB_CODE_SIZE, PROT_READ|PROT_EXEC);
	stub_freed = p;
}

static void enable_stub(void *p)
{
	mprotect(p, STUB_CODE_SIZE, PROT_READ|PROT_EXEC);
}
#endif

void
RkDestroyCallbackEntry(void *(*p)(void), int index)
{
	free_stub((void *)p);
	callback_objects[index] = RK_MAKEINUM(callback_obj_free);
	callback_obj_free = index;
}

#ifdef JB_UNKNOWN
void RK_VOLATILE
RkStartExecution(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
{
	execute(proc);
	abort();
}

static rk_object
call_ext(void)
{
	prep_errobj(notimpl_msg);
	RK_SIGNAL_ERROR(RK_ERROR_DYNLOAD, rk_error_obj);
}

#define INIT_ENTRY_TEMPLATE	((void)0)

void *(*RkMakeCallbackEntry(rk_object proc, int narg, int *index))(void)
{
	return	NULL;
}
#endif

#if defined(RK_JB_I386BSD) || defined(RK_JB_PTHREAD) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__)
# if !(defined(WIN32) && !defined(__CYGWIN32__))
#  include <setjmp.h>
typedef jmp_buf cr_buffer;
#  if defined(RK_BSD_SETJMP) || defined(__BEOS__)
#   define CR_CHKPT(jb)		_setjmp(jb)
#   define CR_SWTCH(jb, s)	_longjmp(jb, s)
#  else
#   define CR_CHKPT(jb)		setjmp(jb)
#   define CR_SWTCH(jb, s)	longjmp(jb, s)
#  endif
# else
typedef long cr_buffer[6];
extern int rk_cr_chkpt(cr_buffer);
extern void rk_cr_switch(cr_buffer, int);
#  define CR_CHKPT(jb)		rk_cr_chkpt(jb)
#  define CR_SWTCH(jb, s)	rk_cr_switch(jb, s)
# endif

# if defined(WIN32) && !defined(__CYGWIN32__)
static LPVOID (__stdcall *pConvertThreadToFiber)(LPVOID);
static LPVOID (__stdcall *pCreateFiber)(DWORD, VOID (CALLBACK *)(PVOID), LPVOID);
static VOID (__stdcall *pSwitchToFiber)(LPVOID);
static int fiber_ok = 0;
static int main_fiber_status;
static LPVOID main_fiber, api_fiber;
# endif

# ifdef __BEOS__
#  include <be/kernel/OS.h>

static sem_id run_main, run_api;
static int main_thread_status;

static void
switch_to_main(void)
{
	release_sem(run_main);
	while (acquire_sem(run_api) != B_NO_ERROR) ;
}

static void
switch_to_api(void)
{
	release_sem(run_api);
	while (acquire_sem(run_main) != B_NO_ERROR) ;
}

static void RK_VOLATILE
thread_initialize_error(char const *apiname, status_t code)
{
	char msg_buf[256];

	sprintf(msg_buf, "Thread initialization error - %s: return code %d\n", apiname, code);
	RkFatalAbort(msg_buf);
}
# endif

# if defined(RK_JB_PTHREAD) || defined(__CYGWIN32__)
#  include <pthread.h>
#  include <semaphore.h>
#  include <signal.h>

static sem_t run_main, run_api;
static int main_thread_status;

static void
switch_to_main(void)
{
	sem_post(&run_main);
	while (sem_wait(&run_api)) ;
}

static void
switch_to_api(void)
{
	sem_post(&run_api);
	while (sem_wait(&run_main)) ;
}

static void RK_VOLATILE
thread_initialize_error(int err)
{
	char msg_buf[256];

	sprintf(msg_buf, "Could not start thread - %s\n", strerror(err));
	RkFatalAbort(msg_buf);
}
# endif

#ifdef _MSC_VER
int __declspec(naked)
rk_cr_chkpt(cr_buffer cb)
{
	__asm {
		mov eax,4[esp]
		mov edx,0[esp]
		mov 0[eax],edx
		mov 4[eax],ebx
		mov 8[eax],esp
		mov 12[eax],ebp
		mov 16[eax],esi
		mov 20[eax],edi
		xor eax,eax
		ret
	}
}

void __declspec(naked)
rk_cr_switch(cr_buffer cb, int v)
{
	__asm {
		mov edx,4[esp]
		mov eax,8[esp]
		mov ecx,0[edx]
		mov ebx,4[edx]
		mov esp,8[edx]
		mov ebp,12[edx]
		mov esi,16[edx]
		mov edi,20[edx]
		mov [esp],ecx
		ret
	}
}
#endif

#ifdef __LCC__
_asm("	.text");

_asm("	.type	_rk_cr_chkpt,function");
_asm("_rk_cr_chkpt:");
_asm("	movl	4(%esp),%eax");
_asm("	movl	0(%esp),%edx");
_asm("	movl	%edx,0(%eax)");
_asm("	movl	%ebx,4(%eax)");
_asm("	movl	%esp,8(%eax)");
_asm("	movl	%ebp,12(%eax)");
_asm("	movl	%esi,16(%eax)");
_asm("	movl	%edi,20(%eax)");
_asm("	xorl	%eax,%eax");
_asm("	ret");
_asm("e_chkpt:");
_asm("	.size	_rk_cr_chkpt,e_chkpt-_rk_cr_chkpt");
_asm("	.globl	_rk_cr_chkpt");

_asm("	.type	_rk_cr_switch,function");
_asm("_rk_cr_switch:");
_asm("	movl	4(%esp),%edx");
_asm("	movl	8(%esp),%eax");
_asm("	movl	0(%edx),%ecx");
_asm("	movl	4(%edx),%ebx");
_asm("	movl	8(%edx),%esp");
_asm("	movl	12(%edx),%ebp");
_asm("	movl	16(%edx),%esi");
_asm("	movl	20(%edx),%edi");
_asm("	movl	%ecx,(%esp)");
_asm("	ret");
_asm("e_switch:");
_asm("	.size	_rk_cr_switch,e_switch-_rk_cr_switch");
_asm("	.globl	_rk_cr_switch");
#endif

static cr_buffer runmain, *callout;

#ifdef __BORLANDC__
# pragma option -k
#endif

static void RK_VOLATILE
do_call(void)
{
# if defined(RK_JB_I386BSD) || defined (RK_ARCH_I386) || defined(__CYGWIN32__) || defined(__BEOS__)
	int i;

	for (i = cfa_argc; i-- > 0; )
		asm("push %0": : "r"(cfa_argv[i]): );
	asm("call *%1; movl %%eax, %0": "=r"(cfa_val): "r"(cfa_func): "%eax", "%ecx", "%edx", "cc", "memory");
# endif
# if defined(WIN32) && !defined(__CYGWIN32__) && !defined(__LCC__)
	int i;
	register long tmp;

	for (i = cfa_argc; i-- > 0; ) {
		tmp = cfa_argv[i];
		__asm { push tmp }
	}
	tmp = (long)cfa_func;
	__asm {
		call tmp
		mov tmp,eax
	}
	cfa_val = tmp;
# endif
# ifdef __LCC__
	_asm("	movl	_cfa_argc,%eax");
	_asm("	jmp	l1");
	_asm("l0:");
	_asm("	pushl	_cfa_argv(,%eax,4)");
	_asm("l1:");
	_asm("	decl	%eax");
	_asm("	jge	l0");
	_asm("	call	*_cfa_func");
	_asm("	movl	%eax,_cfa_val");
# endif
# if defined(WIN32) && !defined(__CYGWIN32__)
	if (fiber_ok) {
		main_fiber_status = 1;
		pSwitchToFiber(main_fiber);
		CR_SWTCH(*callout, 1);
	} else
		CR_SWTCH(runmain, 1);
# elif defined(RK_JB_PTHREAD) || defined(__CYGWIN32__) || defined(__BEOS__)
	main_thread_status = 1;
	switch_to_main();
	CR_SWTCH(*callout, 1);
# else
	CR_SWTCH(runmain, 1);
# endif
}

#ifdef __BORLANDC__
# pragma option -k.
#endif

# if defined(RK_JB_I386BSD) || defined(WIN32) && !defined(__CYGWIN32__)
#  define JBELT_IP(jb)	((jb)[0])
#  define JBELT_SP(jb)	((jb)[2])
#  define JB_SP_OFFSET	(-3)
# endif

# ifdef JB_SP_OFFSET
#  if defined(WIN32)
static void RK_VOLATILE
start_exec_no_fiber(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
#  else
void RK_VOLATILE
RkStartExecution(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
#  endif
{
	long alt_stack[ALT_STACK_SIZE];
	cr_buffer inicall;

	CR_CHKPT(runmain);
	alt_stack[ALT_STACK_SIZE-1] = (long)proc;
	alt_stack[ALT_STACK_SIZE-2] = (long)abort;
	JBELT_IP(runmain) = (long)execute;
	JBELT_SP(runmain) = (long)&alt_stack[ALT_STACK_SIZE+JB_SP_OFFSET];
	callout = &inicall;
	if (!CR_CHKPT(inicall))
		CR_SWTCH(runmain, 1);
	do_call();
#ifdef _MSC_VER
/* optimizor bug workaround */
	CR_SWTCH(inicall, 1);
#endif
}
# endif

# if defined (RK_JB_PTHREAD) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__)
struct EXEC_DATA { void RK_VOLATILE (*execute)(rk_object); rk_object proc; };

#  if defined(RK_JB_PTHREAD) || defined(__CYGWIN32__)
static void *
main_thread_func(void *param)
#  elif defined(__BEOS__)
static int32
main_thread_func(void *param)
#  else
static VOID CALLBACK
main_fiber_proc(PVOID param)
#  endif
{
	(((struct EXEC_DATA *)param)->execute)(((struct EXEC_DATA *)param)->proc);
	abort();
}

#  ifdef __BEOS__
static int32
api_thread_func(void *param)
{
	cr_buffer inicall;

	while (acquire_sem(run_api) != B_NO_ERROR) ;
	callout = &inicall;
	CR_CHKPT(inicall);
	do_call();
}

void RK_VOLATILE
RkStartExecution(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
{
	struct EXEC_DATA exdata;
	thread_id tid_main, tid_api;
	status_t ev, dmy;

	if ((run_main = create_sem(0, "main thread")) < B_NO_ERROR)
		thread_initialize_error("create_sem(main)", run_main);
	if ((run_api = create_sem(0, "api caller")) < B_NO_ERROR)
		thread_initialize_error("create_sem(api)", run_api);
	exdata.execute = execute, exdata.proc = proc;
	if ((tid_main = spawn_thread(main_thread_func, "main thread", B_NORMAL_PRIORITY, (void *)&exdata)) < B_NO_ERROR)
		thread_initialize_error("spawn_thread(main)", tid_main);
	if ((tid_api = spawn_thread(api_thread_func, "api caller", B_NORMAL_PRIORITY, NULL)) < B_NO_ERROR)
		thread_initialize_error("spawn_thread(api)", tid_api);
	resume_thread(tid_api);
	wait_for_thread(tid_main, &ev);
	kill_thread(tid_api);
	wait_for_thread(tid_api, &dmy);
	exit(ev);
}
#  endif

#  if defined(RK_JB_PTHREAD) || defined(__CYGWIN32__)
void RK_VOLATILE
RkStartExecution(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
{
	struct EXEC_DATA exdata;
	cr_buffer inicall;
	pthread_t dmy;
	pthread_attr_t attr;
	int err;
#ifndef __CYGWIN32__
	sigset_t set;
#endif

	sem_init(&run_main, 0, 0);
	sem_init(&run_api, 0, 0);
	exdata.execute = execute, exdata.proc = proc;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	err = pthread_create(&dmy, &attr, main_thread_func, (void *)&exdata);
	pthread_attr_destroy(&attr);
	if (err)
		thread_initialize_error(err);
#ifndef __CYGWIN32__
	sigfillset(&set);
	pthread_sigmask(SIG_BLOCK, &set, NULL);
#endif
	while (sem_wait(&run_api)) ;
	callout = &inicall;
	CR_CHKPT(inicall);
	do_call();
}
#  endif

#  if defined(WIN32) && !defined(__CYGWIN32__)
static void RK_VOLATILE
start_exec_fiber(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
{
	struct EXEC_DATA exdata;
	cr_buffer inicall;

	exdata.execute = execute, exdata.proc = proc;
	api_fiber = pConvertThreadToFiber(NULL);
	main_fiber = pCreateFiber(ALT_STACK_SIZE*4, main_fiber_proc, (LPVOID)&exdata);
	pSwitchToFiber(main_fiber);
	callout = &inicall;
	CR_CHKPT(inicall);
	do_call();
#ifdef _MSC_VER
/* optimizor bug workaround */
	CR_SWTCH(inicall, 1);
#endif
}

void RK_VOLATILE
RkStartExecution(void RK_VOLATILE (*execute)(rk_object), rk_object proc)
{
	if (fiber_ok)
		start_exec_fiber(execute, proc);
	else
		start_exec_no_fiber(execute, proc);
}
#  endif
# endif

static rk_object cb_ret_proc;

static rk_object
run_external(void)
{
	rk_object *cp, proc;
# if defined(WIN32) && !defined(__CYGWIN32__)
	register int status;
# endif

# if defined(WIN32) && !defined(__CYGWIN32__)
	if (fiber_ok) {
		pSwitchToFiber(api_fiber);
		status = main_fiber_status;
	} else {
		if (!(status = CR_CHKPT(runmain)))
			CR_SWTCH(*callout, 1);
	}
	switch (status) {
# elif defined(RK_JB_PTHREAD) || defined(__CYGWIN32__) || defined(__BEOS__)
	switch_to_api();
	switch (main_thread_status) {
# else
	switch (CR_CHKPT(runmain)) {
	case 0:
		CR_SWTCH(*callout, 1);
# endif
	case 1:
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(3, RK_TCODE_EXTARGV);
		cp[1] = (cfa_val&0xffff0000) | RK_MAKEINUM(0);
		cp[2] = RK_MAKEINUM(cfa_val&0xffff);
		cp[3] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		rk_valid_register = 1;
		RK_PROCEED();
	case 2:
		proc = rk_eval_register[0];
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(3, RK_TCODE_EXTARGV);
		cp[1] = rk_eval_register[2];
		cp[2] = rk_eval_register[3];
		cp[3] = RK_DUMMY_OBJ;
		rk_eval_register[0] = (rk_object)cp;
		rk_valid_register = 2;
		cp = RkAllocCells(6);
		cp[0] = RK_VECTOR_TAG(6, 0);
		cp[1] = cb_ret_proc;
		cp[2] = ((unsigned long)callout&0xffff0000) | RK_MAKEINUM(0);
		cp[3] = RK_MAKEINUM((unsigned long)callout&0xffff);
		cp[4] = (rk_object)rk_continuation;
		cp[5] = RK_DUMMY_OBJ;
		rk_continuation = cp;
		return	proc;
	}
}

static rk_object
call_ext(void)
{
	rk_object *cp;
	int i;

	cp = (rk_object *)rk_eval_register[1];
	cfa_func = (int (*)())((cp[1]&0xffff0000) | (cp[2]>>2));
	cp = &((rk_object *)rk_eval_register[0])[1];
	cfa_argc = cp[-1] >> 13;
	for (i = 0; i < cfa_argc; ++i)
		cfa_argv[i] = (cp[i*2]&0xffff0000) | (cp[i*2+1]>>2);
	return	run_external();
}

static rk_object
cb_ret(void)
{
	if ((rk_eval_register[0] & 7) || ((rk_object *)rk_eval_register[0])[0] != RK_VECTOR_TAG(3, RK_TCODE_EXTARGV))
		RK_SIGNAL_ERROR1(RK_ERROR_DYNLOAD);
	*(unsigned long *)&callout = (rk_continuation[2]&0xffff0000) | (rk_continuation[3]>>2);
	rk_continuation = (rk_object *)rk_continuation[4];
	cfa_func = NULL;
	return	run_external();
}

# if defined(RK_JB_I386BSD) || defined(RK_ARCH_I386) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__)

static unsigned char entry_template[] = {
/* 0000 */	0x89, 0xe0,		/* movl %esp,%eax    */
/* 0002 */	0x83, 0xc0, 0x04,	/* addl $4,%eax      */
/* 0005 */	0x50,			/* pushl %eax        */
/* 0006 */	0x68, 0, 0, 0, 0,	/* pushl $<proc>     */	/* proc    = 0x07 */
/* 000b */	0x68, 0, 0, 0, 0,	/* pushl $<index>    */	/* index   = 0x0c */
/* 0010 */	0xb8, 0, 0, 0, 0,	/* movl _run_cb,%eax */	/* _run_cb = 0x11 */
/* 0015 */	0xff, 0xd0,		/* call %eax         */
/* 0017 */	0x83, 0xc4, 0x0c,	/* addl $12,%esp     */
/* 001a */	0xc3,			/* ret               */
/* 001a */   /* 0xc2, */ 0, 0,		/* ret $<n>	     */ /* n       = 0x1b */
/* 001d */	};

#define INIT_ENTRY_TEMPLATE	(*(long *)&entry_template[0x11] = (long)run_cb)

# endif

static void *
run_cb(int index, rk_object proc, void *argp)
{
	cr_buffer *parent, callrec;
	rk_object *cp;

	parent = callout;
	callout = &callrec;
	rk_eval_register[0] = proc;
	rk_eval_register[1] = callback_objects[index];
	rk_eval_register[2] = ((unsigned long)argp&0xffff0000) | RK_MAKEINUM(0);
	rk_eval_register[3] = RK_MAKEINUM((unsigned long)argp&0xffff);
	rk_valid_register = 4;
# if defined(WIN32) && !defined(__CYGWIN32__)
	if (fiber_ok) {
		main_fiber_status = 2;
		pSwitchToFiber(main_fiber);
		CR_CHKPT(callrec);
	} else {
		if (!CR_CHKPT(callrec))
			CR_SWTCH(runmain, 2);
	}
# elif defined(RK_JB_PTHREAD) || defined(__CYGWIN32__) || defined(__BEOS__)
	main_thread_status = 2;
	switch_to_main();
	CR_CHKPT(callrec);
# else
	if (!CR_CHKPT(callrec))
		CR_SWTCH(runmain, 2);
# endif
	if (cfa_func)
		do_call();
	callout = parent;
	cp = (rk_object *)rk_eval_register[0];
	return	(void *)((cp[1]&0xffff0000) | (cp[2]>>2));
}

# if defined(RK_JB_I386BSD) || defined(RK_ARCH_I386) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__)

void *(*RkMakeCallbackEntry(rk_object proc, int narg, int *index))(void)
{
	char *entry;

	if (!(entry = alloc_stub()))
		return	NULL;
	if ((*index = register_callback_object()) == -1) {
		free_stub(entry);
		return	NULL;
	}
	memcpy(entry, entry_template, sizeof entry_template);
	*(long *)&entry[0x07] = (long)proc;
	*(long *)&entry[0x0c] = (long)*index;
# if defined(WIN32) || defined(__CYGWIN32__)
	if (narg >= 0) {
		*(unsigned char *)&entry[0x1a] = 0xc2;
		*(short *)&entry[0x1b] = narg*4;
	}
# endif
	enable_stub(entry);
	return	(void *(*)(void))entry;
}

# endif
#endif

int
RkInitializeLdSo(int index)
{
#if defined(WIN32) && !defined(__CYGWIN32__)
	HMODULE hlib_kernel;
#endif

	if (index != -1) {
		INIT_ENTRY_TEMPLATE;
#if defined(WIN32) && !defined(__CYGWIN32__)
		if (hlib_kernel = LoadLibrary("kernel32.dll")) {
			*(FARPROC *)&pConvertThreadToFiber = GetProcAddress(hlib_kernel, "ConvertThreadToFiber");
			*(FARPROC *)&pCreateFiber = GetProcAddress(hlib_kernel, "CreateFiber");
			*(FARPROC *)&pSwitchToFiber = GetProcAddress(hlib_kernel, "SwitchToFiber");
			if (pConvertThreadToFiber && pCreateFiber && pSwitchToFiber)
				fiber_ok = 1;
		}
#endif
		rk_call_external_proc = RkRegisterProcedure(index + 0, call_ext);
#ifndef JB_UNKNOWN
		cb_ret_proc = RkRegisterProcedure(index + 1, cb_ret);
#endif
	}
#ifdef JB_UNKNOWN
	return	1;
#else
	return	2;
#endif
}
