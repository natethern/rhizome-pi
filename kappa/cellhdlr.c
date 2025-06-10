/*
 * Copyright (c) 1993,96-99,2002,04 Inujima, Masaru <qfwfq@kt.rim.or.jp>
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
static char rcsid[] = "@(#)$Id: cellhdlr.c,v 1.11 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: cellhdlr.c,v $
 * Revision 1.11  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.10  2004/08/06 05:48:05  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.9  2004/07/23 05:22:34  qfwfq
 * obey GCC3 strict-aliasing rule
 *
 * Revision 1.8  2002/09/27 11:06:43  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.7  2002/05/20 06:40:08  qfwfq
 * fix mmap() code
 *
 * Revision 1.6  1999/06/15 07:43:17  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.5  1999/02/22 11:47:06  qfwfq
 * put address space for persistent cells under program's control (win32)
 *
 * Revision 1.4  1996/10/10 08:26:46  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:08:32  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.2  1996/06/05 05:29:16  qfwfq
 * Use malloc if MAP_ANON in mmap is unavailable.
 *
 * Revision 1.1  93/11/08  14:02:18  qfwfq
 * Initial revision
 * 
 */

/*
 * Cell allocator and scavanger.
 */
#include "rhizome.h"

#if !(defined(GO32) || defined(WIN32) || defined(__CYGWIN32__) || defined(__BEOS__))
# ifdef RK_USE_MMAP
#	include <sys/types.h>
#	include <sys/stat.h>
#	include <fcntl.h>
#	include <sys/mman.h>
#	include <errno.h>
#	include <string.h>
# endif
#elif defined(WIN32) || defined(__CYGWIN32__)
#	include <windows.h>
#	include <time.h>
#elif defined(__BEOS__)
#	include <be/kernel/OS.h>
#endif
#ifdef REPORT_STATS
#	include <stdio.h>
#endif

struct bulk_rec {
	struct bulk_rec *fore, *back;
	rk_object *base;
#ifdef RK_MALLOC_NO_PAGE_ALIGN
	void *tbase;
#endif
};

#ifndef RK_MALLOC_NO_PAGE_ALIGN
#	define TBASE	base
#	define XTRA	0
#else
#	define TBASE	tbase
#	define XTRA	8
#endif

struct malloc_rec {
	union {
		void (*fp)(void *);
		unsigned long ul;
	} destructor_u;
	void *data;
};

#define destructor_p	destructor_u.fp
#define destructor_m	destructor_u.ul

struct malloc_rec_hdr {
	struct malloc_rec_hdr *next;
	struct malloc_rec *base;
};

struct scanp {
	rk_object *ephemeral, *persistent;
};

#define CHUNK_BYTES	(RK_HEAP_CHUNK_SIZE*sizeof(rk_object))
#define MALLOC_CHUNK	(CHUNK_BYTES+sizeof(struct malloc_rec_hdr))
#define MALLOC_ELTS	(CHUNK_BYTES/sizeof(struct malloc_rec))

#define NCELL(x)	((*(rk_object *)(x)&0xf)==7 \
				? (*(rk_object *)(x)>>12 ? *(rk_object *)(x)>>12 : ((rk_object *)(x))[1]>>2) : 2)
#define EVEN(n)		(((n)+1)&~1)
#define MOVED(x)	(((unsigned long *)(x))[1]&1)
#define SUBSTANCE(x)	(((unsigned long *)(x))[1]&~1)
#define IS_EPHEMERAL(x)	(ephemeral_from <= (rk_object *)(x) && (rk_object *)(x) < free_cell)
#define IS_NEW(x)	(generation_mark <= (rk_object *)(x) && (rk_object *)(x) < free_cell)

void (*rk_traverse_root)(int, void (*)(rk_object *, void *), void *);

static rk_object *ephemeral_from, *ephemeral_to, *free_cell, *generation_mark, *ephemeral_from_end;
static rk_object *persistent_from, *persistent_free;
static unsigned persistent_limit;
#ifndef GO32
static unsigned persistent_from_size;
#endif
#if defined(GO32) || defined(WIN32) || defined(__CYGWIN32__)
static rk_object *persistent_to;
#endif
static rk_object **cross_rec_base, **cross_rec_free;
static struct bulk_rec bulk_rec_head;
static struct malloc_rec_hdr *malloc_recs;
static struct malloc_rec *malloc_rec_free;

	/*ARGSUSED*/
static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	/* Intentionally empty */
}

#ifdef RK_MALLOC_NO_PAGE_ALIGN

static void *
a_alloc(int n)
{
	void *allocp;

	if (!(allocp = malloc(n + 8)))
		return	NULL;
	return	(void *)(((unsigned long)allocp + 7) & ~7);
}

#else
#	define a_alloc	malloc
#endif

static void *
zzalloc(int n)
{
	void *allocp;

	if (!(allocp = a_alloc(n)))
		RkFatalAbort("Alas! no memory is available for me ;_;\n");
	return	allocp;
}

#if !(defined(GO32) || defined(WIN32) || defined(__CYGWIN32__))
# ifndef __BEOS__
#  ifdef RK_USE_MMAP

#	ifndef MAP_ANON
#	 ifdef MAP_ANONYMOUS
#		define MAP_ANON	MAP_ANONYMOUS
#	 endif
#	endif

static void *
zzmmap(int n)
{
#   ifndef MAP_ANON
	static int zerofd = -1;
	char msg_buf[256];
#   endif
	void *allocp;

#   ifndef MAP_ANON
	if (zerofd == -1 && (zerofd = open("/dev/zero", O_RDWR)) == -1) {
		sprintf(msg_buf, "open: /dev/zero: %s\n", strerror(errno));
		RkFatalAbort(msg_buf);
	}
	allocp = mmap(NULL, n, PROT_READ|PROT_WRITE, MAP_PRIVATE, zerofd, 0);
#   else
	allocp = mmap(NULL, n, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
#   endif
	if (allocp == (void *)-1)
		RkFatalAbort("Virtual memory exhausted.\n");
	return	allocp;
}

#  else
#	define zzmmap			zzalloc
#	define munmap(addr, size)	free(addr)
#  endif
# else

static void *
zzmmap(int n)
{
	void *allocp = NULL;
	area_id id;

	id = create_area("persistent cells", &allocp, B_ANY_ADDRESS, n, B_NO_LOCK, B_READ_AREA|B_WRITE_AREA);
	if (id < B_OK)
		RkFatalAbort("Virtual memory exhausted.\n");
	return	allocp;
}

static void
zzmfree(void *addr)
{
	delete_area(area_for(addr));
}

#	define munmap(addr, size)	zzmfree(addr)
# endif
#elif defined(WIN32) || defined(__CYGWIN32__)

static void *
zzvalloc(int b, int n)
{
	void *allocp;

	if (!(allocp = VirtualAlloc((LPVOID)b, n, MEM_RESERVE, PAGE_READWRITE))
	 && !(allocp = VirtualAlloc((LPVOID)0, n, MEM_RESERVE, PAGE_READWRITE)))
		RkFatalAbort("Failed to reserve address space (?o?)\n");
	return	allocp;
}

static void
zzvcommit(void *addr, int n)
{
	if (!VirtualAlloc(addr, n, MEM_COMMIT, PAGE_READWRITE))
		RkFatalAbort("Virtual memory exhausted.\n");
}

#endif

void
RkInitializeHeap(void)
{
	char *allocp;
	int i;

	generation_mark = free_cell = ephemeral_from = zzalloc(CHUNK_BYTES);
	ephemeral_from_end = ephemeral_from + RK_HEAP_CHUNK_SIZE;
	ephemeral_to = zzalloc(CHUNK_BYTES);
	persistent_limit = RK_HEAP_CHUNK_SIZE*2;
#ifndef GO32
	persistent_from_size = persistent_limit*sizeof(rk_object) + CHUNK_BYTES;
# if !(defined(WIN32) || defined(__CYGWIN32__))
	persistent_free = persistent_from = zzmmap(persistent_from_size);
# else
	persistent_free = persistent_from = zzvalloc(RK_PERSISTENT_ALLOC_BASE, RK_PERSISTENT_ALLOC_SIZE);
	persistent_to = zzvalloc(RK_PERSISTENT_ALLOC_BASE+RK_PERSISTENT_ALLOC_SIZE, RK_PERSISTENT_ALLOC_SIZE);
	zzvcommit(persistent_from, persistent_from_size);
# endif
#else
	persistent_free = persistent_from = zzalloc(RK_PERSISTENT_ALLOC_SIZE);
	persistent_to = zzalloc(RK_PERSISTENT_ALLOC_SIZE);
#endif
	cross_rec_free = cross_rec_base = zzalloc(CHUNK_BYTES);
	bulk_rec_head.back = bulk_rec_head.fore = &bulk_rec_head;
	malloc_recs = (void *)((allocp = zzalloc(MALLOC_CHUNK)) + CHUNK_BYTES);
	malloc_recs->base = (void *)allocp;
	malloc_recs->next = NULL;
	malloc_rec_free = NULL;
	for (i = 0; i < MALLOC_ELTS; ++i) {
		malloc_recs->base[i].destructor_p = NULL;
		malloc_recs->base[i].data = malloc_rec_free;
		malloc_rec_free = &malloc_recs->base[i];
	}
	rk_traverse_root = traverse;
}

static RK_INLINE void
mvobj(rk_object *obj, rk_object **dstv, unsigned ncell)
{
	rk_object *elts = (rk_object *)(*obj & ~7);
	unsigned i;

	for (i = 0; i < ncell; ++i)
		(*dstv)[i] = elts[i];
	elts[1] = (unsigned long)*dstv | 1;
	*obj = (unsigned long)*dstv | (*obj & 7);
	*dstv += ncell;
}

static void
connect_bulk(struct bulk_rec *rec)
{
	if (!((unsigned long)rec->TBASE & 1))
		return;
	rec->fore->back = rec->back;
	rec->back->fore = rec->fore;
	bulk_rec_head.back->fore = rec;
	rec->back = bulk_rec_head.back;
	bulk_rec_head.back = rec;
	rec->fore = &bulk_rec_head;
	*(unsigned long *)&rec->TBASE &= ~1;
}

static void
traverse_cb_full(rk_object *root, void *cbdata)
{
	rk_object obj = *root;
	unsigned size;

	if (!RK_ISCELL(obj))
		return;
	if (MOVED(obj)) {
		*root = SUBSTANCE(obj);
		return;
	}
	if ((size = NCELL(obj)) >= RK_BULK_ALLOC_THRESHOLD) {
		connect_bulk((struct bulk_rec *)((rk_object *)obj + size));
		return;
	}
	size = EVEN(size);
	if (IS_EPHEMERAL(obj))
		mvobj(root, &((struct scanp *)cbdata)->ephemeral, size);
	else
		mvobj(root, &((struct scanp *)cbdata)->persistent, size);
}

static void
scavenge_full(void)
{
	struct scanp freep;
	unsigned bsize, size, i;
	rk_object *scan_ephemeral, *scan_persistent, *objp, obj, *topcell_persistent, *topcell_ephemeral;
#ifndef GO32
	unsigned persistent_to_size;
#endif
#if !(defined(GO32) || defined(WIN32) || defined(__CYGWIN32__))
	rk_object *persistent_to;
#endif
	struct bulk_rec bulk_free, *scan_bulk, *next_bulk;
	struct malloc_rec_hdr *travh;
	struct malloc_rec *travr;

	topcell_ephemeral = scan_ephemeral = freep.ephemeral = ephemeral_to;
#ifndef GO32
	persistent_to_size = (persistent_limit+RK_HEAP_CHUNK_SIZE)*sizeof(rk_object) + CHUNK_BYTES;
# if !(defined(WIN32) || defined(__CYGWIN32__))
	persistent_to = zzmmap(persistent_to_size);
# else
	zzvcommit(persistent_to, persistent_to_size);
# endif
#endif
	topcell_persistent = scan_persistent = freep.persistent = persistent_to;
	cross_rec_free = cross_rec_base;
	for (scan_bulk = bulk_rec_head.fore; scan_bulk != &bulk_rec_head; scan_bulk = scan_bulk->fore)
		*(unsigned long *)&scan_bulk->TBASE |= 1;
	if (bulk_rec_head.fore != &bulk_rec_head) {
		bulk_free = bulk_rec_head;
		bulk_free.fore->back = bulk_free.back->fore = &bulk_free;
		bulk_rec_head.fore = bulk_rec_head.back = &bulk_rec_head;
	} else
		bulk_free.fore = bulk_free.back = &bulk_free;
	scan_bulk = &bulk_rec_head;
	for (travh = malloc_recs; travh; travh = travh->next)
		for (travr = travh->base; travr < travh->base + MALLOC_ELTS; ++travr)
#ifndef RK_FUNCTIONS_ALIGNED
			travr->destructor_m |= 0x80000000;
#else
			travr->destructor_m |= 1;
#endif
	(*rk_traverse_root)(1, traverse_cb_full, &freep);

scavenge_bulks:
	while (scan_bulk->fore != &bulk_rec_head) {
		scan_bulk = scan_bulk->fore;
		objp = scan_bulk->base;
#ifdef __GNUC__
		bsize = (*objp >> 12) ?: (objp[1] >> 2);
#else
		if (!(bsize = (*objp >> 12)))
			bsize = (objp[1] >> 2);
#endif
		for (i = 1; i < bsize; ++i) {
			if (!RK_ISCELL(objp[i]))
				continue;
			if (MOVED(objp[i])) {
				objp[i] = SUBSTANCE(objp[i]);
				continue;
			}
			if ((size = NCELL(objp[i])) >= RK_BULK_ALLOC_THRESHOLD) {
				connect_bulk((struct bulk_rec *)((rk_object *)objp[i] + size));
				continue;
			}
			mvobj(&objp[i], &freep.persistent, EVEN(size));
		}
	}

scavenge_persistents:
	while (scan_persistent < freep.persistent) {
		obj = *scan_persistent;
		if (scan_persistent == topcell_persistent)
			switch (obj & 0xf) {
			default:
				topcell_persistent += 2;
				if (!RK_ISCELL(obj)) {
					++scan_persistent;
					continue;
				}
				break;
			case 3: case 11: case 5: case 13:
				topcell_persistent += 2;
				break;
			case 7:
				topcell_persistent += EVEN(obj>>12);
				++scan_persistent;
				continue;
			case 15:
#ifndef RK_FUNCTIONS_ALIGNED
				((struct malloc_rec *)scan_persistent[1])->destructor_m &= ~0x80000000;
#else
				((struct malloc_rec *)scan_persistent[1])->destructor_m &= ~1;
#endif
				topcell_persistent = (scan_persistent += 2);
				continue;
			}
		else
			if (!RK_ISCELL(obj)) {
				++scan_persistent;
				continue;
			}
		obj &= ~7;
		if (MOVED(obj)) {
			obj = SUBSTANCE(obj);
			if (ephemeral_to <= (rk_object *)obj && (rk_object *)obj < freep.ephemeral)
				*cross_rec_free++ = scan_persistent;
			*scan_persistent = obj | (*scan_persistent&7);
			++scan_persistent;
			continue;
		}
		if ((size = NCELL(obj)) >= RK_BULK_ALLOC_THRESHOLD) {
			connect_bulk((struct bulk_rec *)((rk_object *)obj + size));
			++scan_persistent;
			goto scavenge_bulks;
		}
		mvobj(scan_persistent++, &freep.persistent, EVEN(size));
	}

	while (scan_ephemeral < freep.ephemeral) {
		obj = *scan_ephemeral;
		if (scan_ephemeral == topcell_ephemeral)
			switch (obj & 0xf) {
			default:
				topcell_ephemeral += 2;
				if (!RK_ISCELL(obj)) {
					++scan_ephemeral;
					continue;
				}
				break;
			case 3: case 11: case 5: case 13:
				topcell_ephemeral += 2;
				break;
			case 7:
				topcell_ephemeral += EVEN(obj>>12);
				++scan_ephemeral;
				continue;
			case 15:
#ifndef RK_FUNCTIONS_ALIGNED
				((struct malloc_rec *)scan_ephemeral[1])->destructor_m &= ~0x80000000;
#else
				((struct malloc_rec *)scan_ephemeral[1])->destructor_m &= ~1;
#endif
				topcell_ephemeral = (scan_ephemeral += 2);
				continue;
			}
		else
			if (!RK_ISCELL(obj)) {
				++scan_ephemeral;
				continue;
			}
		obj &= ~7;
		if (MOVED(obj)) {
			*scan_ephemeral = SUBSTANCE(obj) | (*scan_ephemeral&7);
			++scan_ephemeral;
			continue;
		}
		if ((size = NCELL(obj)) >= RK_BULK_ALLOC_THRESHOLD) {
			connect_bulk((struct bulk_rec *)((rk_object *)obj + size));
			++scan_ephemeral;
			goto scavenge_bulks;
		}
		size = EVEN(size);
		if (IS_EPHEMERAL(obj))
			mvobj(scan_ephemeral++, &freep.ephemeral, size);
		else {
			mvobj(scan_ephemeral++, &freep.persistent, size);
			goto scavenge_persistents;
		}
	}

	objp = ephemeral_to, ephemeral_to = ephemeral_from, ephemeral_from = objp;
	ephemeral_from_end = ephemeral_from + RK_HEAP_CHUNK_SIZE;
	free_cell = freep.ephemeral;
#ifndef GO32
# if !(defined(WIN32) || defined(__CYGWIN32__))
	munmap((caddr_t)persistent_from, persistent_from_size);
	persistent_from = persistent_to;
# endif
	persistent_from_size = persistent_to_size;
#endif
#if defined(GO32) || defined(WIN32) || defined(__CYGWIN32__)
	objp = persistent_to, persistent_to = persistent_from, persistent_from = objp;
#endif
	persistent_free = freep.persistent;
	if (persistent_free - persistent_from >= persistent_limit-RK_HEAP_CHUNK_SIZE)
		persistent_limit += RK_HEAP_CHUNK_SIZE;
	for (scan_bulk = bulk_free.fore; scan_bulk != &bulk_free; scan_bulk = next_bulk) {
		next_bulk = scan_bulk->fore;
		free((void *)((unsigned long)scan_bulk->TBASE & ~1));
	}
	malloc_rec_free = NULL;
	for (travh = malloc_recs; travh; travh = travh->next)
		for (travr = travh->base; travr < travh->base + MALLOC_ELTS; ++travr)
#ifndef RK_FUNCTIONS_ALIGNED
			if (travr->destructor_m & 0x80000000) {
# ifndef RK_LOAD_NEGATIVE
				if (travr->destructor_m &= ~0x80000000)
# else
				if (travr->destructor_m != 0x80000000)
# endif
#else
			if (travr->destructor_m & 1) {
				if (travr->destructor_m &= ~1)
#endif
					(*travr->destructor_p)(travr->data);
				travr->destructor_p = NULL;
				travr->data = malloc_rec_free;
				malloc_rec_free = travr;
			}
}

static void
traverse_cb(rk_object *root, void *cbdata)
{
	rk_object obj = *root;
	unsigned size;

	if (!RK_ISCELL(obj))
		return;
	if (MOVED(obj)) {
		*root = SUBSTANCE(obj);
		return;
	}
	if (IS_EPHEMERAL(obj)) {
		size = EVEN(NCELL(obj));
		if (IS_NEW(obj))
			mvobj(root, (rk_object **)cbdata, size);
		else
			mvobj(root, &persistent_free, size);
	}
}

void
RkScavenge(int forth_full_scavenge)
{
	unsigned size;
	rk_object obj, *freep_ephemeral;
	rk_object *scan_ephemeral, *scan_persistent, *topcell_ephemeral, *topcell_persistent, **scan_cross;

#ifdef REPORT_STATS
	time_t clock = time(NULL);
	fprintf(stderr, "Scavenge start:    %s", ctime(&clock));
#endif

	topcell_ephemeral = scan_ephemeral = freep_ephemeral = ephemeral_to;
	topcell_persistent = scan_persistent = persistent_free;
	for (scan_cross = cross_rec_base; scan_cross < cross_rec_free; ++scan_cross) {
		obj = **scan_cross;
		if (!RK_ISCELL(obj) && !(obj&1))
			continue;
		obj &= ~7;
		if (MOVED(obj)) {
			**scan_cross = SUBSTANCE(obj) | (**scan_cross & 7);
			continue;
		}
		if (IS_EPHEMERAL(obj))
			mvobj(*scan_cross, &persistent_free, EVEN(NCELL(obj)));
	}
	cross_rec_free = cross_rec_base;
	(*rk_traverse_root)(0, traverse_cb, &freep_ephemeral);

scavenge_persistents:
	while (scan_persistent < persistent_free) {
		obj = *scan_persistent;
		if (scan_persistent == topcell_persistent)
			switch (obj & 0xf) {
			default:
				topcell_persistent += 2;
				if (!RK_ISCELL(obj)) {
					++scan_persistent;
					continue;
				}
				break;
			case 3: case 11: case 5: case 13:
				topcell_persistent += 2;
				break;
			case 7:
				topcell_persistent += EVEN(obj>>12);
				++scan_persistent;
				continue;
			case 15:
				topcell_persistent = (scan_persistent += 2);
				continue;
			}
		else
			if (!RK_ISCELL(obj)) {
				++scan_persistent;
				continue;
			}
		obj &= ~7;
		if (MOVED(obj)) {
			obj = SUBSTANCE(obj);
			if (ephemeral_to <= (rk_object *)obj && (rk_object *)obj < freep_ephemeral)
				*cross_rec_free++ = scan_persistent;
			*scan_persistent = obj | (*scan_persistent&7);
			++scan_persistent;
			continue;
		}
		if (IS_EPHEMERAL(obj))
			mvobj(scan_persistent, &persistent_free, EVEN(NCELL(obj)));
		++scan_persistent;
	}

	while (scan_ephemeral < freep_ephemeral) {
		obj = *scan_ephemeral;
		if (scan_ephemeral == topcell_ephemeral)
			switch (obj & 0xf) {
			default:
				topcell_ephemeral += 2;
				if (!RK_ISCELL(obj)) {
					++scan_ephemeral;
					continue;
				}
				break;
			case 3: case 11: case 5: case 13:
				topcell_ephemeral += 2;
				break;
			case 7:
				topcell_ephemeral += EVEN(obj>>12);
				++scan_ephemeral;
				continue;
			case 15:
				topcell_ephemeral = (scan_ephemeral += 2);
				continue;
			}
		else
			if (!RK_ISCELL(obj)) {
				++scan_ephemeral;
				continue;
			}
		obj &= ~7;
		if (MOVED(obj)) {
			*scan_ephemeral = SUBSTANCE(obj) | (*scan_ephemeral&7);
			++scan_ephemeral;
			continue;
		}
		if (IS_EPHEMERAL(obj)) {
			size = EVEN(NCELL(obj));
			if (IS_NEW(obj))
				mvobj(scan_ephemeral, &freep_ephemeral, size);
			else {
				mvobj(scan_ephemeral++, &persistent_free, size);
				goto scavenge_persistents;
			}
		}
		++scan_ephemeral;
	}

	scan_ephemeral = ephemeral_to, ephemeral_to = ephemeral_from, ephemeral_from = scan_ephemeral;
	ephemeral_from_end = ephemeral_from + RK_HEAP_CHUNK_SIZE;
	free_cell = freep_ephemeral;
	if (forth_full_scavenge || persistent_free - persistent_from >= persistent_limit)
		scavenge_full();
	generation_mark = free_cell;

#ifdef REPORT_STATS
	clock = time(NULL);
	fprintf(stderr, "Scavenge complete: %s", ctime(&clock));
	fprintf(stderr, "\tephemetal in use:                      %d (cells)\n", free_cell - ephemeral_from);
# ifndef GO32
	fprintf(stderr, "\tpersistent area size:                  %d (cells)\n"
									, persistent_from_size/sizeof(rk_object));
# endif
	fprintf(stderr, "\tpersistent area limit:                 %d (cells)\n", persistent_limit);
	fprintf(stderr, "\tpersistent in use:                     %d (cells)\n", persistent_free - persistent_from);
	fprintf(stderr, "\tpointers from persistent to ephemeral: %d\n", cross_rec_free - cross_rec_base);
#endif
}

rk_object *
RkAllocCells(unsigned ncell)
{
	rk_object *cell;

	cell = free_cell;
	if ((free_cell += ncell) > ephemeral_from_end) {
		free_cell = cell;
		RkScavenge(0);
		if (free_cell + ncell > ephemeral_from_end)
			RkScavenge(1);	/* this leaves nothing in ephemeral region */
		cell = free_cell;
		free_cell += ncell;
	}
	return	cell;
}

rk_object *
RkAllocVector(unsigned ncell)
{
	void *p;
	rk_object *ap;
	struct bulk_rec *bp;

	if (!(p = malloc(ncell*sizeof(rk_object)+sizeof(struct bulk_rec)+XTRA))) {
		RkScavenge(1);
		if (!(p = malloc(ncell*sizeof(rk_object)+sizeof(struct bulk_rec)+XTRA)))
			return	NULL;
	}
#ifndef RK_MALLOC_NO_PAGE_ALIGN
	ap = p;
#else
	ap = (rk_object *)(((unsigned long)p + 7) & ~7);
#endif
	bp = (struct bulk_rec *)(ap + ncell);
	bp->back = bulk_rec_head.back;
	bulk_rec_head.back->fore = bp;
	bp->fore = &bulk_rec_head;
	bulk_rec_head.back = bp;
	bp->base = ap;
#ifdef RK_MALLOC_NO_PAGE_ALIGN
	bp->tbase = p;
#endif
	return	ap;
}

void
RkWriteCell(rk_object *cell, rk_object obj)
{
	*cell = obj;
	if (IS_EPHEMERAL(cell) || !IS_EPHEMERAL(obj))
		return;
	*cross_rec_free++ = cell;
	if (cross_rec_free == cross_rec_base + RK_HEAP_CHUNK_SIZE)
		RkScavenge(0);
}

#ifdef RK_FUNCTIONS_ALIGNED

/* alignment must be assured */
void
rk_plain_destructor(void *ptr)
{
	free(ptr);
}

#endif

rk_object
RkMakeMallocObject(unsigned long tag, void (*destructor)(void *), void *data)
{
	char *allocp;
	int i;
	struct malloc_rec_hdr *hdr;
	struct malloc_rec *rec;
	rk_object *cell;

	if (!malloc_rec_free) {
		RkScavenge(1);
		if (!malloc_rec_free) {
			if (!(allocp = a_alloc(MALLOC_CHUNK)))
				return	(rk_object)NULL;
			hdr = (void *)(allocp + CHUNK_BYTES);
			hdr->base = (void *)allocp;
			hdr->next = malloc_recs;
			malloc_recs = hdr;
			for (i = 0; i < MALLOC_ELTS; ++i) {
				hdr->base[i].destructor_p = NULL;
				hdr->base[i].data = malloc_rec_free;
				malloc_rec_free = &hdr->base[i];
			}
		}
	}
	cell = RkAllocCells(2);
	rec = malloc_rec_free;
	malloc_rec_free = malloc_rec_free->data;
	rec->destructor_p = destructor;
	rec->data = data;
	cell[0] = tag | 0xf;
	cell[1] = (rk_object)rec;
	return	(rk_object)cell;
}

void *
RkGetMallocObject(rk_object obj)
{
	return	((struct malloc_rec *)(((rk_object *)obj)[1]))->data;
}

void
RkSetMallocObject(rk_object obj, void *data)
{
	((struct malloc_rec *)(((rk_object *)obj)[1]))->data = data;
}

void
RkUnsetDestructor(rk_object obj)
{
	((struct malloc_rec *)(((rk_object *)obj)[1]))->destructor_p = NULL;
}
