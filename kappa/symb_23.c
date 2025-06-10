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
static char rcsid[] = "@(#)$Id: symb_23.c,v 1.4 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: symb_23.c,v $
 * Revision 1.4  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.3  1998/07/31 10:34:08  qfwfq
 * Add symbol-aux-datum
 *
 * Revision 1.2  1996/09/06 06:08:35  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.1  1993/11/08 14:02:21  qfwfq
 * Initial revision
 *
 */

/*
 * Symbol identity system. Use 2-3 tree.
 */
#include "rhizome.h"

#include <string.h>

#define SYMBOL_NAME(sym)	(((rk_object *)(((rk_object *)(sym))[0] & ~7))[0])

static int rank, locate;
static rk_object *root, *current, *work;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	(*scan_fun)((rk_object *)&root, cookie);
	(*scan_fun)((rk_object *)&current, cookie);
	(*scan_fun)((rk_object *)&work, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

static int
key_compare(char const *name, unsigned namelen, rk_object string)
{
	char *key;
	unsigned keylen;
	int cmp;

	key = RkGetMallocObject(string);
	keylen = ((unsigned long *)string)[0] >> 12;
	if (keylen == 0) {
		keylen = ((unsigned long *)key)[0];
		key += 4;
	}
	while (namelen--) {
		if (keylen-- == 0)
			return	1;
		if (cmp = (*name++) - (*key++))
			return	cmp;
	}
	return	keylen ? -1 : 0;
}

static rk_object
search_symbol(char const *name, unsigned namelen)
{
	int depth;
	rk_object symbol;

	current = root;
	for (depth = 0; depth < rank; ++depth)
		if (key_compare(name, namelen, current[2]) < 0)
			current = (rk_object *)current[4];
		else if (!(current[1] & 4) || key_compare(name, namelen, current[3]) < 0)
			current = (rk_object *)current[5];
		else
			current = (rk_object *)current[6];
	if (key_compare(name, namelen, SYMBOL_NAME(current[4])) == 0)
		symbol = current[4];
	else if ((depth = key_compare(name, namelen, current[2])) < 0) {
		locate = 0;
		return	0;
	} else if (depth == 0)
		symbol = current[5];
	else if (!(current[1] & 4) || (depth = key_compare(name, namelen, current[3])) < 0) {
		locate = 1;
		return	0;
	} else if (depth == 0)
		symbol = current[6];
	else {
		locate = 2;
		return	0;
	}
	current = (rk_object *)RK_DUMMY_OBJ;
	return	symbol;
}

static void
register_symbol(rk_object symbol)
{
	int depth;
	unsigned int flags;
	rk_object *node;

	flags = current[1];
	if (!(flags & 4)) {
		if (locate == 0) {
			RkWriteCell(&current[3], symbol);
			RkWriteCell(&current[6], current[5]);
			RkWriteCell(&current[5], current[3]);
			RkWriteCell(&current[3], current[2]);
			RkWriteCell(&current[2], SYMBOL_NAME(current[5]));
		} else {
			RkWriteCell(&current[6], symbol);
			RkWriteCell(&current[3], SYMBOL_NAME(current[6]));
		}
		current[1] |= 4;
		current = (rk_object *)RK_DUMMY_OBJ;
		return;
	}
	RkWriteCell(&current[1], symbol);
	node = RkAllocCells(8);
	node[0] = RK_VECTOR_TAG(8, 0);
	node[6] = node[3] = RK_DUMMY_OBJ;
	node[1] = (rk_object)work;
	work = node;
	switch (locate) {
	case 0:
		node[5] = current[6];
		node[4] = current[5];
		node[2] = current[3];
		node[7] = current[2];
		RkWriteCell(&current[5], current[1]);
		RkWriteCell(&current[2], SYMBOL_NAME(current[1]));
		break;
	case 1:
		node[5] = current[6];
		node[4] = current[1];
		node[2] = current[3];
		node[7] = SYMBOL_NAME(current[1]);
		break;
	case 2:
		node[5] = current[1];
		node[4] = current[6];
		node[2] = SYMBOL_NAME(current[1]);
		node[7] = current[3];
		break;
	}
	current[6] = current[3] = RK_DUMMY_OBJ;
	for (depth = rank-1; depth >= 0; --depth) {
		locate = flags >> 3;
		flags = ((rk_object *)current[7])[1];
		if (!(flags & 4)) {
			if (locate == 0) {
				RkWriteCell(&((rk_object *)current[7])[6], ((rk_object *)current[7])[5]);
				RkWriteCell(&((rk_object *)current[7])[5], (rk_object)work);
				RkWriteCell(&((rk_object *)current[7])[3], ((rk_object *)current[7])[2]);
				RkWriteCell(&((rk_object *)current[7])[2], work[7]);
				node = work;
				work = (rk_object *)node[1];
				node[1] = (1 << 3) | 2;
				((rk_object *)((rk_object *)current[7])[6])[1] += (1 << 3);
			} else {
				RkWriteCell(&((rk_object *)current[7])[6], (rk_object)work);
				RkWriteCell(&((rk_object *)current[7])[3], work[7]);
				node = work;
				work = (rk_object *)node[1];
				node[1] = (2 << 3) | 2;
			}
			current[1] = (locate << 3) | 2;
			RkWriteCell(&node[7], current[7]);
			((rk_object *)current[7])[1] |= 4;
			current = (rk_object *)RK_DUMMY_OBJ;
			return;
		}
		RkWriteCell(&((rk_object *)current[7])[1], (rk_object)work);
		work = (rk_object *)work[1];
		current = (rk_object *)current[7];
		node = RkAllocCells(8);
		node[0] = RK_VECTOR_TAG(8, 0);
		node[6] = node[3] = RK_DUMMY_OBJ;
		node[1] = (rk_object)work;
		work = node;
		switch (locate) {
		case 0:
			node[5] = current[6];
			((rk_object *)node[5])[1] = (1 << 3) | (((rk_object *)node[5])[1] & 7);
			node[4] = current[5];
			((rk_object *)node[4])[1] = (0 << 3) | (((rk_object *)node[4])[1] & 7);
			node[2] = current[3];
			node[7] = current[2];
			RkWriteCell(&current[5], current[1]);
			RkWriteCell(&current[2], ((rk_object *)current[1])[7]);
			((rk_object *)current[4])[1] = (0 << 3) | 2;
			RkWriteCell(&((rk_object *)current[5])[7], (rk_object)current);
			((rk_object *)current[5])[1] = (1 << 3) | 2;
			break;
		case 1:
			node[5] = current[6];
			node[4] = current[1];
			node[2] = current[3];
			node[7] = ((rk_object *)current[1])[7];
			((rk_object *)current[5])[1] = (1 << 3) | 2;
			((rk_object *)node[4])[1] = (0 << 3) | 2;
			((rk_object *)node[5])[1] = (1 << 3) | (((rk_object *)node[5])[1] & 7);
			break;
		case 2:
			node[5] = current[1];
			node[4] = current[6];
			node[2] = ((rk_object *)current[1])[7];
			node[7] = current[3];
			((rk_object *)node[4])[1] = (0 << 3) | 2;
			((rk_object *)node[5])[1] = (1 << 3) | 2;
			break;
		}
		RkWriteCell(&((rk_object *)work[4])[7], (rk_object)work);
		RkWriteCell(&((rk_object *)work[5])[7], (rk_object)work);
		current[6] = current[3] = RK_DUMMY_OBJ;
	}
	RkWriteCell(&current[7], (rk_object)work);
	work = (rk_object *)work[1];
	root = RkAllocCells(8);
	root[0] = RK_VECTOR_TAG(8, 0);
	root[1] = 2;
	root[2] = ((rk_object *)current[7])[7];
	root[4] = (rk_object)current;
	root[5] = current[7];
	root[7] = root[6] = root[3] = RK_DUMMY_OBJ;
	RkWriteCell(&((rk_object *)root[5])[7], (rk_object)root);
	RkWriteCell(&current[7], (rk_object)root);
	current[1] = (0 << 3) | 2;
	((rk_object *)root[5])[1] = (1 << 3) | 2;
	++rank;
	current = (rk_object *)RK_DUMMY_OBJ;
}

void
RkInitializeSymbol(void)
{
	static unsigned long name0[1] = {0};
	static char name1[1] = ".";

	root = current = work = (rk_object *)RK_DUMMY_OBJ;
	p_traverse = rk_traverse_root;
	rk_traverse_root = traverse;
	if (!(*(rk_object *)&current = RkMakeMallocObject(RK_MALLOC_TAG(0, RK_TCODE_STRING), NULL, name0))
	 || !(*(rk_object *)&work = RkMakeMallocObject(RK_MALLOC_TAG(1, RK_TCODE_STRING), NULL, name1)))
		RkFatalAbort("Failed to initialize symbol system.\n");
	rank = 0;
	root = RkAllocCells(16);
	root[0] = RK_VECTOR_TAG(8, 0);
	root[1] = 2;
	root[2] = (rk_object)work;
	root[3] = root[6] = root[7] = RK_DUMMY_OBJ;
	root[4] = (rk_object)&root[8];
	root[5] = (rk_object)&root[12];
	root[8] = (rk_object)&root[10] | 5;
	root[9] = RK_SOBJ_UNBOUND;
	root[10] = (rk_object)current;
	root[11] = RK_SOBJ_UNBOUND;
	root[12] = (rk_object)&root[14] | 5;
	root[13] = RK_SOBJ_UNBOUND;
	root[14] = (rk_object)work;
	root[15] = RK_SOBJ_UNBOUND;
	current = work = (rk_object *)RK_DUMMY_OBJ;
}

rk_object
RkInternSymbol(char const *name, unsigned namelen)
{
	rk_object obj, *objp;
	char *name_copy;
	unsigned len;

	if (obj = search_symbol(name, namelen))
		return	obj;
	len = namelen;
	if (namelen == 0 || namelen >= (1<<20)) {
		namelen = 0;
		len += 4;
	}
	if (!(name_copy = malloc(len))) {
		RkScavenge(1);
		if (!(name_copy = malloc(len))) {
			current = (rk_object *)RK_DUMMY_OBJ;
			return	0;
		}
	}
	if (!namelen) {
		((unsigned long *)name_copy)[0] = len-4;
		memcpy(name_copy+4, name, len-4);
	} else
		memcpy(name_copy, name, len);
	if (!(work = (rk_object *)RkMakeMallocObject(RK_MALLOC_TAG(namelen, RK_TCODE_STRING), NULL, name_copy))) {
		free(name_copy);
		current = work = (rk_object *)RK_DUMMY_OBJ;
		return	0;
	}
	objp = RkAllocCells(4);
	objp[0] = (rk_object)&objp[2] | 5;
	objp[1] = RK_SOBJ_UNBOUND;
	objp[2] = (rk_object)work;
	objp[3] = RK_SOBJ_UNBOUND;
	work = objp;
	register_symbol((rk_object)work);
	objp = work;
	work = (rk_object *)RK_DUMMY_OBJ;
	return	(rk_object)objp;
}
