/* Copyright (C) 2000, 2006, 2008 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/strings.h"

#include "libguile/debug-malloc.h"

/*
 * The following code is a hack which I wrote quickly in order to
 * solve a memory leak problem.  Since I wanted to have the
 * application running at close to normal speed, I prioritized speed
 * over maintainability.  /mdj
 */

typedef struct hash_entry {
  const void *key;
  const void *data;
} hash_entry_t;

#define N_SEEK 8

static int malloc_type_size = 31;
static hash_entry_t *malloc_type = 0;
static int malloc_object_size = 8191;
static hash_entry_t *malloc_object = 0;

#define TABLE(table) malloc_ ## table
#define SIZE(table) malloc_ ## table ## _size
#define HASH(table, key) \
  &TABLE (table)[((unsigned long) key >> 4UL) * 2654435761UL % SIZE (table)]

#define CREATE_HASH_ENTRY_AT(entry, table, h, k, done)	\
{							\
  int i;						\
  do							\
    {							\
      for (i = 0; i < N_SEEK; ++i)			\
	if (h[i].key == 0)				\
	  goto done;					\
      grow (&TABLE (table), &SIZE (table));		\
      h = HASH (table, k);				\
    }							\
  while (1);						\
 done:							\
  (entry) = &h[i];					\
}

#define CREATE_HASH_ENTRY(table, k, d, done)		\
  do							\
    {							\
      hash_entry_t *h = HASH (table, k);		\
      hash_entry_t *entry;				\
      CREATE_HASH_ENTRY_AT (entry, table, h, k, done);	\
      entry->key = (k);					\
      entry->data = (d);				\
    }							\
  while (0)

#define GET_CREATE_HASH_ENTRY(entry, table, k, done)		\
  do								\
    {								\
      hash_entry_t *h = HASH (table, k);			\
      int i;							\
      for (i = 0; i < N_SEEK; ++i)				\
	if (h[i].key == (void *) (k))				\
	  goto done;						\
       CREATE_HASH_ENTRY_AT (entry, table, h, k, gche ## done);	\
       entry->key = (k);					\
       entry->data = 0;						\
       break;							\
     done:							\
      (entry) = &h[i];						\
    }								\
  while (0)

static void
grow (hash_entry_t **table, int *size)
{
  hash_entry_t *oldtable = *table;
  int oldsize = *size + N_SEEK;
  hash_entry_t *TABLE (new) = 0;
  int SIZE (new);
  int i, j;
  SIZE (new) = 2 * (oldsize - N_SEEK + 1) - 1;
 again:
  TABLE (new) = realloc (TABLE (new),
			 sizeof (hash_entry_t) * (SIZE (new) + N_SEEK));
  memset (TABLE (new), 0, sizeof (hash_entry_t) * (SIZE (new) + N_SEEK));
  for (i = 0; i < oldsize; ++i)
    if (oldtable[i].key)
      {
	hash_entry_t *h = HASH (new, oldtable[i].key);
	for (j = 0; j < N_SEEK; ++j)
	  if (h[j].key == 0)
	    {
	      h[j] = oldtable[i];
	      goto next;
	    }
	SIZE (new) *= 2;
	goto again;
      next:
	;
      }
  if (table == &malloc_type)
    {
      /* relocate malloc_object entries */
      for (i = 0; i < oldsize; ++i)
	if (oldtable[i].key)
	  {
	    hash_entry_t *h = HASH (new, oldtable[i].key);
	    while (h->key != oldtable[i].key)
	      ++h;
	    oldtable[i].data = h;
	  }
      for (i = 0; i < malloc_object_size + N_SEEK; ++i)
	if (malloc_object[i].key)
	  malloc_object[i].data
	    = ((hash_entry_t *) malloc_object[i].data)->data;
    }
  free (*table);
  *table = TABLE (new);
  *size = SIZE (new);
}

void
scm_malloc_register (void *obj, const char *what)
{
  hash_entry_t *type;
  GET_CREATE_HASH_ENTRY (type, type, what, l1);
  type->data = (void *) ((int) type->data + 1);
  CREATE_HASH_ENTRY (object, obj, type, l2);
}

void
scm_malloc_unregister (void *obj)
{
  hash_entry_t *object, *type;
  GET_CREATE_HASH_ENTRY (object, object, obj, l1);
  type = (hash_entry_t *) object->data;
  if (type == 0)
    {
      fprintf (stderr,
	       "scm_gc_free called on object not allocated with scm_gc_malloc\n");
      abort ();
    }
  type->data = (void *) ((int) type->data - 1);
  object->key = 0;
}  

void
scm_malloc_reregister (void *old, void *new, const char *newwhat)
{
  hash_entry_t *object, *type;

  if (old == NULL)
    scm_malloc_register (new, newwhat);
  else
    {
      GET_CREATE_HASH_ENTRY (object, object, old, l1);
      type = (hash_entry_t *) object->data;
      if (type == 0)
	{
	  fprintf (stderr,
		   "scm_gc_realloc called on object not allocated "
		   "with scm_gc_malloc\n");
	  abort ();
	}
      if (strcmp ((char *) type->key, newwhat) != 0)
	{
	  if (strcmp (newwhat, "vector-set-length!") != 0)
	    {
	      fprintf (stderr,
		       "scm_gc_realloc called with arg %s, was %s\n",
		       newwhat,
		       (char *) type->key);
	      abort ();
	    }
	}
      if (new != old)
	{
	  object->key = 0;
	  CREATE_HASH_ENTRY (object, new, type, l2);
	}
    }
}

SCM_DEFINE (scm_malloc_stats, "malloc-stats", 0, 0, 0,
	    (),
	    "Return an alist ((@var{what} . @var{n}) ...) describing number\n"
	    "of malloced objects.\n"
	    "@var{what} is the second argument to @code{scm_gc_malloc},\n"
	    "@var{n} is the number of objects of that type currently\n"
	    "allocated.")
#define FUNC_NAME s_scm_malloc_stats
{
  SCM res = SCM_EOL;
  int i;
  for (i = 0; i < malloc_type_size + N_SEEK; ++i)
    if (malloc_type[i].key)
      res = scm_acons (scm_from_locale_string ((char *) malloc_type[i].key),
		       scm_from_int ((int) malloc_type[i].data),
		       res);
  return res;
}
#undef FUNC_NAME

void
scm_debug_malloc_prehistory ()
{
  malloc_type = malloc (sizeof (hash_entry_t)
			* (malloc_type_size + N_SEEK));
  memset (malloc_type, 0, sizeof (hash_entry_t) * (malloc_type_size + N_SEEK));
  malloc_object = malloc (sizeof (hash_entry_t)
			  * (malloc_object_size + N_SEEK));
  memset (malloc_object, 0, sizeof (hash_entry_t) * (malloc_object_size + N_SEEK));
}

void
scm_init_debug_malloc ()
{
#include "libguile/debug-malloc.x"
}

