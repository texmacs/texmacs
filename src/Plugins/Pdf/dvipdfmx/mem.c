/*  $Header: /home/cvsroot/dvipdfmx/src/mem.c,v 1.8 2009/09/18 23:56:02 matthias Exp $

    This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team <dvipdfmx@project.ktug.or.kr>
    
    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

#include <stdio.h>	
#include <stdlib.h>

#include "system.h"
#include "mem.h"
#include "error.h"

#ifdef MEM_DEBUG

#include "dpxutil.h"

#define MEM_LINE   128

static struct ht_table *mem_ht;
static long int mem_event;
static int mem_internal;
char *mem_str;

void mem_debug_init(void)
{
  mem_event = 0;
  mem_internal = 0;
  mem_ht = malloc(sizeof(struct ht_table));
  ht_init_table(mem_ht, NULL);
  fprintf(stderr, "*** Memory debugging started ***\n");
}

void mem_debug_check(void)
{
  struct ht_iter iter;

  fprintf(stderr, "*** Memory still in use ***\n");

  if (ht_set_iter(mem_ht, &iter) == 0) {
    do {
      int pl;
      void *p = *((void **) ht_iter_getkey(&iter, &pl));
      char *s = ht_iter_getval(&iter);

      fprintf(stderr, "%p %s\n", p, s);
    } while (!ht_iter_next(&iter));
  }
  ht_clear_iter(&iter);

  fprintf(stderr, "*** End of used memory ***\n");
}

void *mem_add(void *ptr, const char *file, const char *function, int line) {
  if (ptr && !mem_internal) {
    mem_internal = 1;
    mem_str = malloc(MEM_LINE);
    char **p = malloc(sizeof(ptr));
    *p = ptr;
    snprintf(mem_str, MEM_LINE, "(0x%08lx) %s (%s, %d)"
#ifdef __GNUC__
            ", %p" //" %p"
#endif
            , ++mem_event, function, file, line
#ifdef __GNUC__
	     , __builtin_return_address(1) //, __builtin_return_address(2)
#endif
            );
    ht_append_table(mem_ht, p, sizeof(ptr), mem_str);
    mem_internal = 0;
  }

  return ptr;
}

void *mem_remove(void *ptr, const char *file, const char *function, int line) {
  if (ptr && !mem_internal) {
    mem_internal = 1;
    if (!(mem_ht && ht_remove_table(mem_ht, &ptr, sizeof(ptr)))) {
      WARN("Trying to free non-allocated memory\n"
	   "%p %s (%s, %d)"
#ifdef __GNUC__
	   ", called from %p"
#endif
	   "\n", ptr, function, file, line
#ifdef __GNUC__
	   , __builtin_return_address(1)
#endif
	   );
    }
    mem_internal = 0;
  }

  return ptr;
}


#else /* ! MEM_DEBUG */

static long int mem_count;

void mem_debug_init(void)
{
  mem_count = 0;
}

void mem_debug_check(void)
{
  if (mem_count)
    WARN("%ld memory objects still allocated\n", mem_count);
}

void *mem_add(void *ptr) {
  if (ptr)
    mem_count++;

  return ptr;
}

void *mem_remove(void *ptr) {
  if (ptr)
    mem_count--;

  return ptr;
}

#endif /* MEM_DEBUG */


void *new (size_t size)
{
  void *result = malloc (size);
  if (!result) {
    ERROR("Out of memory - asked for %lu bytes\n", (unsigned long) size);
  }

  return result;
}

void *renew (void *mem, size_t size)
{
  if (size) {
    void *result = realloc (mem, size);
    if (!result) {
      ERROR("Out of memory - asked for %lu bytes\n", (unsigned long) size);
    }
    return result;
  } else {
    /* realloc may not return NULL if size == 0 */
    free(mem);
    return NULL;
  }
}
