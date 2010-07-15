/*  $Header: /home/cvsroot/dvipdfmx/src/dpxutil.h,v 1.11 2009/09/20 14:24:51 matthias Exp $

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

#ifndef _DPXUTIL_H_
#define _DPXUTIL_H_

#undef  MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#undef  MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#undef  ABS
#define ABS(a)    (((a) < 0) ? -(a) : (a))

#ifndef is_space
#define is_space(c) ((c) == ' '  || (c) == '\t' || (c) == '\f' || \
		     (c) == '\r' || (c) == '\n' || (c) == '\0')
#endif
#ifndef is_delim
#define is_delim(c) ((c) == '(' || (c) == '/' || \
                     (c) == '<' || (c) == '>' || \
		     (c) == '[' || (c) == ']' || \
                     (c) == '{' || (c) == '}' || \
                     (c) == '%')
#endif

extern void skip_white_spaces (unsigned char **s, unsigned char *endptr);
extern int  xtoi     (char c);
extern int  getxpair (unsigned char **str);
extern int  putxpair (unsigned char c, char **str);
extern int  sputx    (unsigned char c, char **buf, char *endptr);

extern unsigned char ostrtouc (unsigned char **inbuf,
			       unsigned char *inbufend, unsigned char *valid);
extern unsigned char esctouc  (unsigned char **inbuf,
			       unsigned char *inbufend, unsigned char *valid);

#define HASH_TABLE_SIZE 503

struct ht_entry {
  char  *key;
  int    keylen;

  void  *value;

  struct ht_entry *next;
};

struct ht_table {
  long   count;
  void (*hval_free_fn) (void *);
  struct ht_entry *table[HASH_TABLE_SIZE];
};

typedef struct ht_table ht_table;

#define HT_NEW      0
#define HT_REPLACE  1
#define HT_KEEP     2

extern void  ht_init_table   (struct ht_table *ht,
                              void (*hval_free_fn) (void *));
extern void  ht_clear_table  (struct ht_table *ht);
extern long  ht_table_size   (struct ht_table *ht);
extern void *ht_lookup_table (struct ht_table *ht,
                              const void *key, int keylen);
extern void  ht_modify_table (struct ht_table *ht,
			      const void *key, int keylen,
                              void *value, int mode);
#define ht_append_table(ht, key, keylen, value) \
          ht_modify_table(ht, key, keylen, value, HT_NEW)
#define ht_insert_table(ht, key, keylen, value) \
          ht_modify_table(ht, key, keylen, value, HT_REPLACE)
extern int   ht_remove_table (struct ht_table *ht,
			      const void *key, int keylen);

struct ht_iter {
  int    index;
  void  *curr;
  struct ht_table *hash;
};

extern int   ht_set_iter    (struct ht_table *ht, struct ht_iter *iter);
extern void  ht_clear_iter  (struct ht_iter *iter);
extern char *ht_iter_getkey (struct ht_iter *iter, int *keylen);
extern void *ht_iter_getval (struct ht_iter *iter);
extern int   ht_iter_next   (struct ht_iter *iter);

extern char *parse_float_decimal (char **pp, char *endptr);
extern char *parse_c_string      (char **pp, char *endptr);
extern char *parse_c_ident       (char **pp, char *endptr);

#endif /* _DPXUTIL_H_ */
