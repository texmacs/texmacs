/* Copyright (C) 1995,1996,1998,2000,2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/dynwind.h"



/* {Strings}
 */


/* Stringbufs 
 *
 * XXX - keeping an accurate refcount during GC seems to be quite
 * tricky, so we just keep score of whether a stringbuf might be
 * shared, not wether it definitely is.  
 *
 * The scheme I (mvo) tried to keep an accurate reference count would
 * recount all strings that point to a stringbuf during the mark-phase
 * of the GC.  This was done since one cannot access the stringbuf of
 * a string when that string is freed (in order to decrease the
 * reference count).  The memory of the stringbuf might have been
 * reused already for something completely different.
 *
 * This recounted worked for a small number of threads beating on
 * cow-strings, but it failed randomly with more than 10 threads, say.
 * I couldn't figure out what went wrong, so I used the conservative
 * approach implemented below.
 * 
 * A stringbuf needs to know its length, but only so that it can be
 * reported when the stringbuf is freed.
 *
 * Stringbufs (and strings) are not stored very compactly: a stringbuf
 * has room for about 2*sizeof(scm_t_bits)-1 bytes additional
 * information.  As a compensation, the code below is made more
 * complicated by storing small strings inline in the double cell of a
 * stringbuf.  So we have fixstrings and bigstrings...
 */

#define STRINGBUF_F_SHARED      0x100
#define STRINGBUF_F_INLINE      0x200

#define STRINGBUF_TAG           scm_tc7_stringbuf
#define STRINGBUF_SHARED(buf)   (SCM_CELL_WORD_0(buf) & STRINGBUF_F_SHARED)
#define STRINGBUF_INLINE(buf)   (SCM_CELL_WORD_0(buf) & STRINGBUF_F_INLINE)

#define STRINGBUF_OUTLINE_CHARS(buf)   ((char *)SCM_CELL_WORD_1(buf))
#define STRINGBUF_OUTLINE_LENGTH(buf)  (SCM_CELL_WORD_2(buf))
#define STRINGBUF_INLINE_CHARS(buf)    ((char *)SCM_CELL_OBJECT_LOC(buf,1))
#define STRINGBUF_INLINE_LENGTH(buf)   (((size_t)SCM_CELL_WORD_0(buf))>>16)

#define STRINGBUF_CHARS(buf)  (STRINGBUF_INLINE (buf) \
                               ? STRINGBUF_INLINE_CHARS (buf) \
                               : STRINGBUF_OUTLINE_CHARS (buf))
#define STRINGBUF_LENGTH(buf) (STRINGBUF_INLINE (buf) \
                               ? STRINGBUF_INLINE_LENGTH (buf) \
                               : STRINGBUF_OUTLINE_LENGTH (buf))

#define STRINGBUF_MAX_INLINE_LEN (3*sizeof(scm_t_bits))

#define SET_STRINGBUF_SHARED(buf) \
  (SCM_SET_CELL_WORD_0 ((buf), SCM_CELL_WORD_0 (buf) | STRINGBUF_F_SHARED))

#if SCM_DEBUG
static size_t lenhist[1001];
#endif

static SCM
make_stringbuf (size_t len)
{
  /* XXX - for the benefit of SCM_STRING_CHARS, SCM_SYMBOL_CHARS and
     scm_i_symbol_chars, all stringbufs are null-terminated.  Once
     SCM_STRING_CHARS and SCM_SYMBOL_CHARS are removed and the code
     has been changed for scm_i_symbol_chars, this null-termination
     can be dropped.
  */

#if SCM_DEBUG
  if (len < 1000)
    lenhist[len]++;
  else
    lenhist[1000]++;
#endif

  if (len <= STRINGBUF_MAX_INLINE_LEN-1)
    {
      return scm_double_cell (STRINGBUF_TAG | STRINGBUF_F_INLINE | (len << 16),
			      0, 0, 0);
    }
  else
    {
      char *mem = scm_gc_malloc (len+1, "string");
      mem[len] = '\0';
      return scm_double_cell (STRINGBUF_TAG, (scm_t_bits) mem,
			      (scm_t_bits) len, (scm_t_bits) 0);
    }
}

/* Return a new stringbuf whose underlying storage consists of the LEN+1
   octets pointed to by STR (the last octet is zero).  */
SCM
scm_i_take_stringbufn (char *str, size_t len)
{
  scm_gc_register_collectable_memory (str, len + 1, "stringbuf");

  return scm_double_cell (STRINGBUF_TAG, (scm_t_bits) str,
			  (scm_t_bits) len, (scm_t_bits) 0);
}

SCM
scm_i_stringbuf_mark (SCM buf)
{
  return SCM_BOOL_F;
}

void
scm_i_stringbuf_free (SCM buf)
{
  if (!STRINGBUF_INLINE (buf))
    scm_gc_free (STRINGBUF_OUTLINE_CHARS (buf),
		 STRINGBUF_OUTLINE_LENGTH (buf) + 1, "string");
}

scm_i_pthread_mutex_t stringbuf_write_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Copy-on-write strings.
 */

#define STRING_TAG            scm_tc7_string

#define STRING_STRINGBUF(str) (SCM_CELL_OBJECT_1(str))
#define STRING_START(str)     ((size_t)SCM_CELL_WORD_2(str))
#define STRING_LENGTH(str)    ((size_t)SCM_CELL_WORD_3(str))

#define SET_STRING_STRINGBUF(str,buf) (SCM_SET_CELL_OBJECT_1(str,buf))
#define SET_STRING_START(str,start) (SCM_SET_CELL_WORD_2(str,start))

#define IS_STRING(str)        (SCM_NIMP(str) && SCM_TYP7(str) == STRING_TAG)

/* Read-only strings.
 */

#define RO_STRING_TAG         (scm_tc7_string + 0x200)
#define IS_RO_STRING(str)     (SCM_CELL_TYPE(str)==RO_STRING_TAG)

/* Mutation-sharing substrings
 */

#define SH_STRING_TAG       (scm_tc7_string + 0x100)

#define SH_STRING_STRING(sh) (SCM_CELL_OBJECT_1(sh))
/* START and LENGTH as for STRINGs. */

#define IS_SH_STRING(str)   (SCM_CELL_TYPE(str)==SH_STRING_TAG)

SCM
scm_i_make_string (size_t len, char **charsp)
{
  SCM buf = make_stringbuf (len);
  SCM res;
  if (charsp)
    *charsp = STRINGBUF_CHARS (buf);
  res = scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			 (scm_t_bits)0, (scm_t_bits) len);
  return res;
}

static void
validate_substring_args (SCM str, size_t start, size_t end)
{
  if (!IS_STRING (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  if (start > STRING_LENGTH (str))
    scm_out_of_range (NULL, scm_from_size_t (start));
  if (end > STRING_LENGTH (str) || end < start)
    scm_out_of_range (NULL, scm_from_size_t (end));
}

static inline void
get_str_buf_start (SCM *str, SCM *buf, size_t *start)
{
  *start = STRING_START (*str);
  if (IS_SH_STRING (*str))
    {
      *str = SH_STRING_STRING (*str);
      *start += STRING_START (*str);
    }
  *buf = STRING_STRINGBUF (*str);
}

SCM
scm_i_substring (SCM str, size_t start, size_t end)
{
  SCM buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)str_start + start,
			  (scm_t_bits) end - start);
}

SCM
scm_i_substring_read_only (SCM str, size_t start, size_t end)
{
  SCM buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (RO_STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)str_start + start,
			  (scm_t_bits) end - start);
}

SCM
scm_i_substring_copy (SCM str, size_t start, size_t end)
{
  size_t len = end - start;
  SCM buf, my_buf;
  size_t str_start;
  get_str_buf_start (&str, &buf, &str_start);
  my_buf = make_stringbuf (len);
  memcpy (STRINGBUF_CHARS (my_buf),
	  STRINGBUF_CHARS (buf) + str_start + start, len);
  scm_remember_upto_here_1 (buf);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(my_buf),
			  (scm_t_bits)0, (scm_t_bits) len);
}

SCM
scm_i_substring_shared (SCM str, size_t start, size_t end)
{
  if (start == 0 && end == STRING_LENGTH (str))
    return str;
  else 
    {
      size_t len = end - start;
      if (IS_SH_STRING (str))
	{
	  start += STRING_START (str);
	  str = SH_STRING_STRING (str);
	}
      return scm_double_cell (SH_STRING_TAG, SCM_UNPACK(str),
			      (scm_t_bits)start, (scm_t_bits) len);
    }
}

SCM
scm_c_substring (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring (str, start, end);
}

SCM
scm_c_substring_read_only (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_read_only (str, start, end);
}

SCM
scm_c_substring_copy (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_copy (str, start, end);
}

SCM
scm_c_substring_shared (SCM str, size_t start, size_t end)
{
  validate_substring_args (str, start, end);
  return scm_i_substring_shared (str, start, end);
}

SCM
scm_i_string_mark (SCM str)
{
  if (IS_SH_STRING (str))
    return SH_STRING_STRING (str);
  else
    return STRING_STRINGBUF (str);
}

void
scm_i_string_free (SCM str)
{
}

/* Internal accessors
 */

size_t
scm_i_string_length (SCM str)
{
  return STRING_LENGTH (str);
}

const char *
scm_i_string_chars (SCM str)
{
  SCM buf;
  size_t start;
  get_str_buf_start (&str, &buf, &start);
  return STRINGBUF_CHARS (buf) + start;
}

char *
scm_i_string_writable_chars (SCM orig_str)
{
  SCM buf, str = orig_str;
  size_t start;

  get_str_buf_start (&str, &buf, &start);
  if (IS_RO_STRING (str))
    scm_misc_error (NULL, "string is read-only: ~s", scm_list_1 (orig_str));

  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  if (STRINGBUF_SHARED (buf))
    {
      /* Clone stringbuf.  For this, we put all threads to sleep.
       */

      size_t len = STRING_LENGTH (str);
      SCM new_buf;

      scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);

      new_buf = make_stringbuf (len);
      memcpy (STRINGBUF_CHARS (new_buf),
	      STRINGBUF_CHARS (buf) + STRING_START (str), len);

      scm_i_thread_put_to_sleep ();
      SET_STRING_STRINGBUF (str, new_buf);
      start -= STRING_START (str);
      SET_STRING_START (str, 0);
      scm_i_thread_wake_up ();

      buf = new_buf;

      scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
    }

  return STRINGBUF_CHARS (buf) + start;
}

void
scm_i_string_stop_writing (void)
{
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
}

/* Symbols.
 
   Basic symbol creation and accessing is done here, the rest is in
   symbols.[hc].  This has been done to keep stringbufs and the
   internals of strings and string-like objects confined to this file.
*/

#define SYMBOL_STRINGBUF SCM_CELL_OBJECT_1

SCM
scm_i_make_symbol (SCM name, scm_t_bits flags,
		   unsigned long hash, SCM props)
{
  SCM buf;
  size_t start = STRING_START (name);
  size_t length = STRING_LENGTH (name);

  if (IS_SH_STRING (name))
    {
      name = SH_STRING_STRING (name);
      start += STRING_START (name);
    }
  buf = SYMBOL_STRINGBUF (name);

  if (start == 0 && length == STRINGBUF_LENGTH (buf))
    {
      /* reuse buf. */
      scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
      SET_STRINGBUF_SHARED (buf);
      scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
    }
  else
    {
      /* make new buf. */
      SCM new_buf = make_stringbuf (length);
      memcpy (STRINGBUF_CHARS (new_buf),
	      STRINGBUF_CHARS (buf) + start, length);
      buf = new_buf;
    }
  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

SCM
scm_i_c_make_symbol (const char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props)
{
  SCM buf = make_stringbuf (len);
  memcpy (STRINGBUF_CHARS (buf), name, len);

  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

/* Return a new symbol that uses the LEN bytes pointed to by NAME as its
   underlying storage.  */
SCM
scm_i_c_take_symbol (char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props)
{
  SCM buf = scm_i_take_stringbufn (name, len);

  return scm_double_cell (scm_tc7_symbol | flags, SCM_UNPACK (buf),
			  (scm_t_bits) hash, SCM_UNPACK (props));
}

size_t
scm_i_symbol_length (SCM sym)
{
  return STRINGBUF_LENGTH (SYMBOL_STRINGBUF (sym));
}

size_t
scm_c_symbol_length (SCM sym)
#define FUNC_NAME "scm_c_symbol_length"
{
  SCM_VALIDATE_SYMBOL (1, sym);

  return STRINGBUF_LENGTH (SYMBOL_STRINGBUF (sym));
}
#undef FUNC_NAME

const char *
scm_i_symbol_chars (SCM sym)
{
  SCM buf = SYMBOL_STRINGBUF (sym);
  return STRINGBUF_CHARS (buf);
}

SCM
scm_i_symbol_mark (SCM sym)
{
  scm_gc_mark (SYMBOL_STRINGBUF (sym));
  return SCM_CELL_OBJECT_3 (sym);
}

void
scm_i_symbol_free (SCM sym)
{
}

SCM
scm_i_symbol_substring (SCM sym, size_t start, size_t end)
{
  SCM buf = SYMBOL_STRINGBUF (sym);
  scm_i_pthread_mutex_lock (&stringbuf_write_mutex);
  SET_STRINGBUF_SHARED (buf);
  scm_i_pthread_mutex_unlock (&stringbuf_write_mutex);
  return scm_double_cell (RO_STRING_TAG, SCM_UNPACK (buf),
			  (scm_t_bits)start, (scm_t_bits) end - start);
}

/* Debugging
 */

#if SCM_DEBUG

SCM scm_sys_string_dump (SCM);
SCM scm_sys_symbol_dump (SCM);
SCM scm_sys_stringbuf_hist (void);

SCM_DEFINE (scm_sys_string_dump, "%string-dump", 1, 0, 0,
	    (SCM str),
	    "")
#define FUNC_NAME s_scm_sys_string_dump
{
  SCM_VALIDATE_STRING (1, str);
  fprintf (stderr, "%p:\n", str);
  fprintf (stderr, " start: %u\n", STRING_START (str));
  fprintf (stderr, " len:   %u\n", STRING_LENGTH (str));
  if (IS_SH_STRING (str))
    {
      fprintf (stderr, " string: %p\n", SH_STRING_STRING (str));
      fprintf (stderr, "\n");
      scm_sys_string_dump (SH_STRING_STRING (str));
    }
  else
    {
      SCM buf = STRING_STRINGBUF (str);
      fprintf (stderr, " buf:   %p\n", buf);
      fprintf (stderr, "  chars:  %p\n", STRINGBUF_CHARS (buf));
      fprintf (stderr, "  length: %u\n", STRINGBUF_LENGTH (buf));
      fprintf (stderr, "  flags: %x\n", (SCM_CELL_WORD_0 (buf) & 0x300));
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_symbol_dump, "%symbol-dump", 1, 0, 0,
	    (SCM sym),
	    "")
#define FUNC_NAME s_scm_sys_symbol_dump
{
  SCM_VALIDATE_SYMBOL (1, sym);
  fprintf (stderr, "%p:\n", sym);
  fprintf (stderr, " hash: %lu\n", scm_i_symbol_hash (sym));
  {
    SCM buf = SYMBOL_STRINGBUF (sym);
    fprintf (stderr, " buf: %p\n", buf);
    fprintf (stderr, "  chars:  %p\n", STRINGBUF_CHARS (buf));
    fprintf (stderr, "  length: %u\n", STRINGBUF_LENGTH (buf));
    fprintf (stderr, "  shared: %u\n", STRINGBUF_SHARED (buf));
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_sys_stringbuf_hist, "%stringbuf-hist", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_sys_stringbuf_hist
{
  int i;
  for (i = 0; i < 1000; i++)
    if (lenhist[i])
      fprintf (stderr, " %3d: %u\n", i, lenhist[i]);
  fprintf (stderr, ">999: %u\n", lenhist[1000]);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif



SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a string, else @code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return scm_from_bool (IS_STRING (obj));
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_scm_list_to_string, "list->string", 1, 0, 0, scm_string);

SCM_DEFINE (scm_string, "string", 0, 0, 1, 
            (SCM chrs),
	    "@deffnx {Scheme Procedure} list->string chrs\n"
	    "Return a newly allocated string composed of the arguments,\n"
	    "@var{chrs}.")
#define FUNC_NAME s_scm_string
{
  SCM result;
  size_t len;
  char *data;

  {
    long i = scm_ilength (chrs);

    SCM_ASSERT (i >= 0, chrs, SCM_ARG1, FUNC_NAME);
    len = i;
  }

  result = scm_i_make_string (len, &data);
  while (len > 0 && scm_is_pair (chrs))
    {
      SCM elt = SCM_CAR (chrs);

      SCM_VALIDATE_CHAR (SCM_ARGn, elt);
      *data++ = SCM_CHAR (elt);
      chrs = SCM_CDR (chrs);
      len--;
    }
  if (len > 0)
    scm_misc_error (NULL, "list changed while constructing string", SCM_EOL);
  if (!scm_is_null (chrs))
    scm_wrong_type_arg_msg (NULL, 0, chrs, "proper list");

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
            (SCM k, SCM chr),
	    "Return a newly allocated string of\n"
            "length @var{k}.  If @var{chr} is given, then all elements of\n"
	    "the string are initialized to @var{chr}, otherwise the contents\n"
	    "of the @var{string} are unspecified.")
#define FUNC_NAME s_scm_make_string
{
  return scm_c_make_string (scm_to_size_t (k), chr);
}
#undef FUNC_NAME

SCM
scm_c_make_string (size_t len, SCM chr)
#define FUNC_NAME NULL
{
  char *dst;
  SCM res = scm_i_make_string (len, &dst);

  if (!SCM_UNBNDP (chr))
    {
      SCM_VALIDATE_CHAR (0, chr);
      memset (dst, SCM_CHAR (chr), len);
    }

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return scm_from_size_t (STRING_LENGTH (string));
}
#undef FUNC_NAME

size_t
scm_c_string_length (SCM string)
{
  if (!IS_STRING (string))
    scm_wrong_type_arg_msg (NULL, 0, string, "string");
  return STRING_LENGTH (string);
}

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
	    "Return character @var{k} of @var{str} using zero-origin\n"
	    "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  size_t len;
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);

  len = scm_i_string_length (str);
  if (SCM_LIKELY (len > 0))
    idx = scm_to_unsigned_integer (k, 0, len - 1);
  else
    scm_out_of_range (NULL, k);

  return SCM_MAKE_CHAR (scm_i_string_chars (str)[idx]);
}
#undef FUNC_NAME

SCM
scm_c_string_ref (SCM str, size_t p)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  return SCM_MAKE_CHAR (scm_i_string_chars (str)[p]);
}

SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
	    "Store @var{chr} in element @var{k} of @var{str} and return\n"
	    "an unspecified value. @var{k} must be a valid index of\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
  size_t len;
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);

  len = scm_i_string_length (str);
  if (SCM_LIKELY (len > 0))
    idx = scm_to_unsigned_integer (k, 0, len - 1);
  else
    scm_out_of_range (NULL, k);

  SCM_VALIDATE_CHAR (3, chr);
  {
    char *dst = scm_i_string_writable_chars (str);
    dst[idx] = SCM_CHAR (chr);
    scm_i_string_stop_writing ();
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_string_set_x (SCM str, size_t p, SCM chr)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  {
    char *dst = scm_i_string_writable_chars (str);
    dst[p] = SCM_CHAR (chr);
    scm_i_string_stop_writing ();
  }
}

SCM_DEFINE (scm_substring, "substring", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_read_only, "substring/read-only", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n"
	    "\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).\n"
	    "\n"
	    "The returned string is read-only.\n")
#define FUNC_NAME s_scm_substring_read_only
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring_read_only (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_copy, "substring/copy", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_copy
{
  /* For the Scheme version, START is mandatory, but for the C
     version, it is optional.  See scm_string_copy in srfi-13.c for a
     rationale.
  */

  size_t from, to;

  SCM_VALIDATE_STRING (1, str);
  scm_i_get_substring_spec (scm_i_string_length (str),
			    start, &from, end, &to);
  return scm_i_substring_copy (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_shared, "substring/shared", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return string that indirectly refers to the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_shared
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_i_substring_shared (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
            (SCM args),
	    "Return a newly allocated string whose characters form the\n"
            "concatenation of the given strings, @var{args}.")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  size_t i = 0;
  SCM l, s;
  char *data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !scm_is_null (l); l = SCM_CDR (l)) 
    {
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      i += scm_i_string_length (s);
    }
  res = scm_i_make_string (i, &data);
  for (l = args; !scm_is_null (l); l = SCM_CDR (l)) 
    {
      size_t len;
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      len = scm_i_string_length (s);
      memcpy (data, scm_i_string_chars (s), len);
      data += len;
      scm_remember_upto_here_1 (s);
    }
  return res;
}
#undef FUNC_NAME

int
scm_is_string (SCM obj)
{
  return IS_STRING (obj);
}

SCM
scm_from_locale_stringn (const char *str, size_t len)
{
  SCM res;
  char *dst;

  if (len == (size_t)-1)
    len = strlen (str);
  res = scm_i_make_string (len, &dst);
  memcpy (dst, str, len);
  return res;
}

SCM
scm_from_locale_string (const char *str)
{
  return scm_from_locale_stringn (str, -1);
}

SCM
scm_take_locale_stringn (char *str, size_t len)
{
  SCM buf, res;

  if (len == (size_t)-1)
    len = strlen (str);
  else
    {
      /* Ensure STR is null terminated.  A realloc for 1 extra byte should
         often be satisfied from the alignment padding after the block, with
         no actual data movement.  */
      str = scm_realloc (str, len+1);
      str[len] = '\0';
    }

  buf = scm_i_take_stringbufn (str, len);
  res = scm_double_cell (STRING_TAG,
                         SCM_UNPACK (buf),
                         (scm_t_bits) 0, (scm_t_bits) len);
  return res;
}

SCM
scm_take_locale_string (char *str)
{
  return scm_take_locale_stringn (str, -1);
}

char *
scm_to_locale_stringn (SCM str, size_t *lenp)
{
  char *res;
  size_t len;

  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = scm_i_string_length (str);
  res = scm_malloc (len + ((lenp==NULL)? 1 : 0));
  memcpy (res, scm_i_string_chars (str), len);
  if (lenp == NULL)
    {
      res[len] = '\0';
      if (strlen (res) != len)
	{
	  free (res);
	  scm_misc_error (NULL,
			  "string contains #\\nul character: ~S",
			  scm_list_1 (str));
	}
    }
  else
    *lenp = len;

  scm_remember_upto_here_1 (str);
  return res;
}

char *
scm_to_locale_string (SCM str)
{
  return scm_to_locale_stringn (str, NULL);
}

size_t
scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
{
  size_t len;
  
  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = scm_i_string_length (str);
  memcpy (buf, scm_i_string_chars (str), (len > max_len)? max_len : len);
  scm_remember_upto_here_1 (str);
  return len;
}

/* converts C scm_array of strings to SCM scm_list of strings. */
/* If argc < 0, a null terminated scm_array is assumed. */
SCM 
scm_makfromstrs (int argc, char **argv)
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_from_locale_string (argv[i]), lst);
  return lst;
}

/* Return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */

char **
scm_i_allocate_string_pointers (SCM list)
{
  char **result;
  int len = scm_ilength (list);
  int i;

  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  scm_dynwind_begin (0);

  result = (char **) scm_malloc ((len + 1) * sizeof (char *));
  result[len] = NULL;
  scm_dynwind_unwind_handler (free, result, 0);

  /* The list might be have been modified in another thread, so
     we check LIST before each access.
   */
  for (i = 0; i < len && scm_is_pair (list); i++)
    {
      result[i] = scm_to_locale_string (SCM_CAR (list));
      list = SCM_CDR (list);
    }

  scm_dynwind_end ();
  return result;
}

void
scm_i_free_string_pointers (char **pointers)
{
  int i;
  
  for (i = 0; pointers[i]; i++)
    free (pointers[i]);
  free (pointers);
}

void
scm_i_get_substring_spec (size_t len,
			  SCM start, size_t *cstart,
			  SCM end, size_t *cend)
{
  if (SCM_UNBNDP (start))
    *cstart = 0;
  else
    *cstart = scm_to_unsigned_integer (start, 0, len);

  if (SCM_UNBNDP (end))
    *cend = len;
  else
    *cend = scm_to_unsigned_integer (end, *cstart, len);
}
		  
#if SCM_ENABLE_DEPRECATED

/* When these definitions are removed, it becomes reasonable to use
   read-only strings for string literals.  For that, change the reader
   to create string literals with scm_c_substring_read_only instead of
   with scm_c_substring_copy.
*/

int
scm_i_deprecated_stringp (SCM str)
{
  scm_c_issue_deprecation_warning
    ("SCM_STRINGP is deprecated.  Use scm_is_string instead.");
  
  return scm_is_string (str);
}

char *
scm_i_deprecated_string_chars (SCM str)
{
  char *chars;

  scm_c_issue_deprecation_warning
    ("SCM_STRING_CHARS is deprecated.  See the manual for alternatives.");

  /* We don't accept shared substrings here since they are not
     null-terminated.
  */
  if (IS_SH_STRING (str))
    scm_misc_error (NULL, 
		    "SCM_STRING_CHARS does not work with shared substrings.",
		    SCM_EOL);

  /* We explicitly test for read-only strings to produce a better
     error message.
  */

  if (IS_RO_STRING (str))
    scm_misc_error (NULL, 
		    "SCM_STRING_CHARS does not work with read-only strings.",
		    SCM_EOL);
    
  /* The following is still wrong, of course...
   */
  chars = scm_i_string_writable_chars (str);
  scm_i_string_stop_writing ();
  return chars;
}

size_t
scm_i_deprecated_string_length (SCM str)
{
  scm_c_issue_deprecation_warning
    ("SCM_STRING_LENGTH is deprecated.  Use scm_c_string_length instead.");
  return scm_c_string_length (str);
}

#endif

void
scm_init_strings ()
{
  scm_nullstr = scm_i_make_string (0, NULL);

#include "libguile/strings.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
