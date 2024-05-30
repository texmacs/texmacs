/* classes: h_files */

#ifndef SCM_STRINGS_H
#define SCM_STRINGS_H

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2004, 2005, 2006 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"



/* String representation.

   A string is a piece of a stringbuf.  A stringbuf can be used by
   more than one string.  When a string is written to and the
   stringbuf of that string is used by more than one string, a new
   stringbuf is created.  That is, strings are copy-on-write.  This
   behavior can be used to make the substring operation quite
   efficient.

   The implementation is tuned so that mutating a string is costly,
   but just reading it is cheap and lock-free.

   There are also mutation-sharing strings.  They refer to a part of
   an ordinary string.  Writing to a mutation-sharing string just
   writes to the ordinary string.


   Internal, low level interface to the character arrays

   - Use scm_i_string_chars to get a pointer to the byte array of a
     string for reading.  Use scm_i_string_length to get the number of
     bytes in that array.  The array is not null-terminated.

   - The array is valid as long as the corresponding SCM object is
     protected but only until the next SCM_TICK.  During such a 'safe
     point', strings might change their representation.

   - Use scm_i_string_writable_chars to get the same pointer as with
     scm_i_string_chars, but for reading and writing.  This is a
     potentially costly operation since it implements the
     copy-on-write behavior.  When done with the writing, call
     scm_i_string_stop_writing.  You must do this before the next
     SCM_TICK.  (This means, before calling almost any other scm_
     function and you can't allow throws, of course.)

   - New strings can be created with scm_i_make_string.  This gives
     access to a writable pointer that remains valid as long as nobody
     else makes a copy-on-write substring of the string.  Do not call
     scm_i_string_stop_writing for this pointer.

   Legacy interface

   - SCM_STRINGP is just scm_is_string.

   - SCM_STRING_CHARS uses scm_i_string_writable_chars and immediately
     calls scm_i_stop_writing, hoping for the best.  SCM_STRING_LENGTH
     is the same as scm_i_string_length.  SCM_STRING_CHARS will throw
     an error for for strings that are not null-terminated.
*/

SCM_API SCM scm_string_p (SCM x);
SCM_API SCM scm_string (SCM chrs);
SCM_API SCM scm_make_string (SCM k, SCM chr);
SCM_API SCM scm_string_length (SCM str);
SCM_API SCM scm_string_ref (SCM str, SCM k);
SCM_API SCM scm_string_set_x (SCM str, SCM k, SCM chr);
SCM_API SCM scm_substring (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_read_only (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_shared (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_copy (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_append (SCM args);

SCM_API SCM scm_c_make_string (size_t len, SCM chr);
SCM_API size_t scm_c_string_length (SCM str);
SCM_API size_t scm_c_symbol_length (SCM sym);
SCM_API SCM scm_c_string_ref (SCM str, size_t pos);
SCM_API void scm_c_string_set_x (SCM str, size_t pos, SCM chr);
SCM_API SCM scm_c_substring (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_read_only (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_shared (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_copy (SCM str, size_t start, size_t end);

SCM_API int scm_is_string (SCM x);
SCM_API SCM scm_from_locale_string (const char *str);
SCM_API SCM scm_from_locale_stringn (const char *str, size_t len);
SCM_API SCM scm_take_locale_string (char *str);
SCM_API SCM scm_take_locale_stringn (char *str, size_t len);
SCM_API char *scm_to_locale_string (SCM str);
SCM_API char *scm_to_locale_stringn (SCM str, size_t *lenp);
SCM_API size_t scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len);

SCM_API SCM scm_makfromstrs (int argc, char **argv);

/* internal accessor functions.  Arguments must be valid. */

SCM_API SCM scm_i_make_string (size_t len, char **datap);
SCM_API SCM scm_i_substring (SCM str, size_t start, size_t end);
SCM_API SCM scm_i_substring_read_only (SCM str, size_t start, size_t end);
SCM_API SCM scm_i_substring_shared (SCM str, size_t start, size_t end);
SCM_API SCM scm_i_substring_copy (SCM str, size_t start, size_t end);
SCM_API size_t scm_i_string_length (SCM str);
SCM_API const char *scm_i_string_chars (SCM str);
SCM_API char *scm_i_string_writable_chars (SCM str);
SCM_API void scm_i_string_stop_writing (void);

/* internal functions related to symbols. */

SCM_API SCM scm_i_make_symbol (SCM name, scm_t_bits flags, 
			       unsigned long hash, SCM props);
SCM_API SCM
scm_i_c_make_symbol (const char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props);
SCM_API SCM
scm_i_c_take_symbol (char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props);
SCM_API const char *scm_i_symbol_chars (SCM sym);
SCM_API size_t scm_i_symbol_length (SCM sym);
SCM_API SCM scm_i_symbol_substring (SCM sym, size_t start, size_t end);

/* internal GC functions. */

SCM_API SCM scm_i_string_mark (SCM str);
SCM_API SCM scm_i_stringbuf_mark (SCM buf);
SCM_API SCM scm_i_symbol_mark (SCM buf);
SCM_API void scm_i_string_free (SCM str);
SCM_API void scm_i_stringbuf_free (SCM buf);
SCM_API void scm_i_symbol_free (SCM sym);

/* internal utility functions. */

SCM_API char **scm_i_allocate_string_pointers (SCM list);
SCM_API void scm_i_free_string_pointers (char **pointers);
SCM_API void scm_i_get_substring_spec (size_t len,
				       SCM start, size_t *cstart,
				       SCM end, size_t *cend);
SCM_API SCM scm_i_take_stringbufn (char *str, size_t len);

/* deprecated stuff */

#if SCM_ENABLE_DEPRECATED

SCM_API int scm_i_deprecated_stringp (SCM obj);
SCM_API char *scm_i_deprecated_string_chars (SCM str);
SCM_API size_t scm_i_deprecated_string_length (SCM str);

#define SCM_STRINGP(x)       scm_i_deprecated_stringp(x)
#define SCM_STRING_CHARS(x)  scm_i_deprecated_string_chars(x)
#define SCM_STRING_LENGTH(x) scm_i_deprecated_string_length(x)
#define SCM_STRING_UCHARS(str) ((unsigned char *)SCM_STRING_CHARS (str))

#endif

SCM_API void scm_init_strings (void);

#endif  /* SCM_STRINGS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
