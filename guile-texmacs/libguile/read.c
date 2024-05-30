/* Copyright (C) 1995,1996,1997,1999,2000,2001,2003, 2004, 2006, 2007, 2008 Free Software
 * Foundation, Inc.
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

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/unif.h"
#include "libguile/keywords.h"
#include "libguile/alist.h"
#include "libguile/srcprop.h"
#include "libguile/hashtab.h"
#include "libguile/hash.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/validate.h"
#include "libguile/srfi-4.h"
#include "libguile/srfi-13.h"

#include "libguile/read.h"



SCM_GLOBAL_SYMBOL (scm_sym_dot, ".");
SCM_SYMBOL (scm_keyword_prefix, "prefix");
SCM_SYMBOL (scm_keyword_postfix, "postfix");

scm_t_option scm_read_opts[] = {
  { SCM_OPTION_BOOLEAN, "copy", 0,
    "Copy source code expressions." },
  { SCM_OPTION_BOOLEAN, "positions", 0,
    "Record positions of source code expressions." },
  { SCM_OPTION_BOOLEAN, "case-insensitive", 0,
    "Convert symbols to lower case."},
  { SCM_OPTION_SCM, "keywords", SCM_UNPACK (SCM_BOOL_F),
    "Style of keyword recognition: #f, 'prefix or 'postfix."}
#if SCM_ENABLE_ELISP
  ,
  { SCM_OPTION_BOOLEAN, "elisp-vectors", 0,
    "Support Elisp vector syntax, namely `[...]'."},
  { SCM_OPTION_BOOLEAN, "elisp-strings", 0,
    "Support `\\(' and `\\)' in strings."}
#endif
};

/*
  Give meaningful error messages for errors

  We use the format

  FILE:LINE:COL: MESSAGE
  This happened in ....

  This is not standard GNU format, but the test-suite likes the real
  message to be in front.

 */


void
scm_i_input_error (char const *function,
		   SCM port, const char *message, SCM arg)
{
  SCM fn = (scm_is_string (SCM_FILENAME(port))
	    ? SCM_FILENAME(port)
	    : scm_from_locale_string ("#<unknown port>"));

  SCM string_port = scm_open_output_string ();
  SCM string = SCM_EOL;
  scm_simple_format (string_port,
		     scm_from_locale_string ("~A:~S:~S: ~A"),
		     scm_list_4 (fn,
				 scm_from_long (SCM_LINUM (port) + 1),
				 scm_from_int (SCM_COL (port) + 1),
				 scm_from_locale_string (message)));
    
  string = scm_get_output_string (string_port);
  scm_close_output_port (string_port);
  scm_error_scm (scm_from_locale_symbol ("read-error"),
		 function? scm_from_locale_string (function) : SCM_BOOL_F,
		 string,
		 arg,
		 SCM_BOOL_F);
}


SCM_DEFINE (scm_read_options, "read-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the read options. Instead of using\n"
	    "this procedure directly, use the procedures @code{read-enable},\n"
	    "@code{read-disable}, @code{read-set!} and @code{read-options}.")
#define FUNC_NAME s_scm_read_options
{
  SCM ans = scm_options (setting,
			 scm_read_opts,
			 SCM_N_READ_OPTIONS,
			 FUNC_NAME);
  if (SCM_COPY_SOURCE_P)
    SCM_RECORD_POSITIONS_P = 1;
  return ans;
}
#undef FUNC_NAME

/* An association list mapping extra hash characters to procedures.  */
static SCM *scm_read_hash_procedures;



/* Token readers.  */


/* Size of the C buffer used to read symbols and numbers.  */
#define READER_BUFFER_SIZE            128

/* Size of the C buffer used to read strings.  */
#define READER_STRING_BUFFER_SIZE     512

/* The maximum size of Scheme character names.  */
#define READER_CHAR_NAME_MAX_SIZE      50


/* `isblank' is only in C99.  */
#define CHAR_IS_BLANK_(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n')	\
   || ((_chr) == '\f') || ((_chr) == '\r'))

#ifdef MSDOS
# define CHAR_IS_BLANK(_chr)			\
  ((CHAR_IS_BLANK_ (chr)) || ((_chr) == 26))
#else
# define CHAR_IS_BLANK CHAR_IS_BLANK_
#endif


/* R5RS one-character delimiters (see section 7.1.1, ``Lexical
   structure'').  */
#define CHAR_IS_R5RS_DELIMITER(c)				\
  (CHAR_IS_BLANK (c)						\
   || (c == ')') || (c == '(') || (c == ';') || (c == '"'))

#define CHAR_IS_DELIMITER  CHAR_IS_R5RS_DELIMITER

/* Exponent markers, as defined in section 7.1.1 of R5RS, ``Lexical
   Structure''.  */
#define CHAR_IS_EXPONENT_MARKER(_chr)				\
  (((_chr) == 'e') || ((_chr) == 's') || ((_chr) == 'f')	\
   || ((_chr) == 'd') || ((_chr) == 'l'))

/* An inlinable version of `scm_c_downcase ()'.  */
#define CHAR_DOWNCASE(_chr)				\
  (((_chr) <= UCHAR_MAX) ? tolower (_chr) : (_chr))


#ifndef HAVE_DECL_STRNCASECMP
extern int strncasecmp (char const *s1, char const *s2, size_t n);
#endif

#ifndef HAVE_STRNCASECMP
/* XXX: Use Gnulib's `strncasecmp ()'.  */

static int
strncasecmp (const char *s1, const char *s2, size_t len2)
{
  while (*s1 && *s2 && len2 > 0)
    {
      int c1 = *s1, c2 = *s2;

      if (CHAR_DOWNCASE (c1) != CHAR_DOWNCASE (c2))
	return 0;
      else
	{
	  ++s1;
	  ++s2;
	  --len2;
	}
    }
  return !(*s1 || *s2 || len2 > 0);
}
#endif


/* Read an SCSH block comment.  */
static inline SCM scm_read_scsh_block_comment (int chr, SCM port);

/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Return
   zero if the whole token fits in BUF, non-zero otherwise.  */
static inline int
read_token (SCM port, char *buf, size_t buf_size, size_t *read)
{
  *read = 0;

  while (*read < buf_size)
    {
      int chr;

      chr = scm_getc (port);
      chr = (SCM_CASE_INSENSITIVE_P ? CHAR_DOWNCASE (chr) : chr);

      if (chr == EOF)
	return 0;
      else if (CHAR_IS_DELIMITER (chr))
	{
	  scm_ungetc (chr, port);
	  return 0;
	}
      else
	{
	  *buf = (char) chr;
	  buf++, (*read)++;
	}
    }

  return 1;
}


/* Skip whitespace from PORT and return the first non-whitespace character
   read.  Raise an error on end-of-file.  */
static int
flush_ws (SCM port, const char *eoferr)
{
  register int c;
  while (1)
    switch (c = scm_getc (port))
      {
      case EOF:
      goteof:
	if (eoferr)
	  {
	    scm_i_input_error (eoferr,
			       port,
			       "end of file",
			       SCM_EOL);
	  }
	return c;

      case ';':
      lp:
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto goteof;
	  default:
	    goto lp;
	  case SCM_LINE_INCREMENTORS:
	    break;
	  }
	break;

      case '#':
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    eoferr = "read_sharp";
	    goto goteof;
	  case '!':
	    scm_read_scsh_block_comment (c, port);
	    break;
	  default:
	    scm_ungetc (c, port);
	    return '#';
	  }
	break;

      case SCM_LINE_INCREMENTORS:
      case SCM_SINGLE_SPACES:
      case '\t':
	break;

      default:
	return c;
      }

  return 0;
}



/* Token readers.  */

static SCM scm_read_expression (SCM port);
static SCM scm_read_sharp (int chr, SCM port);
static SCM scm_get_hash_procedure (int c);
static SCM recsexpr (SCM obj, long line, int column, SCM filename);


static SCM
scm_read_sexp (int chr, SCM port)
#define FUNC_NAME "scm_i_lreadparen"
{
  register int c;
  register SCM tmp;
  register SCM tl, ans = SCM_EOL;
  SCM tl2 = SCM_EOL, ans2 = SCM_EOL, copy = SCM_BOOL_F;
  static const int terminating_char = ')';

  /* Need to capture line and column numbers here. */
  long line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;


  c = flush_ws (port, FUNC_NAME);
  if (terminating_char == c)
    return SCM_EOL;

  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot,
		 (tmp = scm_read_expression (port))))
    {
      ans = scm_read_expression (port);
      if (terminating_char != (c = flush_ws (port, FUNC_NAME)))
	scm_i_input_error (FUNC_NAME, port, "missing close paren",
			   SCM_EOL);
      return ans;
    }

  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);

  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (scm_is_pair (tmp)
			   ? copy
			   : tmp,
			   SCM_EOL);

  while (terminating_char != (c = flush_ws (port, FUNC_NAME)))
    {
      SCM new_tail;

      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot,
		     (tmp = scm_read_expression (port))))
	{
	  SCM_SETCDR (tl, tmp = scm_read_expression (port));

	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp) ? copy : tmp,
				       SCM_EOL));

	  c = flush_ws (port, FUNC_NAME);
	  if (terminating_char != c)
	    scm_i_input_error (FUNC_NAME, port,
			       "in pair: missing close paren", SCM_EOL);
	  goto exit;
	}

      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;

      if (SCM_COPY_SOURCE_P)
	{
	  SCM new_tail2 = scm_cons (scm_is_pair (tmp)
				    ? copy
				    : tmp, SCM_EOL);
	  SCM_SETCDR (tl2, new_tail2);
	  tl2 = new_tail2;
	}
    }

 exit:
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash,
		      ans,
		      scm_make_srcprops (line, column,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? ans2
					 : SCM_UNDEFINED,
					 SCM_EOL));
  return ans;
}
#undef FUNC_NAME

static SCM
scm_read_string (int chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  /* For strings smaller than C_STR, this function creates only one Scheme
     object (the string returned).  */

  SCM str = SCM_BOOL_F;
  char c_str[READER_STRING_BUFFER_SIZE];
  unsigned c_str_len = 0;
  int c;

  while ('"' != (c = scm_getc (port)))
    {
      if (c == EOF)
	str_eof: scm_i_input_error (FUNC_NAME, port,
				    "end of file in string constant",
				    SCM_EOL);

      if (c_str_len + 1 >= sizeof (c_str))
	{
	  /* Flush the C buffer onto a Scheme string.  */
	  SCM addy;

	  if (str == SCM_BOOL_F)
	    str = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));

	  addy = scm_from_locale_stringn (c_str, c_str_len);
	  str = scm_string_append_shared (scm_list_2 (str, addy));

	  c_str_len = 0;
	}

      if (c == '\\')
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto str_eof;
	  case '"':
	  case '\\':
	    break;
#if SCM_ENABLE_ELISP
	  case '(':
	  case ')':
	    if (SCM_ESCAPED_PARENS_P)
	      break;
	    goto bad_escaped;
#endif
	  case '\n':
	    continue;
	  case '0':
	    c = '\0';
	    break;
	  case 'f':
	    c = '\f';
	    break;
	  case 'n':
	    c = '\n';
	    break;
	  case 'r':
	    c = '\r';
	    break;
	  case 't':
	    c = '\t';
	    break;
	  case 'a':
	    c = '\007';
	    break;
	  case 'v':
	    c = '\v';
	    break;
	  case 'x':
	    {
	      int a, b;
	      a = scm_getc (port);
	      if (a == EOF) goto str_eof;
	      b = scm_getc (port);
	      if (b == EOF) goto str_eof;
	      if      ('0' <= a && a <= '9') a -= '0';
	      else if ('A' <= a && a <= 'F') a = a - 'A' + 10;
	      else if ('a' <= a && a <= 'f') a = a - 'a' + 10;
	      else goto bad_escaped;
	      if      ('0' <= b && b <= '9') b -= '0';
	      else if ('A' <= b && b <= 'F') b = b - 'A' + 10;
	      else if ('a' <= b && b <= 'f') b = b - 'a' + 10;
	      else goto bad_escaped;
	      c = a * 16 + b;
	      break;
	    }
	  default:
	  bad_escaped:
	    scm_i_input_error (FUNC_NAME, port,
			       "illegal character in escape sequence: ~S",
			       scm_list_1 (SCM_MAKE_CHAR (c)));
	  }
      c_str[c_str_len++] = c;
    }

  if (c_str_len > 0)
    {
      SCM addy;

      addy = scm_from_locale_stringn (c_str, c_str_len);
      if (str == SCM_BOOL_F)
	str = addy;
      else
	str = scm_string_append_shared (scm_list_2 (str, addy));
    }
  else
    str = (str == SCM_BOOL_F) ? scm_nullstr : str;

  return str;
}
#undef FUNC_NAME


static SCM
scm_read_number (int chr, SCM port)
{
  SCM result, str = SCM_EOL;
  char buffer[READER_BUFFER_SIZE];
  size_t read;
  int overflow = 0;

  scm_ungetc (chr, port);
  do
    {
      overflow = read_token (port, buffer, sizeof (buffer), &read);

      if ((overflow) || (scm_is_pair (str)))
	str = scm_cons (scm_from_locale_stringn (buffer, read), str);
    }
  while (overflow);

  if (scm_is_pair (str))
    {
      /* The slow path.  */

      str = scm_string_concatenate (scm_reverse_x (str, SCM_EOL));
      result = scm_string_to_number (str, SCM_UNDEFINED);
      if (!scm_is_true (result))
	/* Return a symbol instead of a number.  */
	result = scm_string_to_symbol (str);
    }
  else
    {
      result = scm_c_locale_stringn_to_number (buffer, read, 10);
      if (!scm_is_true (result))
	/* Return a symbol instead of a number.  */
	result = scm_from_locale_symboln (buffer, read);
    }

  return result;
}

static SCM
scm_read_mixed_case_symbol (int chr, SCM port)
{
  SCM result, str = SCM_EOL;
  int overflow = 0, ends_with_colon = 0;
  char buffer[READER_BUFFER_SIZE];
  size_t read = 0;
  int postfix = scm_is_eq (SCM_PACK (SCM_KEYWORD_STYLE), scm_keyword_postfix);

  scm_ungetc (chr, port);
  do
    {
      overflow = read_token (port, buffer, sizeof (buffer), &read);

      if (read > 0)
	ends_with_colon = (buffer[read - 1] == ':');

      if ((overflow) || (scm_is_pair (str)))
	str = scm_cons (scm_from_locale_stringn (buffer, read), str);
    }
  while (overflow);

  if (scm_is_pair (str))
    {
      size_t len;

      str = scm_string_concatenate (scm_reverse_x (str, SCM_EOL));
      len = scm_c_string_length (str);

      /* Per SRFI-88, `:' alone is an identifier, not a keyword.  */
      if (postfix && ends_with_colon && (len > 1))
	{
	  /* Strip off colon.  */
	  str = scm_c_substring (str, 0, len-1);
	  result = scm_string_to_symbol (str);
	  result = scm_symbol_to_keyword (result);
	}
      else
	result = scm_string_to_symbol (str);
    }
  else
    {
      /* For symbols smaller than `sizeof (buffer)', we don't need to recur
	 to Scheme strings.  Therefore, we only create one Scheme object (a
	 symbol) per symbol read.  */
      if (postfix && ends_with_colon && (read > 1))
	result = scm_from_locale_keywordn (buffer, read - 1);
      else
	result = scm_from_locale_symboln (buffer, read);
    }

  return result;
}

static SCM
scm_read_number_and_radix (int chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  SCM result, str = SCM_EOL;
  size_t read;
  char buffer[READER_BUFFER_SIZE];
  unsigned int radix;
  int overflow = 0;

  switch (chr)
    {
    case 'B':
    case 'b':
      radix = 2;
      break;

    case 'o':
    case 'O':
      radix = 8;
      break;

    case 'd':
    case 'D':
      radix = 10;
      break;

    case 'x':
    case 'X':
      radix = 16;
      break;

    default:
      scm_ungetc (chr, port);
      scm_ungetc ('#', port);
      radix = 10;
    }

  do
    {
      overflow = read_token (port, buffer, sizeof (buffer), &read);

      if ((overflow) || (scm_is_pair (str)))
	str = scm_cons (scm_from_locale_stringn (buffer, read), str);
    }
  while (overflow);

  if (scm_is_pair (str))
    {
      str = scm_string_concatenate (scm_reverse_x (str, SCM_EOL));
      result = scm_string_to_number (str, scm_from_uint (radix));
    }
  else
    result = scm_c_locale_stringn_to_number (buffer, read, radix);

  if (scm_is_true (result))
    return result;

  scm_i_input_error (FUNC_NAME, port, "unknown # object", SCM_EOL);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

static SCM
scm_read_quote (int chr, SCM port)
{
  SCM p;
  long line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  switch (chr)
    {
    case '`':
      p = scm_sym_quasiquote;
      break;

    case '\'':
      p = scm_sym_quote;
      break;

    case ',':
      {
	int c;

	c = scm_getc (port);
	if ('@' == c)
	  p = scm_sym_uq_splicing;
	else
	  {
	    scm_ungetc (c, port);
	    p = scm_sym_unquote;
	  }
	break;
      }

    default:
      fprintf (stderr, "%s: unhandled quote character (%i)\n",
	       "scm_read_quote", chr);
      abort ();
    }

  p = scm_cons2 (p, scm_read_expression (port), SCM_EOL);
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash, p,
		      scm_make_srcprops (line, column,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? (scm_cons2 (SCM_CAR (p),
						       SCM_CAR (SCM_CDR (p)),
						       SCM_EOL))
					 : SCM_UNDEFINED,
					 SCM_EOL));


  return p;
}

static inline SCM
scm_read_semicolon_comment (int chr, SCM port)
{
  int c;

  for (c = scm_getc (port);
       (c != EOF) && (c != '\n');
       c = scm_getc (port));

  return SCM_UNSPECIFIED;
}


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

static SCM
scm_read_boolean (int chr, SCM port)
{
  switch (chr)
    {
    case 't':
    case 'T':
      return SCM_BOOL_T;

    case 'f':
    case 'F':
      return SCM_BOOL_F;
    }

  return SCM_UNSPECIFIED;
}

static SCM
scm_read_character (int chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  unsigned c;
  char charname[READER_CHAR_NAME_MAX_SIZE];
  size_t charname_len;

  if (read_token (port, charname, sizeof (charname), &charname_len))
    goto char_error;

  if (charname_len == 0)
    {
      chr = scm_getc (port);
      if (chr == EOF)
	scm_i_input_error (FUNC_NAME, port, "unexpected end of file "
			   "while reading character", SCM_EOL);

      /* CHR must be a token delimiter, like a whitespace.  */
      return (SCM_MAKE_CHAR (chr));
    }

  if (charname_len == 1)
    return SCM_MAKE_CHAR (charname[0]);

  if (*charname >= '0' && *charname < '8')
    {
      /* Dirk:FIXME::  This type of character syntax is not R5RS
       * compliant.  Further, it should be verified that the constant
       * does only consist of octal digits.  Finally, it should be
       * checked whether the resulting fixnum is in the range of
       * characters.  */
      SCM p = scm_c_locale_stringn_to_number (charname, charname_len, 8);
      if (SCM_I_INUMP (p))
	return SCM_MAKE_CHAR (SCM_I_INUM (p));
    }

  for (c = 0; c < scm_n_charnames; c++)
    if (scm_charnames[c]
	&& (!strncasecmp (scm_charnames[c], charname, charname_len)))
      return SCM_MAKE_CHAR (scm_charnums[c]);

 char_error:
  scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
		     scm_list_1 (scm_from_locale_stringn (charname,
							  charname_len)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static inline SCM
scm_read_keyword (int chr, SCM port)
{
  SCM symbol;

  /* Read the symbol that comprises the keyword.  Doing this instead of
     invoking a specific symbol reader function allows `scm_read_keyword ()'
     to adapt to the delimiters currently valid of symbols.

     XXX: This implementation allows sloppy syntaxes like `#:  key'.  */
  symbol = scm_read_expression (port);
  if (!scm_is_symbol (symbol))
    scm_i_input_error ("scm_read_keyword", port,
		       "keyword prefix `~a' not followed by a symbol: ~s",
		       scm_list_2 (SCM_MAKE_CHAR (chr), symbol));

  return (scm_symbol_to_keyword (symbol));
}

static inline SCM
scm_read_vector (int chr, SCM port)
{
  /* Note: We call `scm_read_sexp ()' rather than READER here in order to
     guarantee that it's going to do what we want.  After all, this is an
     implementation detail of `scm_read_vector ()', not a desirable
     property.  */
  return (scm_vector (scm_read_sexp (chr, port)));
}

static inline SCM
scm_read_srfi4_vector (int chr, SCM port)
{
  return scm_i_read_array (port, chr);
}

static SCM
scm_read_guile_bit_vector (int chr, SCM port)
{
  /* Read the `#*10101'-style read syntax for bit vectors in Guile.  This is
     terribly inefficient but who cares?  */
  SCM s_bits = SCM_EOL;

  for (chr = scm_getc (port);
       (chr != EOF) && ((chr == '0') || (chr == '1'));
       chr = scm_getc (port))
    {
      s_bits = scm_cons ((chr == '0') ? SCM_BOOL_F : SCM_BOOL_T, s_bits);
    }

  if (chr != EOF)
    scm_ungetc (chr, port);

  return scm_bitvector (scm_reverse_x (s_bits, SCM_EOL));
}

static inline SCM
scm_read_scsh_block_comment (int chr, SCM port)
{
  int bang_seen = 0;

  for (;;)
    {
      int c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error ("skip_block_comment", port,
			   "unterminated `#! ... !#' comment", SCM_EOL);

      if (c == '!')
	bang_seen = 1;
      else if (c == '#' && bang_seen)
	break;
      else
	bang_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

static SCM
scm_read_extended_symbol (int chr, SCM port)
{
  /* Guile's extended symbol read syntax looks like this:

       #{This is all a symbol name}#

     So here, CHR is expected to be `{'.  */
  SCM result;
  int saw_brace = 0, finished = 0;
  size_t len = 0;
  char buf[1024];

  result = scm_c_make_string (0, SCM_MAKE_CHAR ('X'));

  while ((chr = scm_getc (port)) != EOF)
    {
      if (saw_brace)
	{
	  if (chr == '#')
	    {
	      finished = 1;
	      break;
	    }
	  else
	    {
	      saw_brace = 0;
	      buf[len++] = '}';
	      buf[len++] = chr;
	    }
	}
      else if (chr == '}')
	saw_brace = 1;
      else
	buf[len++] = chr;

      if (len >= sizeof (buf) - 2)
	{
	  scm_string_append (scm_list_2 (result,
					 scm_from_locale_stringn (buf, len)));
	  len = 0;
	}

      if (finished)
	break;
    }

  if (len)
    result = scm_string_append (scm_list_2
				(result,
				 scm_from_locale_stringn (buf, len)));

  return (scm_string_to_symbol (result));
}



/* Top-level token readers, i.e., dispatchers.  */

static SCM
scm_read_sharp_extension (int chr, SCM port)
{
  SCM proc;

  proc = scm_get_hash_procedure (chr);
  if (scm_is_true (scm_procedure_p (proc)))
    {
      long line = SCM_LINUM (port);
      int column = SCM_COL (port) - 2;
      SCM got;

      got = scm_call_2 (proc, SCM_MAKE_CHAR (chr), port);
      if (!scm_is_eq (got, SCM_UNSPECIFIED))
	{
	  if (SCM_RECORD_POSITIONS_P)
	    return (recsexpr (got, line, column,
			      SCM_FILENAME (port)));
	  else
	    return got;
	}
    }

  return SCM_UNSPECIFIED;
}

/* The reader for the sharp `#' character.  It basically dispatches reads
   among the above token readers.   */
static SCM
scm_read_sharp (int chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  SCM result;

  chr = scm_getc (port);

  result = scm_read_sharp_extension (chr, port);
  if (!scm_is_eq (result, SCM_UNSPECIFIED))
    return result;

  switch (chr)
    {
    case '\\':
      return (scm_read_character (chr, port));
    case '(':
      return (scm_read_vector (chr, port));
    case 's':
    case 'u':
    case 'f':
      /* This one may return either a boolean or an SRFI-4 vector.  */
      return (scm_read_srfi4_vector (chr, port));
    case '*':
      return (scm_read_guile_bit_vector (chr, port));
    case 't':
    case 'T':
    case 'F':
      /* This one may return either a boolean or an SRFI-4 vector.  */
      return (scm_read_boolean (chr, port));
    case ':':
      return (scm_read_keyword (chr, port));
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '@':
#if SCM_ENABLE_DEPRECATED
      /* See below for 'i' and 'e'. */
    case 'a':
    case 'c':
    case 'y':
    case 'h':
    case 'l':
#endif
      return (scm_i_read_array (port, chr));

    case 'i':
    case 'e':
#if SCM_ENABLE_DEPRECATED
      {
	/* When next char is '(', it really is an old-style
	   uniform array. */
	int next_c = scm_getc (port);
	if (next_c != EOF)
	  scm_ungetc (next_c, port);
	if (next_c == '(')
	  return scm_i_read_array (port, chr);
	/* Fall through. */
      }
#endif
    case 'b':
    case 'B':
    case 'o':
    case 'O':
    case 'd':
    case 'D':
    case 'x':
    case 'X':
    case 'I':
    case 'E':
      return (scm_read_number_and_radix (chr, port));
    case '{':
      return (scm_read_extended_symbol (chr, port));
    case '!':
      return (scm_read_scsh_block_comment (chr, port));
    default:
      result = scm_read_sharp_extension (chr, port);
      if (scm_is_eq (result, SCM_UNSPECIFIED))
	scm_i_input_error (FUNC_NAME, port, "Unknown # object: ~S",
			   scm_list_1 (SCM_MAKE_CHAR (chr)));
      else
	return result;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
scm_read_expression (SCM port)
#define FUNC_NAME "scm_read_expression"
{
  while (1)
    {
      register int chr;

      chr = scm_getc (port);

      switch (chr)
	{
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	  break;
	case ';':
	  (void) scm_read_semicolon_comment (chr, port);
	  break;
	case '(':
	  return (scm_read_sexp (chr, port));
	case '"':
	  return (scm_read_string (chr, port));
	case '\'':
	case '`':
	case ',':
	  return (scm_read_quote (chr, port));
	case '#':
	  {
	    SCM result;
	    result = scm_read_sharp (chr, port);
	    if (scm_is_eq (result, SCM_UNSPECIFIED))
	      /* We read a comment or some such.  */
	      break;
	    else
	      return result;
	  }
	case ')':
	  scm_i_input_error (FUNC_NAME, port, "unexpected \")\"", SCM_EOL);
	  break;
	case EOF:
	  return SCM_EOF_VAL;
	case ':':
	  if (scm_is_eq (SCM_PACK (SCM_KEYWORD_STYLE), scm_keyword_prefix))
	    return scm_symbol_to_keyword (scm_read_expression (port));
	  /* Fall through.  */

	default:
	  {
	    if (((chr >= '0') && (chr <= '9'))
		|| (strchr ("+-.", chr)))
	      return (scm_read_number (chr, port));
	    else
	      return (scm_read_mixed_case_symbol (chr, port));
	  }
	}
    }
}
#undef FUNC_NAME


/* Actual reader.  */

SCM_DEFINE (scm_read, "read", 0, 1, 0, 
            (SCM port),
	    "Read an s-expression from the input port @var{port}, or from\n"
	    "the current input port if @var{port} is not specified.\n"
	    "Any whitespace before the next token is discarded.")
#define FUNC_NAME s_scm_read
{
  int c;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  c = flush_ws (port, (char *) NULL);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);

  return (scm_read_expression (port));
}
#undef FUNC_NAME




/* Used when recording expressions constructed by `scm_read_sharp ()'.  */
static SCM
recsexpr (SCM obj, long line, int column, SCM filename)
{
  if (!scm_is_pair(obj)) {
    return obj;
  } else {
    SCM tmp = obj, copy;
    /* If this sexpr is visible in the read:sharp source, we want to
       keep that information, so only record non-constant cons cells
       which haven't previously been read by the reader. */
    if (scm_is_false (scm_whash_lookup (scm_source_whash, obj)))
      {
	if (SCM_COPY_SOURCE_P)
	  {
	    copy = scm_cons (recsexpr (SCM_CAR (obj), line, column, filename),
			     SCM_UNDEFINED);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      {
		SCM_SETCDR (copy, scm_cons (recsexpr (SCM_CAR (tmp),
						      line,
						      column,
						      filename),
					    SCM_UNDEFINED));
		copy = SCM_CDR (copy);
	      }
	    SCM_SETCDR (copy, tmp);
	  }
	else
	  {
	    recsexpr (SCM_CAR (obj), line, column, filename);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      recsexpr (SCM_CAR (tmp), line, column, filename);
	    copy = SCM_UNDEFINED;
	  }
	scm_whash_insert (scm_source_whash,
			  obj,
			  scm_make_srcprops (line,
					     column,
					     filename,
					     copy,
					     SCM_EOL));
      }
    return obj;
  }
}

/* Manipulate the read-hash-procedures alist.  This could be written in
   Scheme, but maybe it will also be used by C code during initialisation.  */
SCM_DEFINE (scm_read_hash_extend, "read-hash-extend", 2, 0, 0,
            (SCM chr, SCM proc),
	    "Install the procedure @var{proc} for reading expressions\n"
	    "starting with the character sequence @code{#} and @var{chr}.\n"
	    "@var{proc} will be called with two arguments:  the character\n"
	    "@var{chr} and the port to read further data from. The object\n"
	    "returned will be the return value of @code{read}.")
#define FUNC_NAME s_scm_read_hash_extend
{
  SCM this;
  SCM prev;

  SCM_VALIDATE_CHAR (1, chr);
  SCM_ASSERT (scm_is_false (proc)
	      || scm_is_eq (scm_procedure_p (proc), SCM_BOOL_T),
	      proc, SCM_ARG2, FUNC_NAME);

  /* Check if chr is already in the alist.  */
  this = *scm_read_hash_procedures;
  prev = SCM_BOOL_F;
  while (1)
    {
      if (scm_is_null (this))
	{
	  /* not found, so add it to the beginning.  */
	  if (scm_is_true (proc))
	    {
	      *scm_read_hash_procedures = 
		scm_cons (scm_cons (chr, proc), *scm_read_hash_procedures);
	    }
	  break;
	}
      if (scm_is_eq (chr, SCM_CAAR (this)))
	{
	  /* already in the alist.  */
	  if (scm_is_false (proc))
	    {
	      /* remove it.  */
	      if (scm_is_false (prev))
		{
		  *scm_read_hash_procedures =
		    SCM_CDR (*scm_read_hash_procedures);
		}
	      else
		scm_set_cdr_x (prev, SCM_CDR (this));
	    }
	  else
	    {
	      /* replace it.  */
	      scm_set_cdr_x (SCM_CAR (this), proc);
	    }
	  break;
	}
      prev = this;
      this = SCM_CDR (this);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Recover the read-hash procedure corresponding to char c.  */
static SCM
scm_get_hash_procedure (int c)
{
  SCM rest = *scm_read_hash_procedures;

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;
  
      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);
     
      rest = SCM_CDR (rest);
    }
}

void
scm_init_read ()
{
  scm_read_hash_procedures =
    SCM_VARIABLE_LOC (scm_c_define ("read-hash-procedures", SCM_EOL));

  scm_init_opts (scm_read_options, scm_read_opts, SCM_N_READ_OPTIONS);
#include "libguile/read.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
