/*	Copyright (C) 1995,1996,1998, 2000, 2001, 2004, 2006, 2008 Free Software Foundation, Inc.
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

#include <ctype.h>
#include <limits.h>
#include "libguile/_scm.h"
#include "libguile/validate.h"

#include "libguile/chars.h"
#include "libguile/srfi-14.h"



SCM_DEFINE (scm_char_p, "char?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a character, else @code{#f}.")
#define FUNC_NAME s_scm_char_p
{
  return scm_from_bool (SCM_CHARP(x));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_eq_p, "char=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is the same character as @var{y}, else @code{#f}.")
#define FUNC_NAME s_scm_char_eq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_is_eq (x, y));
}
#undef FUNC_NAME


SCM_DEFINE1 (scm_char_less_p, "char<?", scm_tc7_rpsubr, 
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than @var{y} in the ASCII sequence,\n"
	     "else @code{#f}.")
#define FUNC_NAME s_scm_char_less_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) < SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_leq_p, "char<=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than or equal to @var{y} in the\n"
	     "ASCII sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_leq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) <= SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_gr_p, "char>?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than @var{y} in the ASCII\n"
	     "sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_gr_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) > SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_geq_p, "char>=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than or equal to @var{y} in the\n"
	     "ASCII sequence, else @code{#f}.")
#define FUNC_NAME s_scm_char_geq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (SCM_CHAR(x) >= SCM_CHAR(y));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_eq_p, "char-ci=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is the same character as @var{y} ignoring\n"
	     "case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_eq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x))==scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_less_p, "char-ci<?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than @var{y} in the ASCII sequence\n"
	     "ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_less_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool ((scm_c_upcase(SCM_CHAR(x))) < scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_leq_p, "char-ci<=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is less than or equal to @var{y} in the\n"
	     "ASCII sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_leq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) <= scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_gr_p, "char-ci>?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than @var{y} in the ASCII\n"
	     "sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_gr_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) > scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME

SCM_DEFINE1 (scm_char_ci_geq_p, "char-ci>=?", scm_tc7_rpsubr,
             (SCM x, SCM y),
	     "Return @code{#t} iff @var{x} is greater than or equal to @var{y} in the\n"
	     "ASCII sequence ignoring case, else @code{#f}.")
#define FUNC_NAME s_scm_char_ci_geq_p
{
  SCM_VALIDATE_CHAR (1, x);
  SCM_VALIDATE_CHAR (2, y);
  return scm_from_bool (scm_c_upcase(SCM_CHAR(x)) >= scm_c_upcase(SCM_CHAR(y)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_alphabetic_p, "char-alphabetic?", 1, 0, 0,
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is alphabetic, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_alphabetic_p
{
  return scm_char_set_contains_p (scm_char_set_letter, chr);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_numeric_p, "char-numeric?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is numeric, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_numeric_p
{
  return scm_char_set_contains_p (scm_char_set_digit, chr);
}
#undef FUNC_NAME

SCM_DEFINE (scm_char_whitespace_p, "char-whitespace?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is whitespace, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_whitespace_p
{
  return scm_char_set_contains_p (scm_char_set_whitespace, chr);
}
#undef FUNC_NAME



SCM_DEFINE (scm_char_upper_case_p, "char-upper-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is uppercase, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_upper_case_p
{
  return scm_char_set_contains_p (scm_char_set_upper_case, chr);
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_lower_case_p, "char-lower-case?", 1, 0, 0, 
           (SCM chr),
	    "Return @code{#t} iff @var{chr} is lowercase, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_lower_case_p
{
  return scm_char_set_contains_p (scm_char_set_lower_case, chr);
}
#undef FUNC_NAME



SCM_DEFINE (scm_char_is_both_p, "char-is-both?", 1, 0, 0, 
            (SCM chr),
	    "Return @code{#t} iff @var{chr} is either uppercase or lowercase, else @code{#f}.\n")
#define FUNC_NAME s_scm_char_is_both_p
{
  if (scm_is_true (scm_char_set_contains_p (scm_char_set_lower_case, chr)))
    return SCM_BOOL_T;
  return scm_char_set_contains_p (scm_char_set_upper_case, chr);
}
#undef FUNC_NAME




SCM_DEFINE (scm_char_to_integer, "char->integer", 1, 0, 0, 
            (SCM chr),
	    "Return the number corresponding to ordinal position of @var{chr} in the\n"
	    "ASCII sequence.")
#define FUNC_NAME s_scm_char_to_integer
{
  SCM_VALIDATE_CHAR (1, chr);
  return scm_from_ulong (SCM_CHAR(chr));
}
#undef FUNC_NAME



SCM_DEFINE (scm_integer_to_char, "integer->char", 1, 0, 0, 
           (SCM n),
	    "Return the character at position @var{n} in the ASCII sequence.")
#define FUNC_NAME s_scm_integer_to_char
{
  return SCM_MAKE_CHAR (scm_to_uchar (n));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_upcase, "char-upcase", 1, 0, 0, 
           (SCM chr),
	    "Return the uppercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_upcase
{
  SCM_VALIDATE_CHAR (1, chr);
  return SCM_MAKE_CHAR (toupper (SCM_CHAR (chr)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_char_downcase, "char-downcase", 1, 0, 0, 
           (SCM chr),
	    "Return the lowercase character version of @var{chr}.")
#define FUNC_NAME s_scm_char_downcase
{
  SCM_VALIDATE_CHAR (1, chr);
  return SCM_MAKE_CHAR (tolower (SCM_CHAR(chr)));
}
#undef FUNC_NAME





/*
TODO: change name  to scm_i_.. ? --hwn
*/


int
scm_c_upcase (unsigned int c)
{
  if (c <= UCHAR_MAX)
    return toupper (c);
  else
    return c;
}


int
scm_c_downcase (unsigned int c)
{
  if (c <= UCHAR_MAX)
    return tolower (c);
  else
    return c;
}


#ifdef _DCC
# define ASCII
#else
# if (('\n'=='\025') && (' '=='\100') && ('a'=='\201') && ('A'=='\301'))
#  define EBCDIC
# endif /*  (('\n'=='\025') && (' '=='\100') && ('a'=='\201') && ('A'=='\301')) */
# if (('\n'=='\012') && (' '=='\040') && ('a'=='\141') && ('A'=='\101'))
#  define ASCII
# endif /*  (('\n'=='\012') && (' '=='\040') && ('a'=='\141') && ('A'=='\101')) */
#endif /* def _DCC */


#ifdef EBCDIC
char *const scm_charnames[] =
{
  "nul", "soh", "stx", "etx", "pf", "ht", "lc", "del",
   0   , 0   , "smm", "vt", "ff", "cr", "so", "si",
  "dle", "dc1", "dc2", "dc3", "res", "nl", "bs", "il",
  "can", "em", "cc", 0   , "ifs", "igs", "irs", "ius",
   "ds", "sos", "fs", 0   , "byp", "lf", "eob", "pre",
   0   , 0   , "sm", 0   , 0   , "enq", "ack", "bel",
   0   , 0   , "syn", 0   , "pn", "rs", "uc", "eot",
   0   , 0   , 0   , 0   , "dc4", "nak", 0   , "sub",
   "space", scm_s_newline, "tab", "backspace", "return", "page", "null"};

const char scm_charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
\040\041\042\043\044\045\046\047\
\050\051\052\053\054\055\056\057\
\060\061\062\063\064\065\066\067\
\070\071\072\073\074\075\076\077\
 \n\t\b\r\f\0";
#endif /* def EBCDIC */
#ifdef ASCII
char *const scm_charnames[] =
{
  "nul","soh","stx","etx","eot","enq","ack","bel",
   "bs", "ht", "newline", "vt", "np", "cr", "so", "si",
  "dle","dc1","dc2","dc3","dc4","nak","syn","etb",
  "can", "em","sub","esc", "fs", "gs", "rs", "us",
  "space", "sp", "nl", "tab", "backspace", "return", "page", "null", "del"};
const char scm_charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
  \n\t\b\r\f\0\177";
#endif /* def ASCII */

int scm_n_charnames = sizeof (scm_charnames) / sizeof (char *);





void
scm_init_chars ()
{
#include "libguile/chars.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
