/* classes: h_files */

/* Macros for snarfing initialization actions from C source. */

#ifndef LIBGUILE_SNARF_H
#define LIBGUILE_SNARF_H

/* Copyright (C) 1995, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#if defined(__cplusplus) || defined(GUILE_CPLUSPLUS_SNARF)
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)(...)
#else
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)()
#endif

/* Generic macros to be used in user macro definitions.
 *
 * For example, in order to define a macro which creates ints and
 * initializes them to the result of foo (), do:
 *
 *   #define SCM_FOO(NAME) \
 *     SCM_SNARF_HERE (int NAME) \
 *     SCM_SNARF_INIT (NAME = foo ())
 *
 * The SCM_SNARF_INIT text goes into the corresponding .x file
 * up through the first occurrence of SCM__D on that line, if any.
 */

#ifndef SCM_MAGIC_SNARFER
#  define SCM_SNARF_HERE(X) X
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(X)
#else
#  define SCM_SNARF_HERE(X)
#  define SCM_SNARF_INIT(X) SCM__I X
#  define SCM_SNARF_DOCS(X) X
#endif

#define SCM_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_make_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
SCM_SNARF_DOCS(\
SCM__DP PRIMNAME #ARGLIST | REQ | OPT | VAR | __FILE__:__LINE__ | SCM__S DOCSTRING SCM__E \
)

#define SCM_DEFINE1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(scm_make_subr (s_ ## FNAME, TYPE, FNAME); ) \
SCM_SNARF_DOCS(\
SCM__D1 PRIMNAME #ARGLIST | 2 | 0 | 0 | __FILE__:__LINE__ | SCM__S DOCSTRING SCM__E \
)

#define SCM_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_make_gsubr (RANAME, REQ, OPT, VAR, \
                               (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN))

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_make_gsubr (RANAME, REQ, OPT, VAR, \
                               (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN);) \
SCM_SNARF_DOCS(\
SCM__DR STR | REQ | OPT | VAR | __FILE__:__LINE__ | SCM__S CFN SCM__E \
)

#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
SCM_SNARF_HERE(\
static const char RANAME[]=STR;\
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_make_gsubr_with_generic (RANAME, REQ, OPT, VAR, \
                             (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)

#define SCM_PROC1(RANAME, STR, TYPE, CFN) \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(\
scm_make_subr (RANAME, TYPE, (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN) \
)


#define SCM_GPROC1(RANAME, STR, TYPE, CFN, GF) \
SCM_SNARF_HERE(\
static const char RANAME[]=STR; \
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_make_subr_with_generic (RANAME, TYPE, \
                            (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)

#define SCM_SYNTAX(RANAME, STR, TYPE, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR)\
SCM_SNARF_INIT(scm_make_synt (RANAME, TYPE, CFN))

#define SCM_SYMBOL(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (SCM_CAR (scm_intern0 (scheme_name))))

#define SCM_GLOBAL_SYMBOL(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (SCM_CAR (scm_intern0 (scheme_name))))

#define SCM_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_make_keyword (scheme_name)))

#define SCM_GLOBAL_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_make_keyword (scheme_name)))

#define SCM_VCELL(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, SCM_BOOL_F))

#define SCM_GLOBAL_VCELL(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, SCM_BOOL_F))

#define SCM_VCELL_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, init_val))

#define SCM_GLOBAL_VCELL_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, init_val))

#define SCM_CONST_LONG(c_name, scheme_name,value) \
SCM_VCELL_INIT(c_name, scheme_name, scm_long2num(value))

#ifdef SCM_MAGIC_SNARFER
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) *&*&*&*SCM_ARG_BETTER_BE_IN_POSITION(_arg,_pos,__LINE__)
#endif /* SCM_MAGIC_SNARFER */

#endif /* LIBGUILE_SNARF_H */


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
