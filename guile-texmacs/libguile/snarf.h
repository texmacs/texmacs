/* classes: h_files */

#ifndef SCM_SNARF_H
#define SCM_SNARF_H

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
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



/* Macros for snarfing initialization actions from C source. */

#if defined(__cplusplus) || defined(GUILE_CPLUSPLUS_SNARF)

/* This used to be "SCM (*)(...)" but GCC on RedHat 7.1 doesn't seem
   to like it.
 */
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)()

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
 * up through the first occurrence of SCM_SNARF_DOC_START on that
 * line, if any.
 */

#ifdef SCM_MAGIC_SNARF_INITS
# define SCM_SNARF_HERE(X)
# define SCM_SNARF_INIT(X) ^^ X ^:^
# define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
#else
# ifdef SCM_MAGIC_SNARF_DOCS
#  define SCM_SNARF_HERE(X)
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING) \
^^ { \
cname CNAME ^^ \
fname FNAME ^^ \
type TYPE ^^ \
location __FILE__ __LINE__ ^^ \
arglist ARGLIST ^^ \
argsig REQ OPT VAR ^^ \
DOCSTRING ^^ }
# else
#  define SCM_SNARF_HERE(X) X
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
# endif
#endif

#define SCM_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_PRIMITIVE_GENERIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
static SCM g_ ## FNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
g_ ## FNAME = SCM_PACK (0); \
scm_c_define_gsubr_with_generic (s_ ## FNAME, REQ, OPT, VAR, \
                    		 (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME, \
				 &g_ ## FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_DEFINE_PUBLIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
scm_c_export (s_ ## FNAME, NULL); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_DEFINE1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(scm_c_define_subr (s_ ## FNAME, TYPE, FNAME); ) \
SCM_SNARF_DOCS(1, FNAME, PRIMNAME, ARGLIST, 2, 0, 0, DOCSTRING)

#define SCM_PRIMITIVE_GENERIC_1(FNAME, PRIMNAME, TYPE, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
static SCM g_ ## FNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
g_ ## FNAME = SCM_PACK (0); \
scm_c_define_subr_with_generic (s_ ## FNAME, TYPE, FNAME, &g_ ## FNAME); \
)\
SCM_SNARF_DOCS(1, FNAME, PRIMNAME, ARGLIST, 2, 0, 0, DOCSTRING)

#define SCM_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN))

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN);) \
SCM_SNARF_DOCS(register, CFN, STR, (), REQ, OPT, VAR, \
               "implemented by the C function \"" #CFN "\"")

#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
SCM_SNARF_HERE(\
static const char RANAME[]=STR;\
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_c_define_gsubr_with_generic (RANAME, REQ, OPT, VAR, \
                                 (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)

#define SCM_PROC1(RANAME, STR, TYPE, CFN) \
SCM_SNARF_HERE(static const char RANAME[]=STR) \
SCM_SNARF_INIT(\
scm_c_define_subr (RANAME, TYPE, (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN) \
)


#define SCM_GPROC1(RANAME, STR, TYPE, CFN, GF) \
SCM_SNARF_HERE(\
static const char RANAME[]=STR; \
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_c_define_subr_with_generic (RANAME, TYPE, \
                                (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)

#define SCM_SYNTAX(RANAME, STR, TYPE, CFN)  \
SCM_SNARF_HERE(static const char RANAME[]=STR)\
SCM_SNARF_INIT(scm_make_synt (RANAME, TYPE, CFN))

#define SCM_SYMBOL(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_from_locale_symbol (scheme_name)))

#define SCM_GLOBAL_SYMBOL(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_from_locale_symbol (scheme_name)))

#define SCM_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_from_locale_keyword (scheme_name)))

#define SCM_GLOBAL_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_from_locale_keyword (scheme_name)))

#define SCM_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_define (scheme_name, SCM_BOOL_F));)

#define SCM_GLOBAL_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_define (scheme_name, SCM_BOOL_F));)

#define SCM_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_define (scheme_name, init_val));)

#define SCM_GLOBAL_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_permanent_object (scm_c_define (scheme_name, init_val));)

#define SCM_MUTEX(c_name) \
SCM_SNARF_HERE(static scm_t_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_mutex_init (&c_name, &scm_i_plugin_mutex))

#define SCM_GLOBAL_MUTEX(c_name) \
SCM_SNARF_HERE(scm_t_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_mutex_init (&c_name, &scm_i_plugin_mutex))

#define SCM_REC_MUTEX(c_name) \
SCM_SNARF_HERE(static scm_t_rec_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_rec_mutex_init (&c_name, &scm_i_plugin_rec_mutex))

#define SCM_GLOBAL_REC_MUTEX(c_name) \
SCM_SNARF_HERE(scm_t_rec_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_rec_mutex_init (&c_name, &scm_i_plugin_rec_mutex))

#define SCM_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(static scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_GLOBAL_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(static SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_GLOBAL_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(static size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_GLOBAL_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(static int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_GLOBAL_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(static SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_GLOBAL_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(static SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)

#define SCM_GLOBAL_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)


#ifdef SCM_MAGIC_SNARF_DOCS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) ^^ argpos _arg _pos __LINE__ ^^
#endif /* SCM_MAGIC_SNARF_DOCS */

#endif  /* SCM_SNARF_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
