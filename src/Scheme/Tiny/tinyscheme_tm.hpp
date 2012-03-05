
/******************************************************************************
 * MODULE     : tinyscheme_tm.hpp
 * DESCRIPTION: TinyScheme interface
 * COPYRIGHT  : (C) 2011 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/


#ifndef TINYSCHEME_TM_HPP
#define TINYSCHEME_TM_HPP


//#include "scheme.h" // TinyScheme

#define TM_OP_APPLY OP_APPLY
#undef OP_APPLY
#include "scheme-private.h" // TinyScheme
#undef cons
//#define OP_APPLY TM_OP_APPLY

#include "string.hpp"
#include "array.hpp"
#include "blackbox.hpp"


typedef cell_ptr scm;



//inline bool is_blackbox(scm p)	{ return is_blackbox(p); }
//inline blackbox blackboxvalue(scm p) { return static_cast<cell_tm*>(p)->blackboxvalue(); }
//inline void set_blackbox(scm p, blackbox b) { return static_cast<cell_tm*>(p)->set_blackbox(b); }



/**** Interfacing to TeXmacs *****/


extern scheme *the_scheme;


inline scm scm_null () { return the_scheme->NIL; }
inline scm scm_true () { return the_scheme->T; }
inline scm scm_false () { return the_scheme->F; }

inline bool scm_is_equal(scm o1, scm o2) { return (o1 == o2); }

inline bool scm_is_null (scm obj) { return (obj == scm_null()); }
inline bool scm_is_pair (scm obj) { return is_pair (obj); }
inline bool scm_is_list (scm obj) { return scm_is_pair(obj) || scm_is_null(obj); }
inline bool scm_is_bool (scm obj) { return ((obj == scm_true ()) || (obj == scm_false ())); }
inline bool scm_is_int (scm obj) { return is_integer (obj); }
inline bool scm_is_double (scm obj) { return is_real (obj); }
inline bool scm_is_string (scm obj) { return is_string (obj); }
inline bool scm_is_symbol (scm obj) { return is_symbol (obj); }
inline bool scm_is_blackbox (scm obj) { return is_blackbox (obj); }

inline scm scm_cons (scm obj1, scm obj2) { return _cons (the_scheme,obj1,obj2,0); }
inline scm scm_car (scm obj) { return pair_car (obj); }
inline scm scm_cdr (scm obj) { return pair_cdr (obj); }
inline scm scm_caar (scm obj) { return scm_car (scm_car (obj)); }
inline scm scm_cadr (scm obj) { return scm_car (scm_cdr (obj)); }
inline scm scm_cdar (scm obj) { return scm_cdr (scm_car (obj)); }
inline scm scm_cddr (scm obj) { return scm_cdr (scm_cdr (obj)); }
inline scm scm_caddr (scm obj) { return scm_cadr (scm_cdr (obj)); }
inline scm scm_cadddr (scm obj) { return scm_caddr (scm_cdr (obj)); }

inline void scm_set_car (scm obj, scm obj2) { set_car (obj, obj2); }
inline void scm_set_cdr (scm obj, scm obj2) { set_cdr (obj, obj2); }



inline scm bool_to_scm (bool b) { return b ? scm_true () : scm_false (); }
inline scm int_to_scm (int i) {   return mk_integer (the_scheme,i); }
inline scm double_to_scm (double i) { return mk_real (the_scheme,i); }
inline scm blackbox_to_scm (blackbox b) { return mk_blackbox (the_scheme,(void*)(tm_new<blackbox>(b))); } // CHECK
scm string_to_scm (string s);
scm symbol_to_scm (string s);

inline bool scm_to_bool (scm obj) { return (obj != scm_false()); } 
inline int scm_to_int (scm obj) { return ivalue (obj); }
inline double scm_to_double (scm obj) { return rvalue (obj); }
inline string scm_to_string (scm obj) { return string ( string_value (obj),  string_length (obj)); }
inline string scm_to_symbol (scm obj) { return string (symname (obj),  symlen (obj)); }
inline blackbox scm_to_blackbox (scm obj) { return *(blackbox*)blackboxvalue(obj); }


scm eval_scheme_file (string name);
scm eval_scheme (string s);
scm call_scheme (scm fun);
scm call_scheme (scm fun, scm a1);
scm call_scheme (scm fun, scm a1, scm a2);
scm call_scheme (scm fun, scm a1, scm a2, scm a3);
scm call_scheme (scm fun, scm a1, scm a2, scm a3, scm a4);
scm call_scheme (scm fun, array<scm> a);



/******************************************************************************
 * Gluing
 ******************************************************************************/


template<scm (*PROC)()> 
static scm proc (scheme*, scm ) { 
	scm res = PROC();
	return (res);
}
template<scm (*PROC)(scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1);
	return (res);
}
template<scm (*PROC)(scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2);
	return (res);
}
template<scm (*PROC)(scm, scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm a3 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2,a3);
	return (res);
}
template<scm (*PROC)(scm, scm, scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm a3 = (scm_car(args)); args = scm_cdr (args);
	scm a4 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2,a3,a4);
	return (res);
}
template<scm (*PROC)(scm, scm, scm, scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm a3 = (scm_car(args)); args = scm_cdr (args);
	scm a4 = (scm_car(args)); args = scm_cdr (args);
	scm a5 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2,a3,a4,a5);
	return (res);
}

template<scm (*PROC)(scm, scm, scm, scm, scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm a3 = (scm_car(args)); args = scm_cdr (args);
	scm a4 = (scm_car(args)); args = scm_cdr (args);
	scm a5 = (scm_car(args)); args = scm_cdr (args);
	scm a6 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2,a3,a4,a5,a6);
	return (res);
}

template<scm (*PROC)(scm, scm, scm, scm, scm, scm, scm, scm)> 
static scm proc (scheme*, scm args) { 
	scm a1 = (scm_car(args)); args = scm_cdr (args);
	scm a2 = (scm_car(args)); args = scm_cdr (args);
	scm a3 = (scm_car(args)); args = scm_cdr (args);
	scm a4 = (scm_car(args)); args = scm_cdr (args);
	scm a5 = (scm_car(args)); args = scm_cdr (args);
	scm a6 = (scm_car(args)); args = scm_cdr (args);
	scm a7 = (scm_car(args)); args = scm_cdr (args);
	scm a8 = (scm_car(args)); args = scm_cdr (args);
	scm res = PROC(a1,a2,a3,a4,a5,a6,a7,a8);
	return (res);
}


typedef foreign_func scm_foreign_func;
string scheme_dialect ();
void scm_define_glue(const char *name, scm_foreign_func f);


#define scm_install_procedure(name, func, args, p0, p1) scm_define_glue( name, proc<func>)

#define SCM_ARG1
#define SCM_ARG2
#define SCM_ARG3
#define SCM_ARG4
#define SCM_ARG5
#define SCM_ARG6
#define SCM_ARG7
#define SCM_ARG8

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr)

#endif // TINYSCHEME_TM_HPP
