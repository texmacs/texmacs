
/******************************************************************************
 * MODULE     : object.hpp
 * DESCRIPTION: Implementation of scheme objects
 * COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef OBJECT_H
#define OBJECT_H


#include "scheme.hpp" // interface to texmacs
//#include "../Tiny/tinyscheme_tm.hpp" // interface to TinyScheme
//#include "../Little/scheme_tm.hpp" // interface to LittleScheme
#include "../Guile/guile_tm.hpp" // interface to guile



class scm_object_rep: public object_rep {
	scm handle;

	scm_object_rep (scm obj);
	~scm_object_rep ();

	friend class object;
  friend scm object_to_scm (object o);
  friend object scm_to_object (scm obj);
  template<typename C, typename A1> friend C* tm_new (const A1& a1); 
};

inline scm object_to_scm (object o) {
  scm_object_rep *oo = static_cast<scm_object_rep*>(o.operator->());
  return scm_caar (oo->handle);
  //return scm_caar ((scm)o->lookup ()); 
}
inline object scm_to_object (scm obj) { return tm_new<scm_object_rep> (obj); }


#endif // defined OBJECT_H
