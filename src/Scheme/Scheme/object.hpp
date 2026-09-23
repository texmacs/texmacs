
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
//#include "../Tiny/tinytmscm_tm.hpp" // interface to TinyScheme
//#include "../Guile/guile_tm.hpp" // interface to guile
#include "../S7/s7_tm.hpp" // interface to S7



class tmscm_object_rep: public object_rep {
	tmscm  handle;

	tmscm_object_rep (tmscm  obj);
	~tmscm_object_rep ();

	friend class object;
  friend tmscm  object_to_tmscm  (object o);
  friend object tmscm_to_object (tmscm  obj);
  template<typename C, typename A1> friend C* tm_new (const A1& a1); 
};

inline tmscm  object_to_tmscm  (object o) {
  tmscm_object_rep *oo = static_cast<tmscm_object_rep*>(o.operator->());
  return tmscm_caar (oo->handle);
  //return tmscm_caar ((tmscm )o->lookup ()); 
}
inline object tmscm_to_object (tmscm  obj) { return tm_new<tmscm_object_rep> (obj); }


#endif // defined OBJECT_H
