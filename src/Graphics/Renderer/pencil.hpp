
/******************************************************************************
* MODULE     : pencil.hpp
* DESCRIPTION: pencils for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PENCIL_H
#define PENCIL_H
#include "brush.hpp"

extern int std_shrinkf;
#define PIXEL 256

enum pencil_kind {
  pencil_none,
  pencil_simple,
  pencil_standard,
  pencil_brush
};

enum pencil_cap {
  cap_square,
  cap_flat,
  cap_round
};

enum pencil_join {
  join_bevel,
  join_miter,
  join_round
};

class pencil;
class pencil_rep: abstract_struct {
public:
  inline pencil_rep () {}
  inline virtual ~pencil_rep () {}

  virtual pencil_kind get_type () = 0;
  virtual void* get_handle () = 0;

  virtual pencil set_width (SI w) = 0;
  virtual pencil set_cap (pencil_cap cap) = 0;
  virtual color get_color () = 0;
  virtual brush get_brush () = 0;
  virtual SI get_width () = 0;
  virtual pencil_cap get_cap () = 0;
  virtual pencil_join get_join () = 0;
  virtual double get_miter_lim () = 0;

  friend class pencil;
};

class pencil {
ABSTRACT_NULL(pencil);
  pencil (bool b);
  pencil (color c, SI w= std_shrinkf * PIXEL);
  pencil (brush br, SI w= std_shrinkf * PIXEL);
  pencil (tree t, int alpha, SI w= std_shrinkf * PIXEL);
  pencil (color col, SI w,
	  pencil_cap c, pencil_join j= join_round, double l= 2.0);
  pencil (brush br, SI w,
	  pencil_cap c, pencil_join j= join_round, double l= 2.0);
  pencil (tree t, int alpha,
	  SI w, pencil_cap c, pencil_join j= join_round, double l= 2.0);
  friend inline bool operator == (const pencil& a, const pencil& b);
};
ABSTRACT_NULL_CODE(pencil);

inline bool operator == (const pencil& a, const pencil& b) {
  return a.rep == b.rep; }

#endif // defined PENCIL_H
