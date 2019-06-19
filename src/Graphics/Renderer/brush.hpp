
/******************************************************************************
* MODULE     : brush.hpp
* DESCRIPTION: brushes for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BRUSH_H
#define BRUSH_H
#include "tree.hpp"
class url;

enum brush_kind {
  brush_none,
  brush_color,
  brush_pattern
};

class brush_rep: abstract_struct {
public:
  inline brush_rep () {}
  inline virtual ~brush_rep () {}

  virtual brush_kind get_type () = 0;
  virtual void* get_handle () = 0;

  virtual color get_color () = 0;
  virtual tree get_pattern () = 0;
  virtual url get_pattern_url ();
  virtual int get_alpha () = 0;

  friend class brush;
};

class brush {
ABSTRACT_NULL(brush);
  brush (bool b);
  brush (color c);
  brush (tree p, int a= 255);
  friend bool operator == (const brush& a, const brush& b);
};
ABSTRACT_NULL_CODE(brush);

bool operator == (const brush& a, const brush& b);
inline bool operator != (const brush& a, const brush& b) { return !(a == b); }

brush mix (brush b1, double a1, brush b2, double a2);

void  get_pattern_data (url& u, SI& w, SI& h, tree& eff, brush br, SI pixel);

#endif // defined BRUSH_H
