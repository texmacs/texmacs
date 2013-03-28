
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

typedef unsigned int color;
enum brush_kind { brush_none, brush_color, brush_pattern };

class brush_rep: concrete_struct {
public:
  brush_kind kind;
  color c;
  tree pattern;
  int alpha;

  inline brush_rep ():
    kind (brush_color), c (0xffffffff) {}
  inline brush_rep (bool b):
    kind (b? brush_color: brush_none), c (0xffffffff) {}
  inline brush_rep (color c2):
    kind (brush_color), c (c2) {}
  brush_rep (tree p, int a);

  friend class brush;
};

class brush {
  CONCRETE(brush);
  inline brush (): rep (tm_new<brush_rep> ()) {}
  inline brush (bool b): rep (tm_new<brush_rep> (b)) {}
  inline brush (color c): rep (tm_new<brush_rep> (c)) {}
  inline brush (tree p, int a= 255): rep (tm_new<brush_rep> (p, a)) {}
};
CONCRETE_CODE(brush);

#endif // defined BRUSH_H
