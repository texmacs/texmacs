
/******************************************************************************
* MODULE     : learn_handwriting.cpp
* DESCRIPTION: Facilities for learning handwriting
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "handwriting.hpp"

/******************************************************************************
* Learning glyphs
******************************************************************************/

array<contours>       learned_glyphs;
array<string>         learned_names;
array<array<tree> >   learned_disc1;
array<array<double> > learned_cont1;
array<array<tree> >   learned_disc2;
array<array<double> > learned_cont2;

void
register_glyph (string name, contours gl) {
  array<tree>   disc1;
  array<double> cont1;
  invariants (gl, 1, disc1, cont1);
  array<tree>   disc2;
  array<double> cont2;
  invariants (gl, 2, disc2, cont2);
  learned_names  << name;
  learned_glyphs << gl;
  learned_disc1  << disc1;
  learned_cont1  << cont1;
  learned_disc2  << disc2;
  learned_cont2  << cont2;
  //cout << "Added " << name << ", " << disc1 << "\n";
}
