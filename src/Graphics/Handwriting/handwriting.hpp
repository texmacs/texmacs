
/******************************************************************************
* MODULE     : learn_handwriting.hpp
* DESCRIPTION: Facilities for handwriting
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Scheme/object.hpp"
#include "poly_line.hpp"

extern array<contours>       learned_glyphs;
extern array<string>         learned_names;
extern array<array<tree> >   learned_disc1;
extern array<array<double> > learned_cont1;
extern array<array<tree> >   learned_disc2;
extern array<array<double> > learned_cont2;

void register_glyph (string name, contours gl);
string recognize_glyph (contours gl);
