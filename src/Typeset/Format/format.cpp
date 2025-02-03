
/******************************************************************************
* MODULE     : format.cpp
* DESCRIPTION: standard formats for placing material
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Format/format.hpp"

bool
format_none_rep::equal (format fm) {
  return (fm->type == type);
}

format_none_rep::operator tree () { return "none"; }

bool
format_width_rep::equal (format fm) {
  if (fm->type != type) return false;
  format_width fw= (format_width) fm;
  return (fw->width == width);
}

format_width_rep::operator tree () {
  return tuple ("format width", as_string (width));
}

bool
format_cell_rep::equal (format fm) {
  if (fm->type != type) return false;
  format_cell fw= (format_cell) fm;
  return
    (fw->width == width) && (fw->vpos == vpos) &&
    (fw->depth == depth) && (fw->height == height);
}

format_cell_rep::operator tree () {
  return tuple ("format cell", as_string (width));
}

bool
format_vstream_rep::equal (format fm) {
  if (fm->type != type) return false;
  format_vstream fw= (format_vstream) fm;
  return
    (fw->width == width) && (fw->before == before) && (fw->after == after);
}

format_vstream_rep::operator tree () {
  return tuple ("format vstream", as_string (width),
                as_string (N(before)), as_string (N(after)));
}

bool
query_vstream_width_rep::equal (format fm) {
  if (fm->type != type) return false;
  query_vstream_width fw= (query_vstream_width) fm;
  return (fw->before == before) && (fw->after == after);
}

query_vstream_width_rep::operator tree () {
  return tuple ("query vstream width",
                as_string (N(before)), as_string (N(after)));
}
