
/******************************************************************************
* MODULE     : conservative_bib.cpp
* DESCRIPTION: conservative conversions with bibtex
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "wencoding.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

string
conservative_bib_export (string src_s, tree src_t, string obj_s, tree obj_t) {
  if (get_preference ("texmacs->bibtex:conservative", "off") != "on")
    return obj_s;
  if (src_t == obj_t) return src_s;
  return obj_s;
}
