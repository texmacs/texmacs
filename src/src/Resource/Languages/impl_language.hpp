
/******************************************************************************
* MODULE     : language_internals.h
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef IMPL_LANGUAGE_H
#define IMPL_LANGUAGE_H
#include "language.hpp"

extern text_property_rep tp_normal_rep;
extern text_property_rep tp_hyph_rep;
extern text_property_rep tp_space_rep;
extern text_property_rep tp_blank_rep;
extern text_property_rep tp_period_rep;
extern text_property_rep tp_operator_rep;
extern text_property_rep tp_shortop_rep;

struct verb_language_rep: language_rep {
  verb_language_rep ();
  text_property advance (string s, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

#endif // defined IMPL_LANGUAGE_H
