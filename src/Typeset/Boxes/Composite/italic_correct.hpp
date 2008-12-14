
/******************************************************************************
* MODULE     : italic_correct.h
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ITALIC_CORRECT_H
#define ITALIC_CORRECT_H
#include "boxes.hpp"

inline void
italic_correct (box b) {
  b->x1 -= b->left_correction ();
  b->x2 += b->right_correction ();
}

inline void
italic_restore (box b) {
  b->x1 += b->left_correction ();
  b->x2 -= b->right_correction ();
}

#endif // defined ITALIC_CORRECT_H
