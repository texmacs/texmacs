
/******************************************************************************
* MODULE     : italic_correct.h
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
