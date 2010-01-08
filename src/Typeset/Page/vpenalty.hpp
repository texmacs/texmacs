
/******************************************************************************
* MODULE     : vpenalty.cpp
* DESCRIPTION: Vertical penalties
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*   The main 'pen' parameter is (mainly) determined at the typesetting process
* It is 0 on paragraph borders, 1 inside paragraphs, 10 when two lines near
* a paragraph border, 100 when one line from a paragraph border and
* HYPH_INVALID if a page break is prohibited.
*   The main penalty is adjusted when a given break is really not good
* enough for a given page: if the page length is flexible and we can make
* material between two breaks fit on a page of extended (or reduced) length,
* then we add EXTEND_PAGE_PENALTY resp. REDUCE_PAGE_PENALTY to
* the main penalty.
*   At equal main penalties, we look at the excentricity parameter 'exc',
* which is determined as the square of the difference between the default
* length of the material between two breaks and the default required
* page height.
******************************************************************************/

#ifndef VPENALTY_H
#define VPENALTY_H
#include "basic.hpp"

#define EXTEND_PAGE_PENALTY   33
#define REDUCE_PAGE_PENALTY   33
#define TOO_SHORT_PENALTY     10000
#define TOO_LONG_PENALTY      100000
#define UNBALANCED_COLUMNS    1000
#define LONGER_LATTER_COLUMN  1000

struct vpenalty_rep: concrete_struct {
  int pen;   // main penalty
  int exc;   // excentricity: square of shift with respect to ideal position
  inline vpenalty_rep (): pen (0), exc (0) {}
  inline vpenalty_rep (int pen2): pen (pen2), exc (0) {}
  inline vpenalty_rep (int pen2, int exc2): pen (pen2), exc (exc2) {}
};

class vpenalty {
  CONCRETE(vpenalty);
  inline vpenalty (): rep (tm_new<vpenalty_rep> ()) {}
  inline vpenalty (int pen): rep (tm_new<vpenalty_rep> (pen)) {}
  inline vpenalty (int pen, int exc): rep (tm_new<vpenalty_rep> (pen, exc)) {}
  inline bool operator == (vpenalty pen) {
    return (rep->pen == pen->pen) && (rep->exc == pen->exc); }
  inline bool operator != (vpenalty pen) {
    return (rep->pen != pen->pen) || (rep->exc != pen->exc); }
  inline bool operator < (vpenalty pen) {
    return
      (rep->pen < pen->pen) ||
      ((rep->pen == pen->pen) && (rep->exc < pen->exc)); }
  inline void operator += (vpenalty pen) {
    rep->pen += pen->pen; rep->exc += pen->exc; }
  inline vpenalty operator + (vpenalty pen) {
    return vpenalty (rep->pen + pen->pen, rep->exc + pen->exc); }
};
CONCRETE_CODE(vpenalty);

tm_ostream& operator << (tm_ostream& out, vpenalty pen);

#endif // defined VPENALTY_H
