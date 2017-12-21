
/******************************************************************************
* MODULE     : font_wide.cpp
* DESCRIPTION: microtypographic wide accent positioning
* COPYRIGHT  : (C) 2017  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"

/******************************************************************************
* Standard corrections
******************************************************************************/

void
above_adjust_std (hashmap<string,double>& t) {
  adjust_pair (t, "d", 0.06);
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "<ell>", 0.04);
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "A", 0.08);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "U", -0.04);
  adjust_pair (t, "V", -0.1);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.1);
  adjust_pair (t, "<phi>", 0.06);
  adjust_pair (t, "<omicron>", 0.04);
  adjust_pair (t, "<rho>", 0.04);
  adjust_pair (t, "<sigma>", -0.04);
  adjust_pair (t, "<tau>", -0.04);
  adjust_pair (t, "<psi>", 0.06);
  adjust_pair (t, "<cal-A>", 0.08);
  adjust_pair (t, "<cal-F>", 0.04);
  adjust_pair (t, "<cal-H>", 0.04);
  adjust_pair (t, "<cal-I>", 0.04);
  adjust_pair (t, "<cal-J>", 0.04);
  adjust_pair (t, "<cal-L>", 0.04);
  adjust_pair (t, "<cal-M>", 0.06);
  adjust_pair (t, "<cal-N>", 0.06);
  adjust_pair (t, "<cal-R>", 0.02);
  adjust_pair (t, "<cal-V>", 0.02);
  adjust_pair (t, "<cal-Y>", 0.02);
}

void
below_adjust_std (hashmap<string,double>& t) {
  (void) t;
}

/******************************************************************************
* Guessing further adjustments
******************************************************************************/

void
above_adjust_guessed (hashmap<string,double>& t) {
  (void) t;
}

void
below_adjust_guessed (hashmap<string,double>& t) {
  (void) t;
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> above_guessed (0.0);
static hashmap<string,double> below_guessed (0.0);

hashmap<string,double>
above_guessed_table () {
  if (N (above_guessed) == 0) {
    above_adjust_std (above_guessed);
    above_adjust_guessed (above_guessed);
  }
  return above_guessed;
}

hashmap<string,double>
below_guessed_table () {
  if (N (below_guessed) == 0) {
    below_adjust_std (below_guessed);
    below_adjust_guessed (below_guessed);
  }
  return below_guessed;
}
