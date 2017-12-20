
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

void adjust_char (hashmap<string,double>& t, string c, double delta);
void adjust_pair (hashmap<string,double>& t, string c, double delta);

/******************************************************************************
* Standard corrections
******************************************************************************/

void
above_adjust_std (hashmap<string,double>& t) {
  adjust_pair (t, "d", 0.1);
  adjust_pair (t, "h", -0.04);
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "A", 0.1);
  adjust_pair (t, "J", 0.04);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "V", -0.1);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.1);
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
