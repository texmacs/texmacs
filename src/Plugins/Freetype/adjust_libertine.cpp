
/******************************************************************************
* MODULE     : adjust_libertine.cpp
* DESCRIPTION: Microtypography for the Linux Libertine font
* COPYRIGHT  : (C) 2017  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"

/******************************************************************************
* Table initialization
******************************************************************************/

void
lsub_adjust_libertine (hashmap<string,double>& t) {
  adjust_pair (t, "<Alpha>", -0.03);
  adjust_pair (t, "<Beta>", -0.03);
  adjust_pair (t, "<Gamma>", -0.03);
  adjust_pair (t, "<Delta>", -0.03);
  adjust_pair (t, "<Epsilon>", -0.03);
  adjust_pair (t, "<Zeta>", -0.03);
  adjust_pair (t, "<Eta>", -0.03);
  adjust_pair (t, "<Theta>", -0.03);
  adjust_pair (t, "<Iota>", -0.03);
  adjust_pair (t, "<Kappa>", -0.03);
  adjust_pair (t, "<Lambda>", -0.03);
  adjust_pair (t, "<Mu>", -0.03);
  adjust_pair (t, "<Nu>", -0.03);
  adjust_pair (t, "<Xi>", -0.03);
  adjust_pair (t, "<Omicron>", -0.03);
  adjust_pair (t, "<Pi>", -0.03);
  adjust_pair (t, "<Rho>", -0.03);
  adjust_pair (t, "<Sigma>", -0.03);
  adjust_pair (t, "<Tau>", 0.02);
  adjust_pair (t, "<Upsilon>", -0.03);
  adjust_pair (t, "<Phi>", -0.03);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<Chi>", -0.03);
  adjust_pair (t, "<Omega>", -0.03);
}

void
lsup_adjust_libertine (hashmap<string,double>& t) {
  (void) t;
}

void
rsub_adjust_libertine (hashmap<string,double>& t) {
  adjust_pair (t, "<Alpha>", 0.03);
  adjust_pair (t, "<Delta>", 0.02);
  adjust_pair (t, "<Eta>", 0.03);
  adjust_pair (t, "<Iota>", 0.06);
  adjust_pair (t, "<Kappa>", 0.05);
  adjust_pair (t, "<Lambda>", 0.04);
  adjust_pair (t, "<Mu>", 0.04);
  adjust_pair (t, "<Pi>", 0.03);
  adjust_pair (t, "<Rho>", -0.03);
  adjust_pair (t, "<Psi>", -0.03);
  adjust_pair (t, "<Chi>", 0.03);
  adjust_pair (t, "<Omega>", 0.03);
  adjust_pair (t, "<partial>", -0.05);
}

void
rsup_adjust_libertine (hashmap<string,double>& t) {
  adjust_pair (t, "<Beta>", 0.03);
  adjust_pair (t, "<Gamma>", 0.05);
  adjust_pair (t, "<Delta>", 0.08);
  adjust_pair (t, "<Epsilon>", 0.03);
  adjust_pair (t, "<Zeta>", 0.03);
  adjust_pair (t, "<Eta>", 0.03);
  adjust_pair (t, "<Theta>", 0.03);
  adjust_pair (t, "<Iota>", 0.05);
  adjust_pair (t, "<Lambda>", 0.03);
  adjust_pair (t, "<Omicron>", 0.05);
  adjust_pair (t, "<Pi>", 0.03);
  adjust_pair (t, "<Rho>", 0.05);
  adjust_pair (t, "<Sigma>", 0.03);
  adjust_pair (t, "<Tau>", 0.03);
  adjust_pair (t, "<Upsilon>", 0.03);
  adjust_pair (t, "<Phi>", 0.05);
  adjust_pair (t, "<Psi>", 0.03);
  adjust_pair (t, "<Chi>", 0.02);
  adjust_pair (t, "<Omega>", 0.03);
  adjust_pair (t, "<nabla>", 0.03);
}

void
above_adjust_libertine (hashmap<string,double>& t) {
  (void) t;
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> lsub_libertine (0.0);
static hashmap<string,double> lsup_libertine (0.0);
static hashmap<string,double> rsub_libertine (0.0);
static hashmap<string,double> rsup_libertine (0.0);
static hashmap<string,double> above_libertine (0.0);

hashmap<string,double>
lsub_libertine_table () {
  if (N (lsub_libertine) == 0) {
    lsub_adjust_std (lsub_libertine);
    lsub_adjust_libertine (lsub_libertine);
  }
  return lsub_libertine;
}

hashmap<string,double>
lsup_libertine_table () {
  if (N (lsup_libertine) == 0) {
    lsup_adjust_std (lsup_libertine);
    lsup_adjust_libertine (lsup_libertine);
  }
  return lsup_libertine;
}

hashmap<string,double>
rsub_libertine_table () {
  if (N (rsub_libertine) == 0) {
    rsub_adjust_std (rsub_libertine);
    rsub_adjust_libertine (rsub_libertine);
  }
  return rsub_libertine;
}

hashmap<string,double>
rsup_libertine_table () {
  if (N (rsup_libertine) == 0) {
    rsup_adjust_std (rsup_libertine);
    rsup_adjust_libertine (rsup_libertine);
  }
  return rsup_libertine;
}

hashmap<string,double>
above_libertine_table () {
  if (N (above_libertine) == 0) {
    above_adjust_libertine (above_libertine);
  }
  return above_libertine;
}

/******************************************************************************
* Table initialization
******************************************************************************/

void
lsub_adjust_libertine_italic (hashmap<string,double>& t) {
  adjust_pair (t, "T", 0.05);
}

void
lsup_adjust_libertine_italic (hashmap<string,double>& t) {
  (void) t;
}

void
rsub_adjust_libertine_italic (hashmap<string,double>& t) {
  adjust_pair (t, "a", 0.04);
  adjust_pair (t, "c", 0.03);
  adjust_pair (t, "d", 0.03);
  adjust_pair (t, "g", 0.02);
  adjust_pair (t, "h", 0.03);
  adjust_pair (t, "i", 0.05);
  adjust_pair (t, "k", 0.04);
  adjust_pair (t, "l", 0.04);
  adjust_pair (t, "m", 0.03);
  adjust_pair (t, "n", 0.02);
  adjust_pair (t, "s", 0.03);
  adjust_pair (t, "t", 0.03);
  adjust_pair (t, "u", 0.03);
  adjust_pair (t, "v", 0.02);
  adjust_pair (t, "w", 0.02);
  adjust_pair (t, "x", 0.03);
  adjust_pair (t, "z", 0.03);
  adjust_pair (t, "A", 0.03);
  adjust_pair (t, "B", 0.04);
  adjust_pair (t, "D", 0.03);
  adjust_pair (t, "E", 0.02);
  adjust_pair (t, "G", 0.03);
  adjust_pair (t, "H", 0.02);
  adjust_pair (t, "I", 0.04);
  adjust_pair (t, "J", 0.04);
  adjust_pair (t, "K", 0.02);
  adjust_pair (t, "M", 0.02);
  adjust_pair (t, "N", 0.03);
  adjust_pair (t, "O", 0.03);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "R", 0.04);
  adjust_pair (t, "S", 0.02);
  adjust_pair (t, "U", 0.02);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "X", 0.03);
  adjust_pair (t, "Z", 0.02);
  adjust_pair (t, "<alpha>", 0.04);
  adjust_pair (t, "<beta>", 0.02);
  adjust_pair (t, "<gamma>", 0.02);
  adjust_pair (t, "<delta>", 0.02);
  adjust_pair (t, "<zeta>", 0.03);
  adjust_pair (t, "<theta>", 0.02);
  adjust_pair (t, "<iota>", 0.05);
  adjust_pair (t, "<kappa>", 0.02);
  adjust_pair (t, "<lambda>", 0.03);
  adjust_pair (t, "<mu>", 0.04);
  adjust_pair (t, "<xi>", 0.03);
  adjust_pair (t, "<pi>", 0.02);
  adjust_pair (t, "<omega>", 0.02);
  adjust_pair (t, "<varepsilon>", 0.05);
  adjust_pair (t, "<varkappa>", 0.04);
  adjust_pair (t, "<varpi>", 0.02);
  adjust_pair (t, "<varsigma>", 0.02);
  adjust_pair (t, "<varphi>", 0.04);
}

void
rsup_adjust_libertine_italic (hashmap<string,double>& t) {
  adjust_pair (t, "b", 0.07);
  adjust_pair (t, "c", 0.05);
  adjust_pair (t, "d", 0.03);
  adjust_pair (t, "e", 0.05);
  adjust_pair (t, "g", 0.03);
  adjust_pair (t, "h", 0.02);
  adjust_pair (t, "i", 0.07);
  adjust_pair (t, "j", 0.05);
  adjust_pair (t, "k", 0.02);
  adjust_pair (t, "l", 0.05);
  adjust_pair (t, "m", 0.05);
  adjust_pair (t, "n", 0.02);
  adjust_pair (t, "o", 0.04);
  adjust_pair (t, "p", 0.03);
  adjust_pair (t, "q", 0.03);
  adjust_pair (t, "s", 0.07);
  adjust_pair (t, "t", 0.03);
  adjust_pair (t, "u", 0.03);
  adjust_pair (t, "v", 0.03);
  adjust_pair (t, "w", 0.05);
  adjust_pair (t, "x", 0.04);
  adjust_pair (t, "y", 0.02);
  adjust_pair (t, "z", 0.02);
  adjust_pair (t, "A", 0.05);
  adjust_pair (t, "B", 0.05);
  adjust_pair (t, "D", 0.04);
  adjust_pair (t, "E", 0.07);
  adjust_pair (t, "G", 0.05);
  adjust_pair (t, "H", 0.03);
  adjust_pair (t, "J", 0.05);
  adjust_pair (t, "K", 0.03);
  adjust_pair (t, "L", 0.03);
  adjust_pair (t, "O", 0.05);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.05);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "S", 0.03);
  adjust_pair (t, "T", 0.03);
  adjust_pair (t, "V", 0.02);
  adjust_pair (t, "Y", 0.03);
  adjust_pair (t, "<alpha>", 0.03);
  adjust_pair (t, "<beta>", 0.05);
  adjust_pair (t, "<gamma>", 0.05);
  adjust_pair (t, "<delta>", 0.03);
  adjust_pair (t, "<epsilon>", 0.05);
  adjust_pair (t, "<zeta>", 0.03);
  adjust_pair (t, "<eta>", 0.05);
  adjust_pair (t, "<theta>", 0.03);
  adjust_pair (t, "<iota>", 0.03);
  adjust_pair (t, "<kappa>", 0.03);
  adjust_pair (t, "<mu>", 0.03);
  adjust_pair (t, "<nu>", 0.03);
  adjust_pair (t, "<xi>", 0.03);
  adjust_pair (t, "<omicron>", 0.05);
  adjust_pair (t, "<pi>", 0.05);
  adjust_pair (t, "<rho>", 0.03);
  adjust_pair (t, "<sigma>", 0.05);
  adjust_pair (t, "<tau>", 0.05);
  adjust_pair (t, "<upsilon>", 0.05);
  adjust_pair (t, "<phi>", 0.05);
  adjust_pair (t, "<chi>", 0.03);
  adjust_pair (t, "<omega>", 0.05);
  adjust_pair (t, "<varepsilon>", 0.03);
  adjust_pair (t, "<vartheta>", 0.03);
  adjust_pair (t, "<varkappa>", 0.03);
  adjust_pair (t, "<varpi>", 0.07);
  adjust_pair (t, "<varsigma>", 0.03);
  adjust_pair (t, "<varphi>", 0.05);
}

void
above_adjust_libertine_italic (hashmap<string,double>& t) {
  adjust_pair (t, "a", 0.02);
  adjust_pair (t, "d", 0.1);
  adjust_pair (t, "f", -0.02);
  adjust_pair (t, "j", 0.02);
  adjust_pair (t, "l", 0.03);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "u", 0.02);
  adjust_pair (t, "x", -0.02);
  adjust_pair (t, "A", 0.12);
  adjust_pair (t, "C", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "L", 0.02);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "X", -0.02);
  adjust_pair (t, "<beta>", 0.02);
  adjust_pair (t, "<gamma>", -0.02);
  adjust_pair (t, "<epsilon>", 0.04);
  adjust_pair (t, "<theta>", 0.02);
  adjust_pair (t, "<iota>", 0.02);
  adjust_pair (t, "<lambda>", 0.05);
  adjust_pair (t, "<xi>", 0.02);
  adjust_pair (t, "<psi>", 0.02);
  adjust_pair (t, "<chi>", -0.02);
  adjust_pair (t, "<phi>", 0.05);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> lsub_libertine_italic (0.0);
static hashmap<string,double> lsup_libertine_italic (0.0);
static hashmap<string,double> rsub_libertine_italic (0.0);
static hashmap<string,double> rsup_libertine_italic (0.0);
static hashmap<string,double> above_libertine_italic (0.0);

hashmap<string,double>
lsub_libertine_italic_table () {
  if (N (lsub_libertine_italic) == 0) {
    lsub_adjust_std (lsub_libertine_italic);
    lsub_adjust_libertine_italic (lsub_libertine_italic);
  }
  return lsub_libertine_italic;
}

hashmap<string,double>
lsup_libertine_italic_table () {
  if (N (lsup_libertine_italic) == 0) {
    lsup_adjust_std (lsup_libertine_italic);
    lsup_adjust_libertine_italic (lsup_libertine_italic);
  }
  return lsup_libertine_italic;
}

hashmap<string,double>
rsub_libertine_italic_table () {
  if (N (rsub_libertine_italic) == 0) {
    rsub_adjust_std (rsub_libertine_italic);
    rsub_adjust_libertine_italic (rsub_libertine_italic);
  }
  return rsub_libertine_italic;
}

hashmap<string,double>
rsup_libertine_italic_table () {
  if (N (rsup_libertine_italic) == 0) {
    rsup_adjust_std (rsup_libertine_italic);
    rsup_adjust_libertine_italic (rsup_libertine_italic);
  }
  return rsup_libertine_italic;
}

hashmap<string,double>
above_libertine_italic_table () {
  if (N (above_libertine_italic) == 0) {
    above_adjust_libertine_italic (above_libertine_italic);
  }
  return above_libertine_italic;
}
