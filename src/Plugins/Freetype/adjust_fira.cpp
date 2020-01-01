
/******************************************************************************
* MODULE     : adjust_fira.cpp
* DESCRIPTION: Microtypography for the Fira font
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
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
lsub_adjust_fira (hashmap<string,double>& t) {
  adjust_pair (t, "1", 0.03);
  adjust_pair (t, "<Alpha>", -0.03);
  adjust_pair (t, "<Delta>", -0.03);
  adjust_pair (t, "<Zeta>", -0.03);
  adjust_pair (t, "<Lambda>", -0.03);
  adjust_pair (t, "<Tau>", 0.05);
  adjust_pair (t, "<Chi>", -0.03);
  adjust_pair (t, "<Phi>", -0.03);
  adjust_pair (t, "<Psi>", -0.03);
}

void
lsup_adjust_fira (hashmap<string,double>& t) {
  adjust_pair (t, "7", -0.05);
  adjust_pair (t, "<Xi>", -0.03);
  adjust_pair (t, "<Sigma>", -0.02);
  adjust_pair (t, "<Tau>", -0.04);
  adjust_pair (t, "<Upsilon>", -0.04);
  adjust_pair (t, "<Chi>", -0.02);
}

void
rsub_adjust_fira (hashmap<string,double>& t) {
  adjust_pair (t, "1", 0.02);
  adjust_pair (t, "2", 0.03);
  adjust_pair (t, "7", -0.05);
  adjust_pair (t, "<Alpha>", 0.03);
  adjust_pair (t, "<Delta>", 0.05);
  adjust_pair (t, "<Epsilon>", 0.03);
  adjust_pair (t, "<Zeta>", 0.03);
  adjust_pair (t, "<Kappa>", 0.05);
  adjust_pair (t, "<Lambda>", 0.05);
  adjust_pair (t, "<Xi>", 0.03);
  adjust_pair (t, "<Rho>", -0.02);
  adjust_pair (t, "<Sigma>", 0.03);
  adjust_pair (t, "<Chi>", 0.05);
  adjust_pair (t, "<nabla>", 0.03);
}

void
rsup_adjust_fira (hashmap<string,double>& t) {
  adjust_pair (t, "1", 0.02);
  adjust_pair (t, "7", 0.02);
  adjust_pair (t, "<Alpha>", 0.03);
  adjust_pair (t, "<Gamma>", 0.03);
  adjust_pair (t, "<Delta>", 0.02);
  adjust_pair (t, "<Epsilon>", 0.03);
  adjust_pair (t, "<Zeta>", 0.03);
  adjust_pair (t, "<Theta>", 0.01);
  adjust_pair (t, "<Lambda>", 0.03);
  adjust_pair (t, "<Xi>", 0.02);
  adjust_pair (t, "<Rho>", 0.03);
  adjust_pair (t, "<Tau>", 0.03);
  adjust_pair (t, "<Upsilon>", 0.03);
  adjust_pair (t, "<Phi>", 0.01);
  adjust_pair (t, "<Psi>", 0.04);
  adjust_pair (t, "<Omega>", 0.01);
  adjust_pair (t, "<nabla>", 0.03);
}

void
above_adjust_fira (hashmap<string,double>& t) {
  adjust_pair (t, "<Beta>", -0.02);
  adjust_pair (t, "<Gamma>", 0.02);
  adjust_pair (t, "<Epsilon>", 0.02);
  adjust_pair (t, "<Eta>", 0.01);
  adjust_pair (t, "<Theta>", -0.01);
  adjust_pair (t, "<Kappa>", 0.01);
  adjust_pair (t, "<Lambda>", -0.01);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> lsub_fira (0.0);
static hashmap<string,double> lsup_fira (0.0);
static hashmap<string,double> rsub_fira (0.0);
static hashmap<string,double> rsup_fira (0.0);
static hashmap<string,double> above_fira (0.0);

hashmap<string,double>
lsub_fira_table () {
  if (N (lsub_fira) == 0) {
    lsub_adjust_std (lsub_fira);
    lsub_adjust_fira (lsub_fira);
  }
  return lsub_fira;
}

hashmap<string,double>
lsup_fira_table () {
  if (N (lsup_fira) == 0) {
    lsup_adjust_std (lsup_fira);
    lsup_adjust_fira (lsup_fira);
  }
  return lsup_fira;
}

hashmap<string,double>
rsub_fira_table () {
  if (N (rsub_fira) == 0) {
    rsub_adjust_std (rsub_fira);
    rsub_adjust_fira (rsub_fira);
  }
  return rsub_fira;
}

hashmap<string,double>
rsup_fira_table () {
  if (N (rsup_fira) == 0) {
    rsup_adjust_std (rsup_fira);
    rsup_adjust_fira (rsup_fira);
  }
  return rsup_fira;
}

hashmap<string,double>
above_fira_table () {
  if (N (above_fira) == 0) {
    above_adjust_fira (above_fira);
  }
  return above_fira;
}

/******************************************************************************
* Table initialization
******************************************************************************/

void
lsub_adjust_fira_italic (hashmap<string,double>& t) {
  adjust_pair (t, "f", -0.05);
  adjust_pair (t, "x", -0.03);
  adjust_pair (t, "z", -0.03);
  adjust_pair (t, "A", -0.05);
  adjust_pair (t, "J", -0.05);
  adjust_pair (t, "T", 0.05);
  adjust_pair (t, "X", -0.05);
  adjust_pair (t, "Z", -0.05);
}

void
lsup_adjust_fira_italic (hashmap<string,double>& t) {
  adjust_pair (t, "g", -0.03);
}

void
rsub_adjust_fira_italic (hashmap<string,double>& t) {
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "l", 0.01);
  adjust_pair (t, "m", -0.01);
  adjust_pair (t, "t", 0.03);
  adjust_pair (t, "x", 0.01);
  adjust_pair (t, "z", 0.02);
  adjust_pair (t, "A", 0.01);
  adjust_pair (t, "E", 0.01);
  adjust_pair (t, "K", 0.02);
  adjust_pair (t, "L", 0.03);
  adjust_pair (t, "P", -0.01);
  adjust_pair (t, "X", 0.02);
  adjust_pair (t, "<zeta>", 0.03);
  adjust_pair (t, "<xi>", 0.02);
  adjust_pair (t, "<iota>", 0.05);
  adjust_pair (t, "<kappa>", 0.02);
  adjust_pair (t, "<lambda>", 0.03);
  adjust_pair (t, "<xi>", 0.03);
  adjust_pair (t, "<pi>", 0.02);
  adjust_pair (t, "<omega>", 0.02);
  adjust_pair (t, "<varepsilon>", 0.05);
  adjust_pair (t, "<varkappa>", 0.04);
  adjust_pair (t, "<varpi>", 0.02);
  adjust_pair (t, "<varsigma>", 0.02);
  adjust_pair (t, "<varphi>", 0.04);
  adjust_pair (t, "<nabla>", 0.03);
}

void
rsup_adjust_fira_italic (hashmap<string,double>& t) {
  adjust_pair (t, "a", 0.01);
  adjust_pair (t, "c", 0.03);
  adjust_pair (t, "e", 0.01);
  adjust_pair (t, "i", 0.03);
  adjust_pair (t, "j", 0.03);
  adjust_pair (t, "k", 0.04);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "o", 0.01);
  adjust_pair (t, "p", 0.02);
  adjust_pair (t, "q", 0.01);
  adjust_pair (t, "r", 0.05);
  adjust_pair (t, "s", 0.05);
  adjust_pair (t, "t", 0.04);
  adjust_pair (t, "u", 0.02);
  adjust_pair (t, "v", 0.03);
  adjust_pair (t, "w", 0.04);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "y", 0.02);
  adjust_pair (t, "z", 0.05);
  adjust_pair (t, "A", 0.03);
  adjust_pair (t, "B", 0.02);
  adjust_pair (t, "C", 0.03);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "E", 0.03);
  adjust_pair (t, "F", 0.03);
  adjust_pair (t, "G", 0.04);
  adjust_pair (t, "H", 0.02);
  adjust_pair (t, "I", 0.02);
  adjust_pair (t, "J", 0.02);
  adjust_pair (t, "N", 0.02);
  adjust_pair (t, "O", 0.01);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.02);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "S", 0.04);
  adjust_pair (t, "T", 0.03);
  adjust_pair (t, "V", 0.04);
  adjust_pair (t, "W", 0.01);
  adjust_pair (t, "Y", 0.04);
  adjust_pair (t, "Z", 0.04);
  adjust_pair (t, "<gamma>", 0.02);
  adjust_pair (t, "<epsilon>", 0.03);
  adjust_pair (t, "<zeta>", 0.03);
  adjust_pair (t, "<theta>", 0.02);
  adjust_pair (t, "<iota>", 0.03);
  adjust_pair (t, "<kappa>", 0.02);
  adjust_pair (t, "<nu>", 0.03);
  adjust_pair (t, "<pi>", 0.04);
  adjust_pair (t, "<sigma>", 0.04);
  adjust_pair (t, "<tau>", 0.05);
  adjust_pair (t, "<chi>", 0.03);
  adjust_pair (t, "<varepsilon>", 0.03);
  adjust_pair (t, "<vartheta>", 0.02);
  adjust_pair (t, "<varpi>", 0.04);
  adjust_pair (t, "<varsigma>", 0.04);
  adjust_pair (t, "<nabla>", 0.03);
}

void
above_adjust_fira_italic (hashmap<string,double>& t) {
  adjust_pair (t, "a", 0.04);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "c", 0.03);
  adjust_pair (t, "d", 0.1);
  adjust_pair (t, "f", -0.01);
  adjust_pair (t, "g", 0.03);
  adjust_pair (t, "h", 0.01);
  adjust_pair (t, "i", 0.01);
  adjust_pair (t, "j", 0.01);
  adjust_pair (t, "l", 0.04);
  adjust_pair (t, "m", 0.01);
  adjust_pair (t, "n", 0.02);
  adjust_pair (t, "o", 0.01);
  adjust_pair (t, "q", 0.03);
  adjust_pair (t, "r", -0.01);
  adjust_pair (t, "u", 0.02);
  adjust_pair (t, "v", -0.02);
  adjust_pair (t, "x", -0.02);
  adjust_pair (t, "y", -0.02);
  adjust_pair (t, "z", 0.01);
  adjust_pair (t, "A", 0.07);
  adjust_pair (t, "B", 0.02);
  adjust_pair (t, "C", 0.01);
  adjust_pair (t, "D", 0.02);
  adjust_pair (t, "E", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "H", 0.04);
  adjust_pair (t, "I", 0.02);
  adjust_pair (t, "J", 0.03);
  adjust_pair (t, "L", 0.03);
  adjust_pair (t, "M", 0.03);
  adjust_pair (t, "N", 0.02);
  adjust_pair (t, "O", 0.02);
  adjust_pair (t, "Q", 0.01);
  adjust_pair (t, "T", -0.02);
  adjust_pair (t, "U", 0.06);
  adjust_pair (t, "X", 0.01);
  adjust_pair (t, "Y", -0.01);
  adjust_pair (t, "Z", 0.01);
  adjust_pair (t, "Z", 0.01);
  adjust_pair (t, "<alpha>", 0.04);
  adjust_pair (t, "<beta>", 0.04);
  adjust_pair (t, "<gamma>", -0.01);
  adjust_pair (t, "<delta>", 0.06);
  adjust_pair (t, "<epsilon>", 0.04);
  adjust_pair (t, "<eta>", 0.03);
  adjust_pair (t, "<theta>", 0.03);
  adjust_pair (t, "<iota>", 0.01);
  adjust_pair (t, "<lambda>", 0.01);
  adjust_pair (t, "<mu>", 0.03);
  adjust_pair (t, "<nu>", -0.03);
  adjust_pair (t, "<omicron>", 0.03);
  adjust_pair (t, "<rho>", 0.03);
  adjust_pair (t, "<tau>", -0.02);
  adjust_pair (t, "<upsilon>", 0.03);
  adjust_pair (t, "<varphi>", 0.04);
  adjust_pair (t, "<psi>", 0.04);
  adjust_pair (t, "<chi>", -0.02);
  adjust_pair (t, "<omega>", 0.01);
  adjust_pair (t, "<varepsilon>", 0.02);
  adjust_pair (t, "<varkappa>", 0.02);
  adjust_pair (t, "<varrho>", 0.02);
  adjust_pair (t, "<phi>", 0.06);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> lsub_fira_italic (0.0);
static hashmap<string,double> lsup_fira_italic (0.0);
static hashmap<string,double> rsub_fira_italic (0.0);
static hashmap<string,double> rsup_fira_italic (0.0);
static hashmap<string,double> above_fira_italic (0.0);

hashmap<string,double>
lsub_fira_italic_table () {
  if (N (lsub_fira_italic) == 0) {
    lsub_adjust_std (lsub_fira_italic);
    lsub_adjust_fira_italic (lsub_fira_italic);
  }
  return lsub_fira_italic;
}

hashmap<string,double>
lsup_fira_italic_table () {
  if (N (lsup_fira_italic) == 0) {
    lsup_adjust_std (lsup_fira_italic);
    lsup_adjust_fira_italic (lsup_fira_italic);
  }
  return lsup_fira_italic;
}

hashmap<string,double>
rsub_fira_italic_table () {
  if (N (rsub_fira_italic) == 0) {
    rsub_adjust_std (rsub_fira_italic);
    rsub_adjust_fira_italic (rsub_fira_italic);
  }
  return rsub_fira_italic;
}

hashmap<string,double>
rsup_fira_italic_table () {
  if (N (rsup_fira_italic) == 0) {
    rsup_adjust_std (rsup_fira_italic);
    rsup_adjust_fira_italic (rsup_fira_italic);
  }
  return rsup_fira_italic;
}

hashmap<string,double>
above_fira_italic_table () {
  if (N (above_fira_italic) == 0) {
    above_adjust_fira_italic (above_fira_italic);
  }
  return above_fira_italic;
}
