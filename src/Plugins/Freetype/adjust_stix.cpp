
/******************************************************************************
* MODULE     : adjust_stix.cpp
* DESCRIPTION: Microtypography for the Stix font
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
rsub_adjust_stix (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.02);
  adjust_pair (t, "B", -0.01);
  adjust_pair (t, "F", -0.02);
  adjust_pair (t, "N", -0.02);
  adjust_pair (t, "O", -0.02);
  adjust_pair (t, "P", -0.02);
  adjust_pair (t, "T", -0.02);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "<Alpha>", 0.02);
  adjust_pair (t, "<Beta>", -0.02);
  adjust_pair (t, "<Gamma>", -0.02);
  adjust_pair (t, "<Eta>", 0.02);
  adjust_pair (t, "<Theta>", -0.02);
  adjust_pair (t, "<Iota>", 0.02);
  adjust_pair (t, "<Kappa>", 0.02);
  adjust_pair (t, "<Lambda>", 0.02);
  adjust_pair (t, "<Nu>", -0.01);
  adjust_pair (t, "<Omicron>", -0.02);
  adjust_pair (t, "<Rho>", -0.02);
  adjust_pair (t, "<Upsilon>", -0.04);
  adjust_pair (t, "<Psi>", -0.02);
  adjust_pair (t, "<Backepsilon>", 0.05);
  adjust_char (t, "<#1D714>", -0.01);
  adjust_char (t, "<b-omega>", -0.01);
  adjust_char (t, "<gamma>", -0.05);
  adjust_char (t, "<b-up-gamma>", -0.05);
  adjust_char (t, "<sigma>", -0.03);
  adjust_char (t, "<b-up-sigma>", -0.03);
  adjust_char (t, "<tau>", -0.03);
  adjust_char (t, "<b-up-tau>", -0.03);
  adjust_char (t, "<varpi>", -0.05);
  adjust_char (t, "<b-up-varpi>", -0.05);
  adjust_pair (t, "<cal-A>", -0.05);
  adjust_pair (t, "<cal-B>", -0.03);
  adjust_pair (t, "<cal-C>", -0.05);
  adjust_pair (t, "<cal-D>", -0.03);
  adjust_pair (t, "<cal-E>", -0.05);
  adjust_pair (t, "<cal-F>", -0.05);
  adjust_pair (t, "<cal-G>", -0.05);
  adjust_pair (t, "<cal-H>", -0.1);
  adjust_pair (t, "<cal-I>", -0.15);
  adjust_pair (t, "<cal-J>", -0.1);
  adjust_pair (t, "<cal-K>", -0.1);
  adjust_pair (t, "<cal-L>", -0.1);
  adjust_pair (t, "<cal-M>", -0.05);
  adjust_pair (t, "<cal-N>", -0.15);
  adjust_pair (t, "<cal-S>", -0.1);
  adjust_pair (t, "<cal-T>", -0.25);
  adjust_pair (t, "<cal-U>", -0.05);
  adjust_pair (t, "<cal-V>", -0.25);
  adjust_pair (t, "<cal-W>", -0.25);
  adjust_pair (t, "<cal-X>", -0.15);
  adjust_pair (t, "<cal-Y>", -0.15);
  adjust_pair (t, "<cal-Z>", -0.1);
  adjust_pair (t, "<cal-a>", -0.05);
  adjust_pair (t, "<cal-c>", -0.05);
  adjust_pair (t, "<cal-d>", -0.05);
  adjust_pair (t, "<cal-e>", -0.05);
  adjust_pair (t, "<cal-f>", -0.1);
  adjust_pair (t, "<cal-g>", -0.05);
  adjust_pair (t, "<cal-h>", -0.05);
  adjust_pair (t, "<cal-i>", -0.03);
  adjust_pair (t, "<cal-j>", -0.05);
  adjust_pair (t, "<cal-k>", -0.03);
  adjust_pair (t, "<cal-l>", -0.08);
  adjust_pair (t, "<cal-p>", -0.03);
  adjust_pair (t, "<cal-q>", -0.02);
  adjust_pair (t, "<cal-s>", -0.03);
  adjust_pair (t, "<cal-t>", -0.07);
  adjust_pair (t, "<cal-u>", -0.03);
  adjust_pair (t, "<cal-x>", -0.05);
  adjust_pair (t, "<cal-y>", -0.05);
  adjust_pair (t, "<cal-z>", -0.07);
  adjust_pair (t, "<bbb-D>", -0.02);
  adjust_pair (t, "<bbb-F>", 0.05);
  adjust_pair (t, "<bbb-I>", 0.03);
  adjust_pair (t, "<bbb-J>", 0.05);
  adjust_pair (t, "<bbb-M>", 0.03);
  adjust_pair (t, "<bbb-N>", 0.02);
  adjust_pair (t, "<bbb-Q>", 0.02);
  adjust_pair (t, "<bbb-T>", 0.05);
  adjust_pair (t, "<bbb-V>", 0.07);
  adjust_pair (t, "<bbb-W>", 0.07);
  adjust_pair (t, "<bbb-Y>", 0.05);
  adjust_pair (t, "<bbb-Z>", -0.03);
}
  
void
rsup_adjust_stix (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "/", 0.05);
  adjust_pair (t, "B", 0.03);
  adjust_pair (t, "D", 0.01);
  adjust_pair (t, "P", 0.03);
  adjust_pair (t, "Q", 0.01);
  adjust_pair (t, "R", 0.03);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "a", 0.01);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "c", 0.02);
  adjust_pair (t, "d", 0.01);
  adjust_pair (t, "e", 0.02);
  adjust_pair (t, "i", 0.02);
  adjust_pair (t, "l", 0.02);
  adjust_pair (t, "q", 0.01);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "<Gamma>", 0.02);
  adjust_pair (t, "<Theta>", 0.02);
  adjust_pair (t, "<Rho>", 0.02);
  adjust_pair (t, "<Phi>", 0.02);
  adjust_pair (t, "<Psi>", 0.01);
  adjust_pair (t, "<Tau>", 0.02);
  adjust_pair (t, "<Backepsilon>", 0.04);
  adjust_pair (t, "<Mho>", 0.02);
  adjust_char (t, "<#1D6FD>", 0.02);
  adjust_char (t, "<b-beta>", 0.02);
  adjust_char (t, "<#1D701>", 0.04);
  adjust_char (t, "<b-zeta>", 0.03);
  adjust_char (t, "<#1D703>", 0.05);
  adjust_char (t, "<b-theta>", 0.05);
  adjust_char (t, "<#1D709>", 0.04);
  adjust_char (t, "<b-xi>", 0.03);
  adjust_char (t, "<#1D717>", 0.02);
  adjust_char (t, "<b-vartheta>", 0.02);
  adjust_char (t, "<#1D70D>", 0.02);
  adjust_char (t, "<b-varsigma>", 0.02);
  adjust_pair (t, "<mho>", 0.02);
  adjust_char (t, "<zeta>", -0.03);
  adjust_char (t, "<b-up-zeta>", -0.03);
  adjust_char (t, "<xi>", -0.01);
  adjust_char (t, "<b-up-xi>", -0.01);
  adjust_char (t, "<psi>", -0.02);
  adjust_char (t, "<b-up-psi>", -0.02);
  adjust_pair (t, "<bbb-A>", -0.02);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-H>", 0.02);
  adjust_pair (t, "<bbb-J>", 0.01);
  adjust_pair (t, "<bbb-L>", -0.02);
  adjust_pair (t, "<bbb-M>", 0.01);
  adjust_pair (t, "<bbb-P>", 0.02);
  adjust_pair (t, "<frak-a>", -0.02);
  adjust_pair (t, "<frak-i>", -0.02);
  adjust_pair (t, "<frak-j>", 0.01);
  adjust_pair (t, "<frak-l>", -0.01);
  adjust_pair (t, "<frak-p>", 0.02);
  adjust_pair (t, "<frak-r>", 0.01);
  adjust_pair (t, "<frak-t>", 0.01);
  adjust_pair (t, "<frak-u>", -0.01);
}

void
above_adjust_stix (hashmap<string,double>& t) {
  adjust_pair (t, "b", -0.02);
  adjust_pair (t, "d", 0.06);
  adjust_pair (t, "f", -0.04);
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "k", -0.02);
  adjust_pair (t, "<ell>", 0.04);
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "x", -0.04);
  adjust_pair (t, "z", -0.04);
  adjust_pair (t, "A", 0.04);
  adjust_pair (t, "I", -0.02);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "M", -0.04);
  adjust_pair (t, "N", -0.04);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "V", -0.08);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.08);
  adjust_pair (t, "<phi>", 0.04);
  adjust_pair (t, "<omicron>", 0.04);
  adjust_pair (t, "<rho>", 0.04);
  adjust_pair (t, "<sigma>", -0.08);
  adjust_pair (t, "<tau>", -0.04);
  adjust_pair (t, "<psi>", -0.04);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.02);
  adjust_pair (t, "<cal-b>", 0.04);
  adjust_pair (t, "<cal-c>", 0.04);
  adjust_pair (t, "<cal-d>", 0.2);
  adjust_pair (t, "<cal-f>", 0.1);
  adjust_pair (t, "<cal-g>", 0.04);
  adjust_pair (t, "<cal-h>", 0.04);
  adjust_pair (t, "<cal-i>", 0.1);
  adjust_pair (t, "<cal-imath>", 0.08);
  adjust_pair (t, "<cal-j>", 0.16);
  adjust_pair (t, "<cal-jmath>", 0.12);
  adjust_pair (t, "<cal-k>", 0.04);
  adjust_pair (t, "<cal-l>", 0.04);
  adjust_pair (t, "<cal-t>", 0.04);
  adjust_pair (t, "<cal-x>", 0.02);
  adjust_pair (t, "<cal-y>", 0.02);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.1);
  adjust_pair (t, "<cal-A>", 0.1);
  adjust_pair (t, "<cal-B>", 0.04);
  adjust_pair (t, "<cal-D>", 0.04);
  adjust_pair (t, "<cal-I>", 0.06);
  adjust_pair (t, "<cal-L>", 0.06);
  adjust_pair (t, "<cal-M>", 0.1);
  adjust_pair (t, "<cal-N>", 0.04);
  adjust_pair (t, "<cal-P>", 0.04);
  adjust_pair (t, "<cal-R>", 0.04);
  adjust_pair (t, "<cal-S>", 0.06);
  above_adjust_frak (t, 1.0);
  adjust_pair (t, "<frak-f>", 0.04);
  above_adjust_bbb (t, 1.5);
  adjust_pair (t, "<bbb-f>", -0.06);
  adjust_pair (t, "<bbb-j>", -0.08);
  adjust_pair (t, "<bbb-jmath>", -0.08);
  adjust_pair (t, "<bbb-f>", -0.06);
  adjust_pair (t, "<bbb-v>", -0.06);
  adjust_pair (t, "<bbb-w>", -0.08);
  adjust_pair (t, "<bbb-y>", -0.08);
  adjust_pair (t, "<bbb-E>", 0.02);
  adjust_pair (t, "<bbb-F>", -0.12);
  adjust_pair (t, "<bbb-K>", -0.08);
  adjust_pair (t, "<bbb-L>", 0.02);
  adjust_pair (t, "<bbb-T>", -0.14);
  adjust_pair (t, "<bbb-V>", -0.08);
  adjust_pair (t, "<bbb-Y>", -0.1);
  adjust_pair (t, "1", -0.02);
  adjust_pair (t, "2", -0.04);
  adjust_pair (t, "3", -0.02);
  adjust_pair (t, "4", 0.04);
  adjust_pair (t, "5", 0.04);
  adjust_pair (t, "6", 0.04);
  adjust_pair (t, "9", -0.02);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_stix (0.0);
static hashmap<string,double> rsup_stix (0.0);
static hashmap<string,double> above_stix (0.0);

hashmap<string,double>
rsub_stix_table () {
  if (N (rsub_stix) == 0) {
    rsub_adjust_std (rsub_stix);
    rsub_adjust_stix (rsub_stix);
  }
  return rsub_stix;
}

hashmap<string,double>
rsup_stix_table () {
  if (N (rsup_stix) == 0) {
    rsup_adjust_std (rsup_stix);
    rsup_adjust_stix (rsup_stix);
  }
  return rsup_stix;
}

hashmap<string,double>
above_stix_table () {
  if (N (above_stix) == 0)
    above_adjust_stix (above_stix);
  return above_stix;
}
