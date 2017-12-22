
/******************************************************************************
* MODULE     : adjust_bonum.cpp
* DESCRIPTION: Microtypography for the TeX Gyre Bonum font
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
rsub_adjust_bonum (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "/", -0.05);
  adjust_pair (t, "1", -0.02);
  adjust_pair (t, "4", 0.02);
  adjust_pair (t, "7", -0.08);
  adjust_pair (t, "|", 0.05);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, string (c), 0.03);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, string (c), 0.03);
  adjust_pair (t, "A", 0.02);
  adjust_pair (t, "E", 0.01);
  adjust_pair (t, "F", -0.03);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "K", 0.03);
  adjust_pair (t, "L", 0.05);
  adjust_pair (t, "S", 0.02);
  adjust_pair (t, "T", -0.03);
  adjust_pair (t, "U", -0.02);
  adjust_pair (t, "V", -0.04);
  adjust_pair (t, "W", -0.04);
  adjust_pair (t, "X", 0.03);
  adjust_pair (t, "Y", -0.03);
  adjust_pair (t, "Z", 0.05);
  adjust_pair (t, "a", 0.05);
  adjust_pair (t, "b", 0.02);
  adjust_pair (t, "d", 0.05);
  adjust_pair (t, "h", 0.05);
  adjust_pair (t, "i", 0.05);
  adjust_pair (t, "k", 0.05);
  adjust_pair (t, "l", 0.08);
  adjust_pair (t, "m", 0.05);
  adjust_pair (t, "n", 0.05);
  adjust_pair (t, "o", 0.03);
  adjust_pair (t, "p", 0.02);
  adjust_pair (t, "q", 0.02);
  adjust_pair (t, "t", 0.05);
  adjust_pair (t, "u", 0.05);
  adjust_pair (t, "x", 0.05);
  adjust_pair (t, "z", 0.02);
  adjust_pair (t, "<Gamma>", -0.15);
  adjust_pair (t, "<Nu>", -0.04);
  adjust_pair (t, "<Rho>", -0.08);
  adjust_pair (t, "<Tau>", -0.12);
  adjust_pair (t, "<Upsilon>", -0.1);
  adjust_pair (t, "<Psi>", -0.05);
  adjust_pair (t, "<Mho>", -0.03);
  adjust_char (t, "<#1D6FC>", -0.02);
  adjust_char (t, "<b-alpha>", -0.02);
  adjust_char (t, "<#1D6FD>", -0.02);
  adjust_char (t, "<b-beta>", -0.02);
  adjust_char (t, "<#1D6FE>", -0.03);
  adjust_char (t, "<b-gamma>", -0.03);
  adjust_char (t, "<#1D701>", -0.05);
  adjust_char (t, "<b-zeta>", -0.05);
  adjust_char (t, "<#1D703>", -0.02);
  adjust_char (t, "<b-theta>", -0.02);
  adjust_char (t, "<#1D709>", -0.05);
  adjust_char (t, "<b-xi>", -0.05);
  adjust_char (t, "<#1D70D>", -0.02);
  adjust_char (t, "<b-varsigma>", -0.02);
  adjust_char (t, "<#1D70E>", -0.02);
  adjust_char (t, "<b-sigma>", -0.02);
  adjust_char (t, "<#1D710>", -0.02);
  adjust_char (t, "<b-upsilon>", -0.02);
  adjust_char (t, "<#1D712>", -0.07);
  adjust_char (t, "<b-chi>", -0.07);
  adjust_char (t, "<#1D713>", -0.03);
  adjust_char (t, "<b-psi>", -0.03);
  adjust_char (t, "<#1D717>", -0.02);
  adjust_char (t, "<b-vartheta>", -0.02);
  adjust_char (t, "<#1D71B>", -0.15);
  adjust_char (t, "<b-varpi>", -0.15);
  adjust_char (t, "<gamma>", -0.05);
  adjust_char (t, "<b-up-gamma>", -0.05);
  adjust_pair (t, "<cal-C>", -0.1);
  adjust_pair (t, "<cal-F>", -0.15);
  adjust_pair (t, "<cal-G>", -0.1);
  adjust_pair (t, "<cal-I>", -0.15);
  adjust_pair (t, "<cal-J>", -0.05);
  adjust_pair (t, "<cal-N>", -0.25);
  adjust_pair (t, "<cal-P>", -0.05);
  adjust_pair (t, "<cal-S>", -0.15);
  adjust_pair (t, "<cal-T>", -0.25);
  adjust_pair (t, "<cal-V>", -0.25);
  adjust_pair (t, "<cal-W>", -0.25);
  adjust_pair (t, "<cal-X>", -0.1);
  adjust_pair (t, "<cal-Y>", -0.05);
  adjust_pair (t, "<cal-d>", -0.08);
  adjust_pair (t, "<cal-f>", -0.08);
  adjust_pair (t, "<cal-l>", -0.08);
  adjust_pair (t, "<cal-y>", -0.03);
  adjust_pair (t, "<cal-z>", -0.03);
  adjust_pair (t, "<bbb-F>", -0.1);
  adjust_pair (t, "<bbb-P>", -0.1);
  adjust_pair (t, "<bbb-T>", -0.1);
  adjust_pair (t, "<bbb-U>", -0.1);
  adjust_pair (t, "<bbb-V>", -0.15);
  adjust_pair (t, "<bbb-W>", -0.15);
  adjust_pair (t, "<bbb-Y>", -0.12);
}

void
rsup_adjust_bonum (hashmap<string,double>& t) {
  adjust_pair (t, "!", 0.05);
  adjust_pair (t, "?", 0.05);
  adjust_pair (t, "1", -0.05);
  adjust_pair (t, "|", 0.05);
  adjust_pair (t, "\\", -0.05);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, string (c), 0.03);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, string (c), 0.05);
  adjust_pair (t, "B", 0.05);
  adjust_pair (t, "C", 0.02);
  adjust_pair (t, "D", 0.05);
  adjust_pair (t, "E", 0.02);
  adjust_pair (t, "F", 0.02);
  adjust_pair (t, "G", 0.02);
  adjust_pair (t, "I", 0.02);
  adjust_pair (t, "K", 0.02);
  adjust_pair (t, "L", -0.07);
  adjust_pair (t, "O", 0.05);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "S", 0.05);
  adjust_pair (t, "T", 0.02);
  adjust_pair (t, "U", 0.02);
  adjust_pair (t, "V", 0.02);
  adjust_pair (t, "W", 0.02);
  adjust_pair (t, "X", 0.02);
  adjust_pair (t, "Y", 0.02);
  adjust_pair (t, "a", -0.03);
  adjust_pair (t, "f", 0.02);
  adjust_pair (t, "g", 0.02);
  adjust_pair (t, "h", -0.03);
  adjust_pair (t, "i", 0.02);
  adjust_pair (t, "j", 0.04);
  adjust_pair (t, "l", 0.03);
  adjust_pair (t, "m", -0.03);
  adjust_pair (t, "n", -0.04);
  adjust_pair (t, "q", 0.02);
  adjust_pair (t, "r", 0.02);
  adjust_pair (t, "s", 0.02);
  adjust_pair (t, "t", 0.02);
  adjust_pair (t, "u", -0.03);
  adjust_pair (t, "v", 0.02);
  adjust_pair (t, "w", 0.02);
  adjust_pair (t, "x", 0.02);
  adjust_pair (t, "y", 0.02);
  adjust_pair (t, "<Chi>", -0.03);
  adjust_char (t, "<#1D706>", -0.12);
  adjust_char (t, "<b-lambda>", -0.12);
  adjust_char (t, "<iota>", -0.03);
  adjust_char (t, "<b-up-iota>", -0.03);
  adjust_char (t, "<lambda>", -0.07);
  adjust_char (t, "<b-up-lambda>", -0.07);
  adjust_char (t, "<mu>", -0.03);
  adjust_char (t, "<b-up-mu>", -0.03);
  adjust_char (t, "<xi>", 0.03);
  adjust_char (t, "<b-up-xi>", 0.03);
  adjust_char (t, "<varkappa>", -0.05);
  adjust_char (t, "<b-up-varkappa>", -0.05);
  adjust_pair (t, "<cal-a>", -0.03);
  adjust_pair (t, "<cal-d>", -0.03);
  adjust_pair (t, "<cal-i>", -0.03);
  adjust_pair (t, "<cal-j>", -0.03);
  adjust_pair (t, "<cal-k>", 0.03);
  adjust_pair (t, "<cal-l>", -0.01);
  adjust_pair (t, "<cal-m>", -0.05);
  adjust_pair (t, "<cal-n>", -0.05);
  adjust_pair (t, "<cal-r>", 0.02);
  adjust_pair (t, "<cal-s>", 0.02);
  adjust_pair (t, "<cal-u>", -0.03);
  adjust_pair (t, "<bbb-A>", -0.05);
  adjust_pair (t, "<bbb-B>", 0.02);
  adjust_pair (t, "<bbb-D>", 0.02);
  adjust_pair (t, "<bbb-K>", -0.05);
  adjust_pair (t, "<bbb-L>", -0.1);
  adjust_pair (t, "<bbb-O>", 0.02);
  adjust_pair (t, "<bbb-R>", -0.05);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<frak-" * string (c) * ">", 0.02);
  adjust_pair (t, "<frak-U>", -0.03);
}

void
above_adjust_bonum (hashmap<string,double>& t) {
  adjust_pair (t, "b", -0.04);
  adjust_pair (t, "d", 0.04);
  adjust_pair (t, "f", -0.06);
  adjust_pair (t, "h", -0.04);
  adjust_pair (t, "k", -0.02);
  adjust_pair (t, "m", -0.02);
  adjust_pair (t, "n", -0.02);
  adjust_pair (t, "t", -0.02);
  adjust_pair (t, "u", -0.02);
  adjust_pair (t, "v", -0.02);
  adjust_pair (t, "x", -0.04);
  adjust_pair (t, "z", -0.02);
  adjust_pair (t, "A", 0.1);
  adjust_pair (t, "F", -0.04);
  adjust_pair (t, "J", 0.1);
  adjust_pair (t, "K", -0.04);
  adjust_pair (t, "L", 0.02);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "S", -0.02);
  adjust_pair (t, "T", -0.06);
  adjust_pair (t, "V", -0.06);
  adjust_pair (t, "W", -0.08);
  adjust_pair (t, "X", -0.06);
  adjust_pair (t, "Y", -0.06);
  adjust_pair (t, "<alpha>", 0.02);
  adjust_pair (t, "<beta>", 0.04);
  adjust_pair (t, "<gamma>", 0.04);
  adjust_pair (t, "<delta>", 0.06);
  adjust_pair (t, "<varepsilon>", 0.06);
  adjust_pair (t, "<epsilon>", 0.04);
  adjust_pair (t, "<zeta>", 0.08);
  adjust_pair (t, "<eta>", 0.04);
  adjust_pair (t, "<theta>", 0.08);
  adjust_pair (t, "<vartheta>", 0.08);
  adjust_pair (t, "<iota>", 0.04);
  adjust_pair (t, "<varkappa>", 0.1);
  adjust_pair (t, "<lambda>", 0.18);
  adjust_char (t, "<mu>", 0.06);
  adjust_char (t, "<nu>", 0.02);
  adjust_pair (t, "<xi>", 0.04);
  adjust_pair (t, "<omicron>", 0.04);
  adjust_pair (t, "<rho>", 0.06);
  adjust_pair (t, "<varrho>", 0.08);
  adjust_pair (t, "<varsigma>", 0.04);
  adjust_pair (t, "<tau>", 0.02);
  adjust_pair (t, "<upsilon>", 0.06);
  adjust_pair (t, "<varphi>", 0.04);
  adjust_pair (t, "<phi>", 0.14);
  adjust_pair (t, "<psi>", 0.08);
  adjust_pair (t, "<chi>", 0.04);
  adjust_pair (t, "<omega>", 0.04);
  for (char c= 'a'; c <= 'z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.08);
  adjust_pair (t, "<cal-b>", 0.04);
  adjust_pair (t, "<cal-d>", 0.16);
  adjust_pair (t, "<cal-f>", 0.16);
  adjust_pair (t, "<cal-h>", 0.04);
  adjust_pair (t, "<cal-i>", 0.06);
  adjust_pair (t, "<cal-j>", 0.16);
  adjust_pair (t, "<cal-k>", 0.04);
  adjust_pair (t, "<cal-l>", 0.06);
  adjust_pair (t, "<cal-m>", 0.04);
  adjust_pair (t, "<cal-o>", -0.04);
  adjust_pair (t, "<cal-t>", 0.04);
  adjust_pair (t, "<cal-u>", -0.02);
  adjust_pair (t, "<cal-w>", -0.02);
  for (char c= 'A'; c <= 'Z'; c++)
    adjust_pair (t, "<cal-" * string (c) * ">", 0.14);
  adjust_pair (t, "<cal-A>", 0.1);
  adjust_pair (t, "<cal-B>", 0.06);
  adjust_pair (t, "<cal-D>", 0.04);
  adjust_pair (t, "<cal-H>", 0.04);
  adjust_pair (t, "<cal-I>", -0.04);
  adjust_pair (t, "<cal-M>", 0.1);
  adjust_pair (t, "<cal-N>", 0.04);
  adjust_pair (t, "<cal-Q>", -0.04);
  adjust_pair (t, "<cal-U>", 0.04);
  adjust_pair (t, "<cal-Y>", 0.06);
  adjust_pair (t, "<cal-Z>", -0.04);
  above_adjust_frak (t, 1.0);
  adjust_pair (t, "<frak-d>", -0.04);
  adjust_pair (t, "<frak-t>", 0.005);
  adjust_pair (t, "<frak-L>", -0.02);
  adjust_pair (t, "<frak-M>", 0.02);
  adjust_pair (t, "<frak-N>", 0.02);
  above_adjust_bbb (t, 1.0);
  adjust_pair (t, "<bbb-f>", 0.01);
  adjust_pair (t, "<bbb-l>", 0.02);
  adjust_pair (t, "<bbb-m>", -0.02);
  adjust_pair (t, "<bbb-n>", -0.02);
  adjust_pair (t, "<bbb-J>", -0.02);
  adjust_pair (t, "<bbb-C>", 0.02);
  adjust_pair (t, "<bbb-K>", -0.05);
  adjust_pair (t, "<bbb-L>", -0.05);
  adjust_pair (t, "<bbb-M>", -0.04);
  adjust_pair (t, "<bbb-R>", -0.05);
  adjust_pair (t, "4", 0.03);
  adjust_pair (t, "6", 0.02);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_bonum (0.0);
static hashmap<string,double> rsup_bonum (0.0);
static hashmap<string,double> above_bonum (0.0);

hashmap<string,double>
rsub_bonum_table () {
  if (N (rsub_bonum) == 0) {
    rsub_adjust_std (rsub_bonum);
    rsub_adjust_bonum (rsub_bonum);
  }
  return rsub_bonum;
}

hashmap<string,double>
rsup_bonum_table () {
  if (N (rsup_bonum) == 0) {
    rsup_adjust_std (rsup_bonum);
    rsup_adjust_bonum (rsup_bonum);
  }
  return rsup_bonum;
}

hashmap<string,double>
above_bonum_table () {
  if (N (above_bonum) == 0) {
    above_adjust_bonum (above_bonum);
  }
  return above_bonum;
}
