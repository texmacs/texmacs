
/******************************************************************************
* MODULE     : adjust_cmr.cpp
* DESCRIPTION: Microtypography for the TeX Gyre Cmr font
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
rsub_adjust_cmr (hashmap<string,double>& t) {
  string empty= "a"; empty[0]= '\0';
  adjust_char (t, empty, -0.15);
  adjust_char (t, "\2", -0.02);
  adjust_char (t, "\3", 0.03);
  adjust_char (t, "\7", -0.1);
  adjust_char (t, "\11", -0.07);
  adjust_char (t, "!", 0.05);
  adjust_char (t, "/", -0.1);
  adjust_char (t, "7", -0.07);
  adjust_char (t, "B", -0.02);
  adjust_char (t, "I", 0.04);
  adjust_char (t, "K", 0.02);
  adjust_char (t, "M", 0.02);
  adjust_char (t, "N", -0.04);
  adjust_char (t, "O", -0.02);
  adjust_char (t, "P", -0.1);
  adjust_char (t, "T", -0.07);
}
  
void
rsup_adjust_cmr (hashmap<string,double>& t) {
  adjust_char (t, "\1", -0.08);
  adjust_char (t, "\2", -0.02);
  adjust_char (t, "\3", -0.08);
  adjust_char (t, "!", 0.05);
  adjust_char (t, "\\", -0.1);
  adjust_char (t, "1", -0.05);
  adjust_char (t, "A", -0.08);
  adjust_char (t, "I", 0.04);
  adjust_char (t, "O", -0.02);
}

void
rsub_adjust_cmmi (hashmap<string,double>& t) {
  adjust_char (t, "N", -0.03);
  adjust_char (t, "T", -0.05);
  adjust_char (t, "U", -0.03);
  adjust_char (t, "V", -0.05);
  adjust_char (t, "W", -0.1);
  adjust_char (t, "X", -0.03);
  adjust_char (t, "Y", -0.05);
}
  
void
rsup_adjust_cmmi (hashmap<string,double>& t) {
  adjust_char (t, "\25", -0.04);
  adjust_char (t, "A", -0.03);
}

void
above_adjust_cmmi (hashmap<string,double>& t) {
  adjust_pair (t, "d", 0.1);
  adjust_pair (t, "f", 0.02);
  adjust_pair (t, "h", -0.02);
  adjust_pair (t, "`", 0.04); // ell
  adjust_pair (t, "r", -0.04);
  adjust_pair (t, "A", 0.12);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "R", 0.02);
  adjust_pair (t, "T", -0.04);
  adjust_pair (t, "U", -0.04);
  adjust_pair (t, "V", -0.1);
  adjust_pair (t, "W", -0.06);
  adjust_pair (t, "Y", -0.1);
  adjust_pair (t, "\36", 0.06); // varphi
  adjust_pair (t, "\32", 0.04); // rho
  adjust_pair (t, "\33", -0.06); // sigma
  adjust_pair (t, "\34", -0.06); // tau
  adjust_pair (t, "\40", 0.06); // psi
}

void
above_adjust_cmsy (hashmap<string,double>& t) {
  adjust_pair (t, "A", 0.18);
  adjust_pair (t, "F", 0.04);
  adjust_pair (t, "H", 0.04);
  adjust_pair (t, "I", 0.04);
  adjust_pair (t, "J", 0.06);
  adjust_pair (t, "L", 0.1);
  adjust_pair (t, "M", 0.06);
  adjust_pair (t, "Q", 0.04);
  adjust_pair (t, "S", 0.02);
  adjust_pair (t, "T", -0.08);
  adjust_pair (t, "U", -0.04);
  adjust_pair (t, "V", -0.06);
  adjust_pair (t, "W", -0.04);
  adjust_pair (t, "Y", -0.06);
}

void
rsub_adjust_bbm (hashmap<string,double>& t) {
  adjust_char (t, "E", 0.02);
  adjust_char (t, "F", -0.1);
  adjust_char (t, "I", 0.02);
  adjust_char (t, "J", -0.03);
  adjust_char (t, "N", -0.03);
  adjust_char (t, "P", -0.07);
  adjust_char (t, "T", -0.1);
  adjust_char (t, "U", -0.05);
  adjust_char (t, "V", -0.1);
  adjust_char (t, "W", -0.1);
  adjust_char (t, "Y", -0.12);
}
  
void
rsup_adjust_bbm (hashmap<string,double>& t) {
  adjust_char (t, "A", -0.08);
  adjust_char (t, "I", 0.02);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashmap<string,double> rsub_cmr (0.0);
static hashmap<string,double> rsup_cmr (0.0);
static hashmap<string,double> rsub_cmmi (0.0);
static hashmap<string,double> rsup_cmmi (0.0);
static hashmap<string,double> above_cmmi (0.0);
static hashmap<string,double> above_cmsy (0.0);
static hashmap<string,double> rsub_bbm (0.0);
static hashmap<string,double> rsup_bbm (0.0);

hashmap<string,double>
rsub_cmr_table () {
  static hashmap<string,double> rsub_cmr (0.0);
  if (N (rsub_cmr) == 0) rsub_adjust_cmr (rsub_cmr);
  return rsub_cmr;
}

hashmap<string,double>
rsup_cmr_table () {
  static hashmap<string,double> rsup_cmr (0.0);
  if (N (rsup_cmr) == 0) rsup_adjust_cmr (rsup_cmr);
  return rsup_cmr;
}

hashmap<string,double>
rsub_cmmi_table () {
  static hashmap<string,double> rsub_cmmi (0.0);
  if (N (rsub_cmmi) == 0) rsub_adjust_cmmi (rsub_cmmi);
  return rsub_cmmi;
}

hashmap<string,double>
rsup_cmmi_table () {
  if (N (rsup_cmmi) == 0) rsup_adjust_cmmi (rsup_cmmi);
  return rsup_cmmi;
}

hashmap<string,double>
above_cmmi_table () {
  if (N (above_cmmi) == 0) above_adjust_cmmi (above_cmmi);
  return above_cmmi;
}

hashmap<string,double>
above_cmsy_table () {
  if (N (above_cmsy) == 0) above_adjust_cmsy (above_cmsy);
  return above_cmsy;
}

hashmap<string,double>
rsub_bbm_table () {
  if (N (rsub_bbm) == 0) rsub_adjust_bbm (rsub_bbm);
  return rsub_bbm;
}

hashmap<string,double>
rsup_bbm_table () {
  if (N (rsup_bbm) == 0) rsup_adjust_bbm (rsup_bbm);
  return rsup_bbm;
}
