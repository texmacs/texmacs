
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
static hashmap<string,double> rsub_bbm (0.0);
static hashmap<string,double> rsup_bbm (0.0);

hashmap<string,double>
rsub_cmr_table () {
  if (N (rsub_cmr) == 0) rsub_adjust_cmr (rsub_cmr);
  return rsub_cmr;
}

hashmap<string,double>
rsup_cmr_table () {
  if (N (rsup_cmr) == 0) rsup_adjust_cmr (rsup_cmr);
  return rsup_cmr;
}

hashmap<string,double>
rsub_cmmi_table () {
  if (N (rsub_cmmi) == 0) rsub_adjust_cmmi (rsub_cmmi);
  return rsub_cmmi;
}

hashmap<string,double>
rsup_cmmi_table () {
  if (N (rsup_cmmi) == 0) rsup_adjust_cmmi (rsup_cmmi);
  return rsup_cmmi;
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
