
/******************************************************************************
* MODULE     : adjust_cmr.cpp
* DESCRIPTION: Microtypography for the Computer Modern Roman font
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
lsub_adjust_ecrm (hashmap<string,double>& t) {
  adjust_char (t, "T", 0.03);
  adjust_char (t, "V", 0.05);
  adjust_char (t, "W", 0.05);
  adjust_char (t, "Y", 0.05);
}

void
lsup_adjust_ecrm (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.1);
  adjust_char (t, "O", -0.02);
  adjust_char (t, "Q", -0.02);
}

void
rsub_adjust_ecrm (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.02);
  adjust_char (t, "B", -0.02);
  adjust_char (t, "C", -0.02);
  adjust_char (t, "D", -0.02);
  adjust_char (t, "F", -0.07);
  adjust_char (t, "I", 0.04);
  adjust_char (t, "H", 0.02);
  adjust_char (t, "J", -0.02);
  adjust_char (t, "K", 0.02);
  adjust_char (t, "M", 0.02);
  adjust_char (t, "N", -0.04);
  adjust_char (t, "O", -0.02);
  adjust_char (t, "P", -0.1);
  adjust_char (t, "Q", 0.04);
  adjust_char (t, "R", 0.02);
  adjust_char (t, "T", -0.07);
  adjust_char (t, "U", -0.04);
  adjust_char (t, "V", -0.1);
  adjust_char (t, "W", -0.1);
  adjust_char (t, "X", 0.02);
  adjust_char (t, "Y", -0.1);
  adjust_char (t, "a", 0.03);
  adjust_char (t, "d", 0.03);
  adjust_char (t, "f", 0.03);
  adjust_char (t, "g", 0.08);
  adjust_char (t, "h", 0.03);
  adjust_char (t, "i", 0.03);
  adjust_char (t, "j", 0.04);
  adjust_char (t, "k", 0.03);
  adjust_char (t, "l", 0.03);
  adjust_char (t, "m", 0.03);
  adjust_char (t, "n", 0.03);
  adjust_char (t, "q", 0.08);
  adjust_char (t, "u", 0.03);
  adjust_char (t, "v", -0.05);
  adjust_char (t, "w", -0.05);
  adjust_char (t, "x", 0.03);
  adjust_char (t, "y", -0.05);
  adjust_char (t, "z", 0.03);
}
  
void
rsup_adjust_ecrm (hashmap<string,double>& t) {
  adjust_char (t, "A", -0.08);
  adjust_char (t, "D", 0.02);
  adjust_char (t, "I", 0.04);
  adjust_char (t, "L", -0.08);
  adjust_char (t, "O", -0.02);
  adjust_char (t, "f", 0.02);
  adjust_char (t, "g", 0.02);
}

void
lsub_adjust_ecss (hashmap<string,double>& t) {
  adjust_char (t, "A", -0.02);
  adjust_char (t, "C", 0.03);
  adjust_char (t, "G", 0.03);
  adjust_char (t, "J", -0.02);
  adjust_char (t, "O", 0.01);
  adjust_char (t, "T", 0.1);
  adjust_char (t, "V", 0.05);
  adjust_char (t, "W", 0.03);
  adjust_char (t, "Y", 0.1);
  // TODO: lowercase
}

void
lsup_adjust_ecss (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.05);
  adjust_char (t, "J", 0.05);
  adjust_char (t, "V", -0.02);
  adjust_char (t, "W", -0.02);
  adjust_char (t, "X", -0.02);
  adjust_char (t, "Y", -0.02);
  // TODO: lowercase
}

void
rsub_adjust_ecss (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.05);
  adjust_char (t, "C", 0.03);
  adjust_char (t, "E", 0.05);
  adjust_char (t, "F", -0.03);
  adjust_char (t, "I", 0.03);
  adjust_char (t, "J", 0.03);
  adjust_char (t, "K", 0.05);
  adjust_char (t, "L", 0.05);
  adjust_char (t, "P", -0.07);
  adjust_char (t, "Q", 0.02);
  adjust_char (t, "R", 0.05);
  adjust_char (t, "T", -0.1);
  adjust_char (t, "V", -0.05);
  adjust_char (t, "W", -0.03);
  adjust_char (t, "X", 0.05);
  adjust_char (t, "Y", -0.1);
  adjust_char (t, "Z", 0.05);
  // TODO: lowercase
}
  
void
rsup_adjust_ecss (hashmap<string,double>& t) {
  adjust_char (t, "A", -0.02);
  adjust_char (t, "B", 0.02);
  adjust_char (t, "D", 0.02);
  adjust_char (t, "E", 0.02);
  adjust_char (t, "F", 0.03);
  adjust_char (t, "G", 0.02);
  adjust_char (t, "I", 0.02);
  adjust_char (t, "J", 0.02);
  adjust_char (t, "L", -0.1);
  adjust_char (t, "O", 0.03);
  adjust_char (t, "P", 0.02);
  adjust_char (t, "Q", 0.03);
  adjust_char (t, "R", 0.02);
  adjust_char (t, "S", 0.01);
  // TODO: lowercase
}

void
lsub_adjust_cmr (hashmap<string,double>& t) {
  lsub_adjust_ecrm (t);
}

void
lsup_adjust_cmr (hashmap<string,double>& t) {
  adjust_char (t, "\1", 0.1); // Delta
  adjust_char (t, "\3", 0.1); // Lambda
  adjust_char (t, "/", 0.05);
  lsup_adjust_ecrm (t);
}

void
rsub_adjust_cmr (hashmap<string,double>& t) {
  string empty= "a"; empty[0]= '\0';
  adjust_char (t, empty, -0.15); // Gamma
  adjust_char (t, "\2", -0.02);  // Theta
  adjust_char (t, "\3", 0.03);   // Lambda
  adjust_char (t, "\7", -0.1);   // Upsilon
  adjust_char (t, "\11", -0.07); // Psi
  adjust_char (t, "!", 0.05);
  adjust_char (t, "/", -0.1);
  adjust_char (t, "7", -0.07);
  rsub_adjust_ecrm (t);
}
  
void
rsup_adjust_cmr (hashmap<string,double>& t) {
  adjust_char (t, "\1", -0.08); // Delta
  adjust_char (t, "\2", -0.02); // Theta
  adjust_char (t, "\3", -0.08); // Lambda
  adjust_char (t, "!", 0.05);
  adjust_char (t, "\\", -0.1);
  adjust_char (t, "1", -0.05);
  rsup_adjust_ecrm (t);
}

void
above_adjust_cmr (hashmap<string,double>& t) {
  adjust_char (t, "1", -0.01);
  adjust_char (t, "2", -0.02);
  adjust_char (t, "4", 0.03);
  adjust_char (t, "9", -0.02);
}

void
lsub_adjust_cmmi (hashmap<string,double>& t) {
  adjust_char (t, "f", 0.05);
  adjust_char (t, "i", -0.02);
  adjust_char (t, "l", -0.02);
  adjust_char (t, "m", -0.02);
  adjust_char (t, "n", -0.02);
  adjust_char (t, "r", -0.02);
  adjust_char (t, "t", -0.02);
  adjust_char (t, "u", -0.02);
  adjust_char (t, "v", -0.02);
  adjust_char (t, "w", -0.02);
  adjust_char (t, "x", -0.02);
  adjust_char (t, "V", 0.05);
  adjust_char (t, "Y", 0.05);
  adjust_char (t, "\41", -0.03);  // omega
}

void
lsup_adjust_cmmi (hashmap<string,double>& t) {
  adjust_char (t, "a", 0.05);
  adjust_char (t, "c", 0.05);
  adjust_char (t, "d", 0.05);
  adjust_char (t, "e", 0.05);
  adjust_char (t, "f", 0.1);
  adjust_char (t, "g", 0.05);
  adjust_char (t, "h", 0.03);
  adjust_char (t, "j", 0.03);
  adjust_char (t, "o", 0.05);
  adjust_char (t, "q", 0.05);
  adjust_char (t, "s", 0.05);
  adjust_char (t, "t", -0.03);
  adjust_char (t, "z", 0.03);
  adjust_char (t, "A", 0.15);
  adjust_char (t, "B", 0.1);
  adjust_char (t, "D", 0.1);
  adjust_char (t, "E", 0.1);
  adjust_char (t, "F", 0.1);
  adjust_char (t, "H", 0.1);
  adjust_char (t, "I", 0.08);
  adjust_char (t, "J", 0.08);
  adjust_char (t, "K", 0.1);
  adjust_char (t, "L", 0.1);
  adjust_char (t, "M", 0.1);
  adjust_char (t, "N", 0.1);
  adjust_char (t, "P", 0.1);
  adjust_char (t, "R", 0.1);
  adjust_char (t, "X", 0.05);
  adjust_char (t, "Z", 0.05);
  adjust_char (t, "\14", 0.03);  // beta
  adjust_char (t, "\25", 0.05);  // lambda
  adjust_char (t, "\26", 0.03);  // mu
  adjust_char (t, "\32", 0.05);  // rho
  adjust_char (t, "\45", 0.05);  // varrho
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
  adjust_char (t, "\25", -0.04);  // lambda
  adjust_char (t, "b", 0.02);
  adjust_char (t, "c", 0.03);
  adjust_char (t, "d", 0.03);
  adjust_char (t, "t", 0.02);
  adjust_char (t, "A", -0.03);
}

void
above_adjust_cmmi (hashmap<string,double>& t) {
  adjust_char (t, "d", 0.1);
  adjust_char (t, "f", 0.02);
  adjust_char (t, "h", -0.02);
  adjust_char (t, "`", 0.04);   // ell
  adjust_char (t, "r", -0.04);
  adjust_char (t, "A", 0.12);
  adjust_char (t, "J", 0.06);
  adjust_char (t, "R", 0.02);
  adjust_char (t, "T", -0.04);
  adjust_char (t, "U", -0.04);
  adjust_char (t, "V", -0.1);
  adjust_char (t, "W", -0.06);
  adjust_char (t, "Y", -0.1);
  adjust_char (t, "\36", 0.06);  // varphi
  adjust_char (t, "\32", 0.04);  // rho
  adjust_char (t, "\33", -0.06); // sigma
  adjust_char (t, "\34", -0.06); // tau
  adjust_char (t, "\40", 0.06);  // psi
}

void
lsub_adjust_cmsy (hashmap<string,double>& t) {
  adjust_char (t, "D", 0.03);
  adjust_char (t, "H", 0.03);
  adjust_char (t, "I", -0.03);
  adjust_char (t, "P", 0.03);
  adjust_char (t, "Q", 0.03);
  adjust_char (t, "R", 0.03);
  adjust_char (t, "T", 0.03);
  adjust_char (t, "U", -0.03);
  adjust_char (t, "V", 0.05);
  adjust_char (t, "W", 0.05);
  adjust_char (t, "r", 0.1);   // nabla
}

void
lsup_adjust_cmsy (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.1);
  adjust_char (t, "C", -0.03);
  adjust_char (t, "F", 0.07);
  adjust_char (t, "J", 0.05);
  adjust_char (t, "L", 0.05);
  adjust_char (t, "M", 0.1);
  adjust_char (t, "N", 0.05);
  adjust_char (t, "Q", 0.03);
  adjust_char (t, "U", -0.05);
  adjust_char (t, "X", 0.05);
  adjust_char (t, "Z", 0.05);
}

void
above_adjust_cmsy (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.18);
  adjust_char (t, "F", 0.04);
  adjust_char (t, "H", 0.04);
  adjust_char (t, "I", 0.04);
  adjust_char (t, "J", 0.06);
  adjust_char (t, "L", 0.1);
  adjust_char (t, "M", 0.06);
  adjust_char (t, "Q", 0.04);
  adjust_char (t, "S", 0.02);
  adjust_char (t, "T", -0.08);
  adjust_char (t, "U", -0.04);
  adjust_char (t, "V", -0.06);
  adjust_char (t, "W", -0.04);
  adjust_char (t, "Y", -0.06);
}

void
lsub_adjust_bbm (hashmap<string,double>& t) {
  adjust_char (t, "V", 0.05);
  adjust_char (t, "W", 0.05);
  adjust_char (t, "Y", 0.05);
}

void
lsup_adjust_bbm (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.05);
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

void
above_adjust_bbm (hashmap<string,double>& t) {
  adjust_char (t, "b", -0.08);
  adjust_char (t, "d", 0.06);
  adjust_char (t, "f", 0.06);
  adjust_char (t, "h", -0.08);
  adjust_char (t, "k", -0.08);
  adjust_char (t, "B", -0.02);
  adjust_char (t, "C", 0.04);
  adjust_char (t, "D", -0.02);
  adjust_char (t, "J", 0.04);
  adjust_char (t, "L", -0.08);
  adjust_char (t, "R", -0.04);
}

void
above_adjust_eufm (hashmap<string,double>& t) {
  adjust_char (t, "b", -0.04);
  adjust_char (t, "d", -0.02);
  adjust_char (t, "f", 0.02);
  adjust_char (t, "h", -0.04);
  adjust_char (t, "l", 0.04);
  adjust_char (t, "m", -0.02);
  adjust_char (t, "t", 0.02);
  adjust_char (t, "x", -0.02);
  adjust_char (t, "C", 0.02);
  adjust_char (t, "E", 0.04);
  adjust_char (t, "L", 0.02);
  adjust_char (t, "M", 0.02);
  adjust_char (t, "P", 0.02);
  adjust_char (t, "R", 0.02);
  adjust_char (t, "T", 0.02);
}

void
rsup_adjust_rsfs (hashmap<string,double>& t) {
  adjust_char (t, "A", 0.01);
  adjust_char (t, "B", 0.02);
  adjust_char (t, "C", 0.05);
  adjust_char (t, "J", -0.05);
  adjust_char (t, "L", 0.05);
  adjust_char (t, "O", 0.05);
  adjust_char (t, "Q", 0.02);
  adjust_char (t, "R", 0.02);
  adjust_char (t, "S", 0.02);
  adjust_char (t, "X", -0.02);
  adjust_char (t, "Z", 0.02);
}

void
rsub_adjust_rsfs (hashmap<string,double>& t) {
  adjust_char (t, "F", -0.03);
  adjust_char (t, "I", -0.03);
  adjust_char (t, "J", -0.07);
  adjust_char (t, "P", -0.04);
  adjust_char (t, "S", -0.04);
  adjust_char (t, "V", -0.02);
  adjust_char (t, "W", -0.03);
}

void
above_adjust_rsfs (hashmap<string,double>& t) {
  adjust_char (t, "G", -0.02);
  adjust_char (t, "H", -0.02);
  adjust_char (t, "J", 0.07);
  adjust_char (t, "K", -0.04);
  adjust_char (t, "M", 0.05);
  adjust_char (t, "O", -0.02);
  adjust_char (t, "T", -0.02);
  adjust_char (t, "U", -0.02);
  adjust_char (t, "V", -0.07);
  adjust_char (t, "W", -0.07);
  adjust_char (t, "X", -0.03);
  adjust_char (t, "Y", -0.04);
}

/******************************************************************************
* Interface
******************************************************************************/

hashmap<string,double>
lsub_ecrm_table () {
  static hashmap<string,double> lsub_ecrm (0.0);
  if (N (lsub_ecrm) == 0) lsub_adjust_ecrm (lsub_ecrm);
  return lsub_ecrm;
}

hashmap<string,double>
lsup_ecrm_table () {
  static hashmap<string,double> lsup_ecrm (0.0);
  if (N (lsup_ecrm) == 0) lsup_adjust_ecrm (lsup_ecrm);
  return lsup_ecrm;
}

hashmap<string,double>
rsub_ecrm_table () {
  static hashmap<string,double> rsub_ecrm (0.0);
  if (N (rsub_ecrm) == 0) rsub_adjust_ecrm (rsub_ecrm);
  return rsub_ecrm;
}

hashmap<string,double>
rsup_ecrm_table () {
  static hashmap<string,double> rsup_ecrm (0.0);
  if (N (rsup_ecrm) == 0) rsup_adjust_ecrm (rsup_ecrm);
  return rsup_ecrm;
}

hashmap<string,double>
lsub_ecss_table () {
  static hashmap<string,double> lsub_ecss (0.0);
  if (N (lsub_ecss) == 0) lsub_adjust_ecss (lsub_ecss);
  return lsub_ecss;
}

hashmap<string,double>
lsup_ecss_table () {
  static hashmap<string,double> lsup_ecss (0.0);
  if (N (lsup_ecss) == 0) lsup_adjust_ecss (lsup_ecss);
  return lsup_ecss;
}

hashmap<string,double>
rsub_ecss_table () {
  static hashmap<string,double> rsub_ecss (0.0);
  if (N (rsub_ecss) == 0) rsub_adjust_ecss (rsub_ecss);
  return rsub_ecss;
}

hashmap<string,double>
rsup_ecss_table () {
  static hashmap<string,double> rsup_ecss (0.0);
  if (N (rsup_ecss) == 0) rsup_adjust_ecss (rsup_ecss);
  return rsup_ecss;
}

hashmap<string,double>
lsub_cmr_table () {
  static hashmap<string,double> lsub_cmr (0.0);
  if (N (lsub_cmr) == 0) lsub_adjust_cmr (lsub_cmr);
  return lsub_cmr;
}

hashmap<string,double>
lsup_cmr_table () {
  static hashmap<string,double> lsup_cmr (0.0);
  if (N (lsup_cmr) == 0) lsup_adjust_cmr (lsup_cmr);
  return lsup_cmr;
}

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
above_cmr_table () {
  static hashmap<string,double> above_cmr (0.0);
  if (N (above_cmr) == 0) above_adjust_cmr (above_cmr);
  return above_cmr;
}

hashmap<string,double>
lsub_cmmi_table () {
  static hashmap<string,double> lsub_cmmi (0.0);
  if (N (lsub_cmmi) == 0) lsub_adjust_cmmi (lsub_cmmi);
  return lsub_cmmi;
}

hashmap<string,double>
lsup_cmmi_table () {
  static hashmap<string,double> lsup_cmmi (0.0);
  if (N (lsup_cmmi) == 0) lsup_adjust_cmmi (lsup_cmmi);
  return lsup_cmmi;
}

hashmap<string,double>
rsub_cmmi_table () {
  static hashmap<string,double> rsub_cmmi (0.0);
  if (N (rsub_cmmi) == 0) rsub_adjust_cmmi (rsub_cmmi);
  return rsub_cmmi;
}

hashmap<string,double>
rsup_cmmi_table () {
  static hashmap<string,double> rsup_cmmi (0.0);
  if (N (rsup_cmmi) == 0) rsup_adjust_cmmi (rsup_cmmi);
  return rsup_cmmi;
}

hashmap<string,double>
above_cmmi_table () {
  static hashmap<string,double> above_cmmi (0.0);
  if (N (above_cmmi) == 0) above_adjust_cmmi (above_cmmi);
  return above_cmmi;
}

hashmap<string,double>
lsub_cmsy_table () {
  static hashmap<string,double> lsub_cmsy (0.0);
  if (N (lsub_cmsy) == 0) lsub_adjust_cmsy (lsub_cmsy);
  return lsub_cmsy;
}

hashmap<string,double>
lsup_cmsy_table () {
  static hashmap<string,double> lsup_cmsy (0.0);
  if (N (lsup_cmsy) == 0) lsup_adjust_cmsy (lsup_cmsy);
  return lsup_cmsy;
}

hashmap<string,double>
above_cmsy_table () {
  static hashmap<string,double> above_cmsy (0.0);
  if (N (above_cmsy) == 0) above_adjust_cmsy (above_cmsy);
  return above_cmsy;
}

hashmap<string,double>
lsub_bbm_table () {
  static hashmap<string,double> lsub_bbm (0.0);
  if (N (lsub_bbm) == 0) lsub_adjust_bbm (lsub_bbm);
  return lsub_bbm;
}

hashmap<string,double>
lsup_bbm_table () {
  static hashmap<string,double> lsup_bbm (0.0);
  if (N (lsup_bbm) == 0) lsup_adjust_bbm (lsup_bbm);
  return lsup_bbm;
}

hashmap<string,double>
rsub_bbm_table () {
  static hashmap<string,double> rsub_bbm (0.0);
  if (N (rsub_bbm) == 0) rsub_adjust_bbm (rsub_bbm);
  return rsub_bbm;
}

hashmap<string,double>
rsup_bbm_table () {
  static hashmap<string,double> rsup_bbm (0.0);
  if (N (rsup_bbm) == 0) rsup_adjust_bbm (rsup_bbm);
  return rsup_bbm;
}

hashmap<string,double>
above_bbm_table () {
  static hashmap<string,double> above_bbm (0.0);
  if (N (above_bbm) == 0) above_adjust_bbm (above_bbm);
  return above_bbm;
}

hashmap<string,double>
above_eufm_table () {
  static hashmap<string,double> above_eufm (0.0);
  if (N (above_eufm) == 0) above_adjust_eufm (above_eufm);
  return above_eufm;
}

hashmap<string,double>
rsub_rsfs_table () {
  static hashmap<string,double> rsub_rsfs (0.0);
  if (N (rsub_rsfs) == 0) rsub_adjust_rsfs (rsub_rsfs);
  return rsub_rsfs;
}

hashmap<string,double>
rsup_rsfs_table () {
  static hashmap<string,double> rsup_rsfs (0.0);
  if (N (rsup_rsfs) == 0) rsup_adjust_rsfs (rsup_rsfs);
  return rsup_rsfs;
}

hashmap<string,double>
above_rsfs_table () {
  static hashmap<string,double> above_rsfs (0.0);
  if (N (above_rsfs) == 0) above_adjust_rsfs (above_rsfs);
  return above_rsfs;
}
