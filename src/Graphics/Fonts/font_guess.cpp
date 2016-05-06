
/******************************************************************************
* MODULE     : font_guess.cpp
* DESCRIPTION: Font distance computation based on guessed features
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "Freetype/tt_tools.hpp"
#include "analyze.hpp"

array<string> remove_other (array<string> a, bool keep_glyphs= true);
array<string> common (array<string> v1, array<string> v2);
array<string> exclude (array<string> a, array<string> b);
array<string> remove_duplicates (array<string> a);
bool same_kind (string s1, string s2);
bool is_glyphs (string s);
bool is_category (string s);

/******************************************************************************
* Guessing features
******************************************************************************/

static int
abs_int (int i) {
  return max (i, -i);
}

array<string>
guessed_features (string family, string style) {
  array<string> r;
  array<string> a= font_database_characteristics (family, style);
  //cout << "a= " << a << "\n";

  string slant  = find_attribute_value (a, "slant");
  string vcnt   = find_attribute_value (a, "vcnt");
  string fillp  = find_attribute_value (a, "fillp");
  string lasprat= find_attribute_value (a, "lasprat");
  string pasprat= find_attribute_value (a, "pasprat");
  string lvw    = find_attribute_value (a, "lvw");
  
  bool oblique  = (slant != "" && slant != "0");
  bool italic   = oblique && contains (string ("italic=yes"), a);
  bool smallcaps= contains (string ("case=smallcaps"), a);
  bool mono     = contains (string ("mono=yes"), a);
  bool sans     = contains (string ("sans=yes"), a);
  bool irregular= contains (string ("regular=no"), a);

  if (vcnt != "" && fillp != "") {
    int vf= as_int (vcnt);
    int fp= as_int (fillp);

    // begin adjustments
    int delta= 0;
    if (oblique) delta += min (abs_int (as_int (slant)), 50) / 4;
    int asprat= 110;
    if (lasprat != "") asprat= as_int (lasprat);
    if (pasprat != "" && mono) asprat= as_int (pasprat);
    int ecart= asprat - 110;
    ecart= max (min (ecart, 80), -40);
    if (ecart > 0) delta += ecart / 8;
    else delta += ecart / 4;
    vf += delta;
    fp += delta;
    // cout << family << ", " << style << " -> " << delta << "\n";
    // end adjustments

    if (vf > 60) r << string ("black");
    else if (vf > 45) r << string ("bold");
    else if (vf > 40 && fp > 40) r << string ("bold");
    else if (vf < 10) r << string ("thin");
    else if (vf < 20 && fp < 30) r << string ("light");
  }

  if (lasprat != "" && pasprat != "" && lvw != "") {
    int lrat= as_int (lasprat);
    int prat= as_int (pasprat);
    int rat = (4*lrat + prat + 2) / 5;
    if (mono) rat= (lrat + prat) / 2;

    // begin adjustments
    int w= as_int (lvw);
    w= min (w, 40);
    rat -= w/2;
    // cout << family << ", " << style << " -> " << (w/2) << "\n";
    // end adjustments

    if (rat < 75) r << string ("condensed");
    else if (rat > 120) r << string ("wide");
  }

  if (italic) r << string ("italic");
  else if (oblique) r << string ("oblique");
  if (smallcaps) r << string ("smallcaps");
  if (mono) r << string ("mono");
  if (sans) r << string ("sansserif");

  (void) irregular;
  //if (irregular) r << string ("pen");

  return r;
}

array<string>
cautious_patch (array<string> v, array<string> w) {
  array<string> r= copy (v);
  for (int i=0; i<N(w); i++) {
    int j;
    for (j=1; j<N(r); j++)
      if (same_kind (r[j], w[i]))
        break;
    if (j == N(r)) r << w[i];
  }
  return r;
}

array<string>
guessed_features (string family, bool pure_guess) {
  array<string> r;
  array<string> allf;
  array<string> commonf;
  array<string> styles= font_database_styles (family);
  for (int i=0; i<N(styles); i++) {
    array<string> a= guessed_features (family, styles[i]);
    if (!pure_guess) {
      array<string> fn= logical_font_exact (family, styles[i]);
      fn= remove_other (fn, false);
      array<string> tail= range (fn, 1, N(fn));
      allf= remove_duplicates (append (allf, tail));
      if (i == 0) commonf= tail;
      else commonf= common (commonf, tail);
      //cout << "  Guess " << family << ", " << styles[i] << " -> "
      //     << fn << " + " << a << " -> ";
      a= cautious_patch (fn, a);
      a= range (a, 1, N(a));
      //cout << a << "\n";
    }
    if (i == 0) r= a;
    else r= common (r, a);
  }
  string master= family_to_master (family);
  //cout << "  Blacklist " << allf << " - " << commonf << " -> ";
  allf= exclude (allf, commonf);
  //cout << allf << "\n";
  //cout << "Retain " << r << " - " << allf << " -> ";
  r= exclude (r, allf);
  //cout << r << "\n";
  array<string> v;
  v << master << r;
  return v;
}

/******************************************************************************
* Guessed distances
******************************************************************************/

double
category_asym_distance (array<string> f1, array<string> f2) {
  int d=0, n=0;
  for (int i=1; i<N(f1); i++)
    if (is_category (f1[i])) {
      int j;
      for (j=1; j<N(f2); j++)
        if (f2[j] == f1[i]) break;
        else if (f2[j] == "retro" && f1[i] == "medieval") break;
        else if (f2[j] == "medieval" && f1[i] == "retro") break;
      if (j == N(f2)) d++;
      n++;
    }
  if (n == 0) return -1.0;
  return ((double) d) / ((double) n);
}

double
category_distance (array<string> f1, array<string> f2) {
  double d1= category_asym_distance (f1, f2);
  double d2= category_asym_distance (f2, f1);
  if (d1 < 0 && d2 < 0) return 0.0;
  if (d1 < 0) d1= 1.0;
  if (d2 < 0) d2= 1.0;
  return d1 + d2;
}

double
guessed_distance (string fam1, string sty1, string fam2, string sty2) {
  static hashmap<tree,double> memo (1000000.0);
  tree key= tuple (fam1, sty1, fam2, sty2);
  if (memo->contains (key)) return memo[key];
  array<string> f1= logical_font_exact (fam1, sty1);
  array<string> f2= logical_font_exact (fam2, sty2);
  array<string> v1= font_database_characteristics (fam1, sty1);
  array<string> v2= font_database_characteristics (fam2, sty2);
  double d1= category_distance (f1, f2);
  double d2= characteristic_distance (v1, v2);
  double d = d1 + d2;
  memo (key)= d;
  return d;
}

double
guessed_distance_families (string fam1, string fam2) {
  static hashmap<tree,double> memo (1000000.0);
  tree key= tuple (fam1, fam2);
  if (memo->contains (key)) return memo[key];
  array<string> stys1= font_database_styles (fam1);
  array<string> stys2= font_database_styles (fam2);
  if (N(stys1) == 0 && fam1 != "tcx" && fam1 != "tc")
    stys1= font_database_global_styles (fam1);
  if (N(stys2) == 0 && fam2 != "tcx" && fam2 != "tc")
    stys2= font_database_global_styles (fam2);
  double d= 1000000.0;
  for (int i1=0; i1<N(stys1); i1++)
    for (int i2=0; i2<N(stys2); i2++)
      d= min (d, guessed_distance (fam1, stys1[i1], fam2, stys2[i2]));
  memo (key)= d;
  return d;
}

double
guessed_distance (string master1, string master2) {
  static hashmap<tree,double> memo (1000000.0);
  if (master1 == master2) return 0.0;
  tree key= tuple (master1, master2);
  if (memo->contains (key)) return memo[key];
  array<string> fams1= master_to_families (master1);
  array<string> fams2= master_to_families (master2);
  double d= 1000000.0;
  for (int i1=0; i1<N(fams1); i1++)
    for (int i2=0; i2<N(fams2); i2++)
      d= min (d, guessed_distance_families (fams1[i1], fams2[i2]));
  memo (key)= d;
  //cout << "    " << master1 << ", " << master2 << " -> " << 100.0*d << "\n";
  return d;
}
