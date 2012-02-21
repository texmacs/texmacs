
/******************************************************************************
* MODULE     : recognize_handwriting.cpp
* DESCRIPTION: Recognition of handwriting
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "handwriting.hpp"

/******************************************************************************
* Recognize one glyph
******************************************************************************/

void
recognize_glyph_one (contours gl, int& level, string& best, double& best_rec) {
  array<tree>   disc1;
  array<double> cont1;
  invariants (gl, 1, disc1, cont1);
  array<tree>   disc2;
  array<double> cont2;
  invariants (gl, 2, disc2, cont2);

  best= "";
  best_rec= -100.0;
  int    best_i= -1;
  for (int i=0; i<N(learned_names); i++)
    if (N(learned_glyphs[i]) == N(gl) && disc1 == learned_disc1[i]) {
      string        name= learned_names[i];
      array<double> cont= learned_cont1[i];
      double        dist= l2_norm (cont - cont1) / sqrt (N(cont1));
      double        rec = 1.0 - dist;
      if (rec > best_rec) { best_rec= rec; best= name; best_i= i; }
      //cout << name << ": " << 100.0 * rec << "%\n";
    }
  if (best != "") {
    //cout << "disc= " << disc1 << "\n";
    //cout << "cont= " << cont1 << "\n";
    level= 1;
    return;
  }

  for (int i=0; i<N(learned_names); i++)
    if (N(learned_glyphs[i]) == N(gl) && disc2 == learned_disc2[i]) {
      string        name= learned_names[i];
      array<double> cont= learned_cont2[i];
      double        dist= l2_norm (cont - cont2) / sqrt (N(cont2));
      double        rec = 1.0 - dist;
      if (rec > best_rec) { best_rec= rec; best= name; }
      //cout << name << ": " << 100.0 * rec << "%\n";
    }
  level= 2;
}

/******************************************************************************
* Recognize several glyphs
******************************************************************************/

bool
attached (poly_line pl1, poly_line pl2) {
  point p1= inf (pl1), q1= sup (pl1);
  point p2= inf (pl2), q2= sup (pl2);
  //cout << "<< " << p1 << ", " << q1 << "\n";
  //cout << ">> " << p2 << ", " << q2 << "\n";
  if (p2[1] > q1[1]) return true;
  if (p2[0] > q1[0]) return false;
  return true;
}

string
recognize_glyph (contours gl) {
  string r;
  for (int i=0; i<N(gl); ) {
    int eat= 1;
    while (i+eat < N(gl) && attached (gl[i+eat-1], gl[i+eat])) eat++;
    int    lev= 3;
    string subr= "";
    double rec= -100.0;
    recognize_glyph_one (range (gl, i, i+eat), lev, subr, rec);
    r << subr;
    i += eat;
    //cout << "Add " << eat << "\n";
  }
  return r;
}
