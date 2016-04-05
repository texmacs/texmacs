
/******************************************************************************
* MODULE     : modulation.cpp
* DESCRIPTION: font modulations
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modulation.hpp"

/******************************************************************************
* Default methods and utility routines
******************************************************************************/

void
modulation_rep::get_matrix (double& a11, double& a12,
                            double& a21, double& a22) {
  a11= 1.0; a12= 0.0; a21= 0.0; a22= 1.0;
}

modulation
modulation_rep::car () {
  FAILED ("not a composite modulation");
  return identity_modulation ();
}

modulation
modulation_rep::cdr () {
  FAILED ("not a composite modulation");
  return identity_modulation ();
}

bool
is_identity (modulation m) {
  return m->kind == modulation_identity;
}

bool
is_linear (modulation m) {
  return m->kind == modulation_identity || m->kind == modulation_linear;
}

bool
is_composite (modulation m) {
  return m->kind == modulation_composite;
}

modulation car (modulation m) { return m->car (); }
modulation cdr (modulation m) { return m->cdr (); }

/******************************************************************************
* Identity modulations
******************************************************************************/

class identity_modulation_rep: public modulation_rep {
public:
  identity_modulation_rep (): modulation_rep (modulation_identity) {}
  ~identity_modulation_rep () {}

  tree expression () { return tuple ("identity"); }
};

modulation
identity_modulation () {
  return tm_new<identity_modulation_rep> ();
}

/******************************************************************************
* Linear modulations
******************************************************************************/

class linear_modulation_rep: public modulation_rep {
  double m11, m12, m21, m22;
public:
  linear_modulation_rep (double a11, double a12, double a21, double a22):
    modulation_rep (modulation_linear),
    m11 (a11), m12 (a12), m21 (a21), m22 (a22) {}
  ~linear_modulation_rep () {}

  tree expression () {
    return tuple ("linear", as_string (m11), as_string (m12),
                            as_string (m21), as_string (m22)); }
  void get_matrix (double& a11, double& a12, double& a21, double& a22) {
    a11= m11; a12= m12; a21= m21; a22= m22; }
};

modulation
linear_modulation (double a11, double a12, double a21, double a22) {
  return tm_new<linear_modulation_rep> (a11, a12, a21, a22);
}

modulation
resize_modulation (double zoom) {
  return linear_modulation (zoom, 0.0, 0.0, zoom);
}

modulation
scale_modulation (double scalex, double scaley) {
  return linear_modulation (scalex, 0.0, 0.0, scaley);
}

modulation
slant_modulation (double slant) {
  return linear_modulation (1.0, slant, 0.0, 1.0);
}

/******************************************************************************
* Linear modulations
******************************************************************************/

class composed_modulation_rep: public modulation_rep {
  modulation m1, m2;
  double m11, m12, m21, m22;
public:
  composed_modulation_rep (modulation m1b, modulation m2b,
                           double a11, double a12, double a21, double a22):
    modulation_rep (modulation_composite), m1 (m1b), m2 (m2b),
    m11 (a11), m12 (a12), m21 (a21), m22 (a22) {}
  ~composed_modulation_rep () {}

  tree expression () {
    return tuple ("compose", m1->expression (), m2->expression ()); }
  void get_matrix (double& a11, double& a12, double& a21, double& a22) {
    a11= m11; a12= m12; a21= m21; a22= m22; }
  modulation car () { return m1; }
  modulation cdr () { return m2; }
};

modulation
compose (modulation m1, modulation m2) {
  if (is_identity (m1)) return m2;
  if (is_identity (m2)) return m1;
  double a11, a12, a21, a22;
  double b11, b12, b21, b22;
  m1->get_matrix (a11, a12, a21, a22);
  m2->get_matrix (b11, b12, b21, b22);
  double m11= a11 * b11 + a12 * b21;
  double m12= a11 * b12 + a12 * b22;
  double m21= a21 * b11 + a22 * b21;
  double m22= a21 * b12 + a22 * b22;
  if (is_linear (m1) && is_linear (m2))
    return linear_modulation (m11, m12, m21, m22);
  return tm_new<composed_modulation_rep> (m1, m2, m11, m12, m21, m22);
}
