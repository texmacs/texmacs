
/******************************************************************************
* MODULE     : modulation.hpp
* DESCRIPTION: font modulations
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MODULATION_H
#define MODULATION_H
#include "renderer.hpp"

/******************************************************************************
* The abstract modulation class
******************************************************************************/

enum modulation_kind {
  modulation_identity,
  modulation_linear,
  modulation_composite };

class modulation_rep;
class modulation {
ABSTRACT_NULL(modulation);
};

class modulation_rep: public abstract_struct {
public:
  modulation_kind kind;

public:
  inline modulation_rep (modulation_kind kind2): kind (kind2) {}
  inline virtual ~modulation_rep () {}

  virtual tree expression () = 0;
  virtual void get_matrix (double& a11, double& a12, double& a21, double& a22);
  virtual modulation car ();
  virtual modulation cdr ();

  friend class modulation;
};

ABSTRACT_NULL_CODE(modulation);

/******************************************************************************
* Modulations
******************************************************************************/

bool is_identity (modulation m);
bool is_zoom (modulation m);
bool is_linear (modulation m);
bool is_composite (modulation m);
double get_zoom (modulation m);
modulation car (modulation m);
modulation cdr (modulation m);

modulation identity_modulation ();
modulation resize_modulation (double zoom);
modulation scale_modulation (double scalex, double scaley);
modulation slant_modulation (double slant);
modulation compose (modulation m1, modulation m2);

#endif // defined MODULATION_H
