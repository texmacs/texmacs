
/******************************************************************************
* MODULE     : patch.cpp
* DESCRIPTION: Routines on patches
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "patch.hpp"

/******************************************************************************
* Concrete patches
******************************************************************************/

class modification_patch_rep: public patch_rep {
  modification mod;
public:
  inline modification_patch_rep (modification mod2): mod (mod2) {}
  inline int get_type () { return PATCH_MODIFICATION; }
  inline modification get_modification () { return mod; }
};

class compound_patch_rep: public patch_rep {
  array<patch> a;
public:
  inline compound_patch_rep (array<patch> a2): a (a2) {}
  inline int get_type () { return PATCH_COMPOUND; }
  inline int get_arity () { return N(a); }
  inline patch get_child (int i) { return a[i]; }
};

class birth_patch_rep: public patch_rep {
  double actor;
  bool birth;
public:
  inline birth_patch_rep (double a2, bool b2): actor (a2), birth (b2) {}
  inline int get_type () { return PATCH_BIRTH; }
  inline double get_actor () { return actor; }
  inline bool get_birth () { return birth; }
};

class actor_patch_rep: public patch_rep {
  double actor;
  patch p;
public:
  inline actor_patch_rep (double a2, patch p2): actor (a2), p (p2) {}
  inline int get_type () { return PATCH_ACTOR; }
  inline int get_arity () { return 1; }
  inline patch get_child (int i) {
    ASSERT (i == 0, "out of range");
    return p; }
  inline double get_actor () { return actor; }
};

patch::patch (modification mod):
  rep (tm_new<modification_patch_rep> (mod)) {}
patch::patch (array<patch> a):
  rep (tm_new<compound_patch_rep> (a)) {}
patch::patch (double actor, bool create):
  rep (tm_new<birth_patch_rep> (actor, create)) {}
patch::patch (double actor, patch p):
  rep (tm_new<actor_patch_rep> (actor, p)) {}

/******************************************************************************
* Common routines
******************************************************************************/

ostream&
operator << (ostream& out, patch p) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    out << get_modification (p);
    break;
  case PATCH_COMPOUND:
    out << "Composite" << INDENT;
    for (int i=0; i<N(p); i++)
      out << LF << p[i];
    out << UNINDENT;
    break;
  case PATCH_BIRTH:
    if (get_birth (p)) out << "Birth ";
    else out << "Death ";
    out << get_actor (p);
    break;
  case PATCH_ACTOR:
    out << "Actor " << get_actor (p) << INDENT << LF;
    out << p[0];
    out << UNINDENT;
    break;
  default:
    FAILED ("unsupported patch type");
  }
  return out;
}

/******************************************************************************
* Patch application
******************************************************************************/

bool
is_applicable (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return is_applicable (t, get_modification (p));
  case PATCH_BIRTH:
    return true;
  case PATCH_COMPOUND:
  case PATCH_ACTOR:
    for (int i=0; i<N(p); i++) {
      if (!is_applicable (p[i], t)) return false;
      t= clean_apply (p[i], t);
    }
    return true;
  default:
    FAILED ("unsupported patch type");
    return false;
  }
}

tree
clean_apply (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return clean_apply (t, get_modification (p));
  case PATCH_BIRTH:
    return t;
  case PATCH_COMPOUND:
  case PATCH_ACTOR:
    for (int i=0; i<N(p); i++)
      t= clean_apply (p[i], t);
    return t;
  default:
    FAILED ("unsupported patch type");
    return t;
  }
}

void
apply (patch p, tree& t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    apply (t, get_modification (p));
    break;
  case PATCH_BIRTH:
    break;
  case PATCH_COMPOUND:
  case PATCH_ACTOR:
    for (int i=0; i<N(p); i++)
      apply (p[i], t);
    break;
  default:
    FAILED ("unsupported patch type");
  }
}

/******************************************************************************
* Patch inversion
******************************************************************************/

patch
invert (patch p, tree t) {
  switch (get_type (p)) {
  case PATCH_MODIFICATION:
    return patch (invert (get_modification (p), t));
  case PATCH_BIRTH:
    return p;
  case PATCH_COMPOUND:
    {
      int i, n=N(p);
      array<patch> r(n);
      for (i=0; i<n; i++) {
	r[n-1-i]= invert (p[i], t);
	t= clean_apply (p[i], t);
      }
      return patch (r);
    }
  case PATCH_ACTOR:
    return patch (get_actor (p), invert (p[0], t));
  default:
    FAILED ("unsupported patch type");
    return patch ();
  }
}

/******************************************************************************
* Commutation of patches
******************************************************************************/

static bool
swap_basic (patch& p1, patch& p2) {
  patch aux= p1;
  p1= p2;
  p2= aux;
  return true;
}

bool
swap (patch& p1, patch& p2) {
  if (get_type (p1) == PATCH_COMPOUND) {
    int n= N(p1);
    array<patch> a (n);
    for (int i=0; i<n; i++) a[i]= p1[i];
    for (int i=n-1; i>=0; i--) {
      if (!swap (a[i], p2)) return false;
      swap_basic (a[i], p2);
    }
    p1= p2;
    p2= patch (a);
    return true;
  }
  if (get_type (p2) == PATCH_COMPOUND) {
    int n= N(p2);
    array<patch> a (n);
    for (int i=0; i<n; i++) a[i]= p2[i];
    for (int i=0; i<n; i++) {
      if (!swap (p1, a[i])) return false;
      swap_basic (p1, a[i]);
    }
    p2= p1;
    p1= patch (a);
    return true;
  }
  if (get_type (p1) == PATCH_ACTOR) {
    patch s= p1[0];
    bool r= swap (s, p2);
    p2= patch (get_actor (p1), p2);
    p1= s;
    return r;
  }
  if (get_type (p2) == PATCH_ACTOR) {
    patch s= p2[0];
    bool r= swap (p1, s);
    p1= patch (get_actor (p2), p1);
    p2= s;
    return r;
  }
  if (get_type (p1) == PATCH_BIRTH ||
      get_type (p2) == PATCH_BIRTH)
    return swap_basic (p1, p2);
  if (get_type (p1) == PATCH_MODIFICATION &&
      get_type (p2) == PATCH_MODIFICATION)
    {
      modification m1= get_modification (p1);
      modification m2= get_modification (p2);
      bool r= swap (m1, m2);
      p1= patch (m1);
      p2= patch (m2);
      return r;
    }
  FAILED ("invalid situaltion");
  return false;
}

bool
commute (patch p1, patch p2) {
  patch s1= p1;
  patch s2= p2;
  return swap (s1, s2);
}
