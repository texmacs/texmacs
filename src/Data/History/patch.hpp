
/******************************************************************************
* MODULE     : patch.hpp
* DESCRIPTION: Abstract patches
* COPYRIGHT  : (C) 2009  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PATCH_H
#define PATCH_H
#include "modification.hpp"

#define PATCH_MODIFICATION 0
#define PATCH_COMPOUND     1
#define PATCH_BIRTH        2
#define PATCH_ACTOR        3

/******************************************************************************
* Abstract patches
******************************************************************************/

class patch;
class patch_rep: public abstract_struct {
public:
  inline patch_rep () {}
  inline virtual ~patch_rep () {}
  virtual int get_type () = 0;
  inline virtual int get_arity () {
    return 0; }
  inline virtual patch get_child (int i);
  inline virtual modification get_modification () {
    FAILED ("not a modification"); return mod_assign (path (), ""); }
  inline virtual bool get_birth () {
    FAILED ("not a birth"); return false; }
  inline virtual double get_actor () {
    return -1; }
};

class patch {
ABSTRACT_NULL (patch);
  patch (modification mod);
  patch (array<patch> a);
  patch (double actor, bool create);
  patch (double actor, patch p);
  inline patch operator [] (int i) {
    return rep->get_child (i); }
};
ABSTRACT_NULL_CODE (patch);

inline patch patch_rep::get_child (int i) {
  FAILED ("not a composite patch"); (void) i; return patch (); }
ostream& operator << (ostream& out, patch p);

/******************************************************************************
* Routines on patches
******************************************************************************/

inline int get_type (patch p) {
  return p->get_type (); }
inline int N (patch p) {
  return p->get_arity (); }
inline modification get_modification (patch p) {
  return p->get_modification (); }
inline bool get_birth (patch p) {
  return p->get_birth (); }
inline double get_actor (patch p) {
  return p->get_actor (); }

bool is_applicable (patch p, tree t);
tree clean_apply (patch p, tree t);
void apply (patch p, tree& t);
patch invert (patch p, tree t);

bool commute (patch p1, patch p2);
void swap (patch& p1, patch& p2);
patch operator << (patch& p1, patch p2);

#endif // defined PATCH_H
