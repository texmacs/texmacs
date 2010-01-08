
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
#define PATCH_BRANCH       2
#define PATCH_BIRTH        3
#define PATCH_AUTHOR       4

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
  inline virtual modification get_inverse () {
    FAILED ("not a modification"); return mod_assign (path (), ""); }
  inline virtual bool get_birth () {
    FAILED ("not a birth"); return false; }
  inline virtual double get_author () {
    return -1; }
};

class patch {
ABSTRACT_NULL (patch);
  patch (modification mod, modification inv);
  patch (array<patch> a);
  patch (bool par, array<patch> a);
  patch (patch p1, patch p2);
  patch (double author, bool create);
  patch (double author, patch p);
  inline patch operator [] (int i) {
    return rep->get_child (i); }
};
ABSTRACT_NULL_CODE (patch);

inline patch patch_rep::get_child (int i) {
  FAILED ("not a composite patch"); (void) i; return patch (); }

/******************************************************************************
* Routines on patches
******************************************************************************/

int nr_children (patch p);
patch child (patch p, int i);
array<patch> children (patch p);
array<patch> children (patch p, int i, int j);
int nr_branches (patch p);
patch branch (patch p, int i);
array<patch> branches (patch p);
array<patch> branches (patch p, int i, int j);

double new_author ();
double new_marker ();
void set_author (double author);
double get_author ();

tm_ostream& operator << (tm_ostream& out, patch p);
patch copy (patch p);
patch compactify (patch p);
path cursor_hint (patch p, tree t);

inline int get_type (patch p) {
  return p->get_type (); }
inline int N (patch p) {
  return p->get_arity (); }
inline modification get_modification (patch p) {
  return p->get_modification (); }
inline modification get_inverse (patch p) {
  return p->get_inverse (); }
inline bool get_birth (patch p) {
  return p->get_birth (); }
inline double get_author (patch p) {
  return p->get_author (); }

bool is_applicable (patch p, tree t);
tree clean_apply (patch p, tree t);
void apply (patch p, tree& t);

modification invert (modification m, tree t);
bool commute (modification m1, modification m2);
bool swap (modification& m1, modification& m2);
bool join (modification& m1, modification m2, tree t);
patch invert (patch p, tree t);
bool commute (patch p1, patch p2);
bool swap (patch& p1, patch& p2);
bool join (patch& p1, patch p2, tree t);

#endif // defined PATCH_H
