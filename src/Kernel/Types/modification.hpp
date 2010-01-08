
/******************************************************************************
* MODULE     : modification.hpp
* DESCRIPTION: elementary tree modifications
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MODIFICATION_H
#define MODIFICATION_H
#include "tree.hpp"
#include "path.hpp"

/******************************************************************************
* Elementary tree modifications
******************************************************************************/

#define MOD_ASSIGN       1
#define MOD_INSERT       2
#define MOD_REMOVE       3
#define MOD_SPLIT        4
#define MOD_JOIN         5
#define MOD_ASSIGN_NODE  6
#define MOD_INSERT_NODE  7
#define MOD_REMOVE_NODE  8

/******************************************************************************
* The modification class
******************************************************************************/

typedef int modification_type;
class modification;

class modification_rep: concrete_struct {
public:
  modification_type k;
  path p;
  tree t;

public:
  inline modification_rep (modification_type k2, path p2):
    k (k2), p (p2) {}
  inline modification_rep (modification_type k2, path p2, tree t2):
    k (k2), p (p2), t (t2) {}
  inline ~modification_rep () {}
  friend class modification;
};

class modification {
CONCRETE(modification);
  inline modification (modification_type k, path p):
    rep (tm_new<modification_rep> (k, p)) {}
  inline modification (modification_type k, path p, tree t):
    rep (tm_new<modification_rep> (k, p, t)) {}
};
CONCRETE_CODE(modification);

bool operator == (modification m1, modification m2);
bool operator != (modification m1, modification m2);
tm_ostream& operator << (tm_ostream& out, modification mod);

/******************************************************************************
* Constructors and accessors
******************************************************************************/

inline modification mod_assign (path p, tree t) {
  return modification (MOD_ASSIGN, p, t); }
inline modification mod_insert (path p, int pos, tree t) {
  return modification (MOD_INSERT, p * pos, t); }
inline modification mod_remove (path p, int pos, int nr) {
  return modification (MOD_REMOVE, p * path (pos, nr)); }
inline modification mod_split (path p, int pos, int at) {
  return modification (MOD_SPLIT, p * path (pos, at)); }
inline modification mod_join (path p, int pos) {
  return modification (MOD_JOIN, p * pos); }
inline modification mod_assign_node (path p, tree_label lab) {
  return modification (MOD_ASSIGN_NODE, p, tree (lab)); }
inline modification mod_insert_node (path p, int pos, tree t) {
  return modification (MOD_INSERT_NODE, p * pos, t); }
inline modification mod_remove_node (path p, int pos) {
  return modification (MOD_REMOVE_NODE, p * pos); }
inline modification operator * (int i, modification mod) {
  return modification (mod->k, path (i, mod->p), mod->t); }
inline modification operator * (path p, modification mod) {
  return modification (mod->k, p * mod->p, mod->t); }
inline modification operator * (modification mod, int i) {
  return modification (mod->k, mod->p * i, mod->t); }
inline modification operator / (modification mod, path p) {
  return modification (mod->k, mod->p / p, mod->t); }
inline modification copy (modification mod) {
  return modification (mod->k, copy (mod->p), copy (mod->t)); }

path root (modification mod);
int index (modification mod);
int argument (modification mod);
tree_label L (modification mod);

/******************************************************************************
* Further routines on modifications
******************************************************************************/

bool is_applicable (tree t, modification mod);
tree clean_apply (tree t, modification mod);
void raw_apply (tree& t, modification mod);      // in observer.cpp
void apply (tree& t, modification mod);          // in observer.cpp

/******************************************************************************
* Hooks
******************************************************************************/

void edit_announce (editor_rep* ed, modification mod);
void edit_done (editor_rep* ed, modification mod);
void archive_announce (archiver_rep* buf, modification mod);
void link_announce (observer obs, modification mod);

#endif // defined MODIFICATION_H
