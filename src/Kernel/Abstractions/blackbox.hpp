
/******************************************************************************
* MODULE     : blackbox.hpp
* DESCRIPTION: For hiding the implementation of a type
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BLACKBOX_H
#define BLACKBOX_H
#include "basic.hpp"

class blackbox_rep: public abstract_struct {
public:
  inline blackbox_rep () {}
  inline virtual ~blackbox_rep () {}
  virtual int get_type () = 0;
  virtual bool equal (blackbox_rep* ptr) = 0;
  virtual ostream& display (ostream& out) = 0;
};

class blackbox {
public:
ABSTRACT_NULL(blackbox);
};
ABSTRACT_NULL_CODE(blackbox);

template<class T>
class whitebox_rep: public blackbox_rep {
public:
  T data;
public:
  inline whitebox_rep (const T& data2): data (data2) {}
  inline ~whitebox_rep () {}
  inline int get_type () { return type_helper<T>::id; }
  inline bool equal (blackbox_rep* ptr) {
    return ptr != NULL && ptr->get_type () == type_helper<T>::id &&
           ((whitebox_rep<T>*) ptr)->data == data; }
  inline ostream& display (ostream& out) { return out << data; }
};

inline bool operator == (blackbox bb1, blackbox bb2) {
  if (is_nil (bb1)) return is_nil (bb2);
  else return bb1->equal (bb2.rep); }
inline bool operator != (blackbox bb1, blackbox bb2) {
  if (is_nil (bb1)) return !is_nil (bb2);
  else return !bb1->equal (bb2.rep); }
inline ostream& operator << (ostream& out, blackbox bb) {
  if (is_nil (bb)) return out << "nil";
  else return bb->display (out); }

inline int
type_box (blackbox bb) {
  return is_nil (bb)? 0: bb->get_type ();
}

template<class T> blackbox
close_box (const T& data) {
  return tm_new<whitebox_rep<T> > (data);
}

template<class T> T
open_box (blackbox bb) {
  ASSERT (type_box (bb) == type_helper<T>::id, "type mismatch");
  return ((whitebox_rep<T>*) bb.rep) -> data;
}

#endif // BLACKBOX_H
