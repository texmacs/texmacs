
/******************************************************************************
* MODULE     : blackbox.hpp
* DESCRIPTION: For hiding the implementation of a type
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  return new whitebox_rep<T> (data);
}

template<class T> T
open_box (blackbox bb) {
  if (type_box (bb) != type_helper<T>::id)
    fatal_error ("type mismatch", "open_box");
  return ((whitebox_rep<T>*) bb.rep) -> data;
}

#endif // BLACKBOX_H
