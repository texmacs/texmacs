
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
  void* data;
  inline blackbox_rep (void* data2): data (data2) {}
  inline virtual ~blackbox_rep () {}
  inline virtual ostream& display (ostream& out) {
    return out << "blackbox"; }
};

class blackbox {
public:
ABSTRACT_NULL(blackbox);
};
ABSTRACT_NULL_CODE(blackbox);

inline bool operator == (blackbox bb1, blackbox bb2) {
  return bb1->data == bb2->data; }
inline bool operator != (blackbox bb1, blackbox bb2) {
  return bb1->data != bb2->data; }
inline ostream& operator << (ostream& out, blackbox bb) {
  return out << bb->display (out); }

template<class T>
class whitebox_rep: public blackbox_rep {
public:
  inline whitebox_rep (const T& data):
    blackbox_rep ((void*) new T (data)) {}
  inline ~whitebox_rep () { delete (T*) data; }
  inline ostream& display (ostream& out) {
    return out << *((T*) data); }
};

template<class T> blackbox
whitebox (const T& data) {
  return new whitebox_rep<T> (data);
}

template<class T> T&
view_as (blackbox bb) {
  return *((T*) bb->data);
}

#endif // BLACKBOX_H
