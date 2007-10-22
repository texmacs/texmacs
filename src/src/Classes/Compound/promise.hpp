
/******************************************************************************
* MODULE     : promise.hpp
* DESCRIPTION: promises
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PROMISE_H
#define PROMISE_H
#include "tree.hpp"

class tree;
template<class T> class promise_rep;
template<class T> class promise;
template<class T> ostream& operator << (ostream& out, promise<T> cmd);

template<class T>
class promise_rep: public abstract_struct {
public:
  inline promise_rep () {}
  inline virtual ~promise_rep () {}
  inline virtual ostream& print (ostream& out);
  virtual T eval () = 0;
};

template<class T>
class promise {
public:
ABSTRACT_NULL_TEMPLATE(promise,T);
  inline T operator () ();
  friend ostream& operator << LESSGTR (ostream& out, promise<T> cmd);
};
ABSTRACT_NULL_TEMPLATE_CODE(promise,class,T);

#define TMPL template<class T>
TMPL inline ostream& promise_rep<T>::print (ostream& out) {
  return out << "promise"; }
TMPL inline T promise<T>::operator () () {
  return rep->eval (); }
TMPL inline bool operator == (promise<T> mw1, promise<T> mw2) {
  return mw1.rep == mw2.rep; }
TMPL inline ostream& operator << (ostream& out, promise<T> cmd) {
  if (nil (cmd)) return out << "(null)"; else return cmd->print(out); }
#undef TMPL

#endif // defined PROMISE_H
