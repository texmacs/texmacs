
/******************************************************************************
* MODULE     : resource.hpp
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef RESOURCE_H
#define RESOURCE_H
#include "string.hpp"
#include "hashmap.hpp"

template<class T> struct rep {
  string res_name;
  inline rep<T> (string res_name2):res_name (res_name2) {
    T::instances (res_name)= static_cast<pointer>(this); }
  inline virtual ~rep<T> () {
    T::instances -> reset (res_name); }
};

template<class R> class resource_ptr {
protected:
  ~resource_ptr() {};
public:
  R* rep;
  static hashmap<string,pointer> instances;
  inline R* operator ->()  { return rep; }
};

#ifdef OS_WIN32
#define RESOURCE(PTR)                               \
struct PTR##_rep;                                    \
struct PTR : public resource_ptr<PTR##_rep> {       \
  inline PTR (PTR##_rep* rep2= NULL) { rep=rep2; }  \
  inline PTR (string s) { rep=(PTR##_rep*) instances [s]; } \
  inline ~PTR() {}                                  \
}
#else
#define RESOURCE(PTR)                               \
struct PTR##_rep;                                    \
template<> hashmap<string,pointer> resource_ptr<PTR##_rep>::instances; \
struct PTR : public resource_ptr<PTR##_rep> {       \
  inline PTR (PTR##_rep* rep2= NULL) { rep=rep2; }  \
  inline PTR (string s) { rep=(PTR##_rep*) instances [s]; } \
  inline ~PTR() {}                                  \
}
#endif

#ifdef OS_WIN32
#define RESOURCE_CODE(PTR) \
hashmap<string,pointer> resource_ptr<PTR##_rep>::instances (NULL); 
#else
#define RESOURCE_CODE(PTR) \
template<> hashmap<string,pointer> resource_ptr<PTR##_rep>::instances (NULL);
#endif

template<class R>
inline bool is_nil (const resource_ptr<R>& res) { return res.rep == NULL; }

template<class R>
ostream& operator << (ostream& out, const resource_ptr<R>& t);

#define make(T,s,im) ((T::instances -> contains (s))? T(s): T(im))

template<class T>
ostream& operator << (ostream& out, const resource_ptr<T>& t) {
  return out << t->res_name;
}

#endif // RESOURCE_H
