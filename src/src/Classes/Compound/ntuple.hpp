
/******************************************************************************
* MODULE     : ntuple.hpp
* DESCRIPTION: Pairs, tuples and quadruples
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef NTUPLE_H
#define NTUPLE_H
#include "basic.hpp"

template<class T1, class T2>
class pair {
public:
  T1 x1; T2 x2;
  inline pair (const pair& p): x1 (p.x1), x2 (p.x2) {}
  inline pair (const T1& y1, const T2& y2): x1 (y1), x2 (y2) {}
  inline pair& operator = (const pair& p) { x1= p.x1; x2= p.x2; return *this; }
  inline bool operator == (const pair& p) { return x1 == p.x1 && x2 == p.x2; }
  inline bool operator != (const pair& p) { return x1 != p.x1 || x2 != p.x2; }
};

template<class T1, class T2> inline ostream&
operator << (ostream& out, const pair<T1,T2>& p) {
  return out << "[ " << p.x1 << ", " << p.x2 << " ]"; }

template<class T1, class T2, class T3>
class triple {
public:
  T1 x1; T2 x2; T3 x3;
  inline triple (const triple& t):
    x1 (t.x1), x2 (t.x2), x3 (t.x3) {}
  inline triple (const T1& y1, const T2& y2, const T3& y3):
    x1 (y1), x2 (y2), x3 (y3) {}
  inline triple& operator = (const triple& t) {
    x1= t.x1; x2= t.x2; x3= t.x3; return *this; }
  inline bool operator == (const triple& t) {
    return x1 == t.x1 && x2 == t.x2 && x3 == t.x3; }
  inline bool operator != (const triple& t) {
    return x1 != t.x1 || x2 != t.x2 || x3 != t.x3; }
};

template<class T1, class T2, class T3> inline ostream&
operator << (ostream& out, const triple<T1,T2,T3>& t) {
  return out << "[ " << t.x1 << ", " << t.x2 << ", " << t.x3 << " ]"; }

template<class T1, class T2, class T3, class T4>
class quadruple {
public:
  T1 x1; T2 x2; T3 x3; T4 x4;
  inline quadruple (const quadruple& q):
    x1 (q.x1), x2 (q.x2), x3 (q.x3), x4 (q.x4) {}
  inline quadruple (const T1& y1, const T2& y2, const T3& y3, const T3& y4):
    x1 (y1), x2 (y2), x3 (y3), x4 (y4) {}
  inline quadruple& operator = (const quadruple& q) {
    x1= q.x1; x2= q.x2; x3= q.x3; x4= q.x4; return *this; }
  inline bool operator == (const quadruple& q) {
    return x1 == q.x1 && x2 == q.x2 && x3 == q.x3 && x4 == q.x4; }
  inline bool operator != (const quadruple& q) {
    return x1 != q.x1 || x2 != q.x2 || x3 != q.x3 || x4 != q.x4; }
};

template<class T1, class T2, class T3, class T4> inline ostream&
operator << (ostream& out, const quadruple<T1,T2,T3,T4>& q) {
  return out << "[ " << q.x1 << ", " << q.x2
	     << ", " << q.x3 << ", " << q.x4 << " ]"; }

template<class T1, class T2, class T3, class T4, class T5>
class quintuple {
public:
  T1 x1; T2 x2; T3 x3; T4 x4; T5 x5;
  inline quintuple (const quintuple& q):
    x1 (q.x1), x2 (q.x2), x3 (q.x3), x4 (q.x4), x5 (q.x5) {}
  inline quintuple (const T1& y1, const T2& y2, const T3& y3,
		    const T3& y4, const T5& y5):
    x1 (y1), x2 (y2), x3 (y3), x4 (y4), x5 (y5) {}
  inline quintuple& operator = (const quintuple& q) {
    x1= q.x1; x2= q.x2; x3= q.x3; x4= q.x4; x5= q.x5; return *this; }
  inline bool operator == (const quintuple& q) {
    return x1 == q.x1 && x2 == q.x2 && x3 == q.x3 &&
           x4 == q.x4 && x5 == q.x5; }
  inline bool operator != (const quintuple& q) {
    return x1 != q.x1 || x2 != q.x2 || x3 != q.x3 ||
           x4 != q.x4 || x5 != q.x5; }
};

template<class T1, class T2, class T3, class T4, class T5> inline ostream&
operator << (ostream& out, const quintuple<T1,T2,T3,T4,T5>& q) {
  return out << "[ " << q.x1 << ", " << q.x2 << ", " << q.x3
	     << ", " << q.x4 << ", " << q.x5 << " ]"; }

#endif // NTUPLE_H
