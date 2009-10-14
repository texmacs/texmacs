
/******************************************************************************
* MODULE     : ntuple.hpp
* DESCRIPTION: Pairs, triples and quartets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

template<class T1, class T2> int
hash (const pair<T1,T2>& p) {
  int h1= hash (p.x1);
  return (h1 << 11) ^ (h1 >> 21) ^ hash (p.x2); }

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

template<class T1, class T2, class T3> int
hash (const triple<T1,T2,T3>& t) {
  int h= hash (t.x1);
  h= (h << 11) ^ (h >> 21) ^ hash (t.x2);
  return (h << 11) ^ (h >> 21) ^ hash (t.x3); }

template<class T1, class T2, class T3> inline ostream&
operator << (ostream& out, const triple<T1,T2,T3>& t) {
  return out << "[ " << t.x1 << ", " << t.x2 << ", " << t.x3 << " ]"; }

template<class T1, class T2, class T3, class T4>
class quartet {
public:
  T1 x1; T2 x2; T3 x3; T4 x4;
  inline quartet (const quartet& q):
    x1 (q.x1), x2 (q.x2), x3 (q.x3), x4 (q.x4) {}
  inline quartet (const T1& y1, const T2& y2, const T3& y3, const T3& y4):
    x1 (y1), x2 (y2), x3 (y3), x4 (y4) {}
  inline quartet& operator = (const quartet& q) {
    x1= q.x1; x2= q.x2; x3= q.x3; x4= q.x4; return *this; }
  inline bool operator == (const quartet& q) {
    return x1 == q.x1 && x2 == q.x2 && x3 == q.x3 && x4 == q.x4; }
  inline bool operator != (const quartet& q) {
    return x1 != q.x1 || x2 != q.x2 || x3 != q.x3 || x4 != q.x4; }
};

template<class T1, class T2, class T3, class T4> int
hash (const quartet<T1,T2,T3,T4>& q) {
  int h= hash (q.x1);
  h= (h << 11) ^ (h >> 21) ^ hash (q.x2);
  h= (h << 11) ^ (h >> 21) ^ hash (q.x3);
  return (h << 11) ^ (h >> 21) ^ hash (q.x4); }

template<class T1, class T2, class T3, class T4> inline ostream&
operator << (ostream& out, const quartet<T1,T2,T3,T4>& q) {
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

template<class T1, class T2, class T3, class T4, class T5> int
hash (const quintuple<T1,T2,T3,T4,T5>& q) {
  int h= hash (q.x1);
  h= (h << 11) ^ (h >> 21) ^ hash (q.x2);
  h= (h << 11) ^ (h >> 21) ^ hash (q.x3);
  h= (h << 11) ^ (h >> 21) ^ hash (q.x4);
  return (h << 11) ^ (h >> 21) ^ hash (q.x5); }

template<class T1, class T2, class T3, class T4, class T5> inline ostream&
operator << (ostream& out, const quintuple<T1,T2,T3,T4,T5>& q) {
  return out << "[ " << q.x1 << ", " << q.x2 << ", " << q.x3
	     << ", " << q.x4 << ", " << q.x5 << " ]"; }

#endif // NTUPLE_H
