
/******************************************************************************
* MODULE     : raster_operators.hpp
* DESCRIPTION: Templates for several raster operations
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_OPERATORS_HPP
#define RASTER_OPERATORS_HPP
#include "picture.hpp"
#include "operators.hpp"

/******************************************************************************
* Default implementations
******************************************************************************/

template<typename C> inline void clear (C& x) { x= 0.0; }
template<typename C> inline void clear_alpha (C& x) { x= 0.0; }

template<typename C> inline C mul_alpha (const C& x) { return x; }
template<typename C> inline C div_alpha (const C& x) { return x; }
template<typename C> inline C alpha_distance (const C& x, const C& y) {
  return 2.0 * fabs (y - x); }
template<typename C> inline C copy_alpha (const C& x, const C& y) {
  (void) x; return y; }
template<typename C> inline C apply_alpha (const C& x, const C& y) {
  return x * y; }

template<typename C> inline C
mix (const C& c1, double a1, const C& c2, double a2) {
  return a1 * c1 + a2 * c2; }

template<typename C> inline C
mix (const C& c1, double a1, const C& c2, double a2,
     const C& c3, double a3, const C& c4, double a4) {
  return a1 * c1 + a2 * c2 + a3 * c3 + a4 * c4; }

/******************************************************************************
* Nullary operators
******************************************************************************/

struct clear_op {
  template<typename C> static inline void
  set_op (C& x) { clear (x); }
};

struct clear_alpha_op {
  template<typename C> static inline void
  set_op (C& x) { clear_alpha (x); }
};

/******************************************************************************
* Unary operators
******************************************************************************/

template<typename Op, typename C>
struct unary_return_type_helper {
  typedef C RET;
};

#define Unary_return_type(Op,C) \
  typename unary_return_type_helper<Op,C>::RET

struct mul_alpha_op {
  template<typename C> static inline C
  op (const C& x) { return mul_alpha (x); }
};

struct div_alpha_op {
  template<typename C> static inline C
  op (const C& x) { return div_alpha (x); }
};

struct normalize_op {
  template<typename C> static inline C
  op (const C& x) { return normalize (x); }
};

struct get_alpha_op {
  template<typename C> static inline typename C::scalar_type
  op (const C& x) { return get_alpha (x); }
};

template<typename C>
struct unary_return_type_helper<get_alpha_op,C> {
  typedef typename C::scalar_type RET;
};

/******************************************************************************
* Binary operators
******************************************************************************/

struct hypot_op {
  template<typename C> static inline C
  op (const C& x, const C& y) { return hypot (x, y); }
};

struct min_op {
  template<typename C> static inline C
  op (const C& x, const C& y) { return min (x, y); }
};

struct max_op {
  template<typename C> static inline C
  op (const C& x, const C& y) { return max (x, y); }
};

struct copy_alpha_op {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return copy_alpha (x, y); }
};

struct apply_alpha_op {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return apply_alpha (x, y); }
};

/******************************************************************************
* Composition operators
******************************************************************************/

template<composition_mode M>
struct composition_op {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { (void) y; return x; }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { (void) x; (void) y; }
};

template<>
struct composition_op<compose_source> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { (void) x; return y; }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= y; }
};

template<>
struct composition_op<compose_source_over> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return source_over (x, y); }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= source_over (x, y); }
};

template<>
struct composition_op<compose_towards_source> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return towards_source (x, y); }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= towards_source (x, y); }
};

template<>
struct composition_op<compose_alpha_distance> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return alpha_distance (x, y); }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= alpha_distance (x, y); }
};

template<>
struct composition_op<compose_add> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return x + y; }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x += y; }
};

template<>
struct composition_op<compose_sub> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return x - y; }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x -= y; }
};

template<>
struct composition_op<compose_mul> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return x * y; }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x *= y; }
};

template<>
struct composition_op<compose_min> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return min (x, y); }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= min (x, y); }
};

template<>
struct composition_op<compose_max> {
  template<typename C, typename S> static inline C
  op (const C& x, const S& y) { return max (x, y); }
  template<typename C, typename S> static inline void
  set_op (C& x, const S& y) { x= max (x, y); }
};

typedef composition_op<compose_source> source_op;
typedef composition_op<compose_source_over> source_over_op;
typedef composition_op<compose_towards_source> towards_source_op;
typedef composition_op<compose_alpha_distance> alpha_distance_op;

#endif // RASTER_OPERATORS_H
