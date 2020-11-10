
/******************************************************************************
* MODULE     : formatter.hpp
* DESCRIPTION: formatting trees in a lazy way.
*              extra formatting done is by calling 'produce'
*              with new formatting information as a parameter
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FORMATTER_H
#define FORMATTER_H
#include "boxes.hpp"
#include "env.hpp"

enum format_type {
  FORMAT_NONE, FORMAT_CELL, FORMAT_WIDTH, FORMAT_VSTREAM,
  QUERY_WIDTH, QUERY_VSTREAM_WIDTH
};

enum lazy_type {
  LAZY_DOCUMENT, LAZY_PARAGRAPH, LAZY_SURROUND,
  LAZY_CANVAS, LAZY_ORNAMENT, LAZY_ART_BOX,
  LAZY_CONCAT, LAZY_TABLE, LAZY_ATOM,
  LAZY_BOX, LAZY_HSTREAM, LAZY_HDONE, LAZY_VSTREAM, LAZY_VDONE, LAZY_PAGES
};

/******************************************************************************
* The format class
******************************************************************************/

extern int format_count;
struct format;
struct format_rep: public abstract_struct {
  format_type type;

  inline  format_rep (format_type t): type (t) {
    TM_DEBUG (format_count++); }
  inline  virtual ~format_rep () {
    TM_DEBUG (format_count--); }

  virtual operator tree () = 0;
  virtual bool equal (format fm) = 0;
};

struct format {
  ABSTRACT_NULL(format);            
  inline operator tree () { return (tree) (*rep); }
  inline bool operator == (format fm) { return rep->equal (fm); }
  inline bool operator != (format fm) { return !rep->equal (fm); }
};
ABSTRACT_NULL_CODE(format);

format make_format_none ();
format make_format_cell (SI width, int vpos, SI depth, SI height);
format make_format_width (SI width);

/******************************************************************************
* The lazy class
******************************************************************************/

extern int lazy_count;
struct lazy;
struct lazy_rep: public abstract_struct {
  lazy_type type;  // the lazy type
  path ip;         // source location

  inline  lazy_rep (lazy_type type2, path ip2): type (type2), ip (ip2) {
    TM_DEBUG (lazy_count++); }
  inline  virtual ~lazy_rep () {
    TM_DEBUG (lazy_count--); }

  virtual operator tree () = 0;
  virtual void append (lazy lz);
  virtual lazy produce (lazy_type request, format fm);
    // further format the lazy structure using 'fm',
    // and produce a lazy structure of type 'request'
  virtual format query (lazy_type request, format fm);
    // retrieve formatting information as a function of 'fm'
    // before production of a lazy structure of type 'request'
  virtual void propagate ();
    // hack to propagate environment properties such as 'no_indent_after'
};

struct lazy {
  ABSTRACT_NULL(lazy);            
  operator box ();
  inline operator tree () { return (tree) (*rep); }
  inline void operator << (lazy lz) { rep->append (lz); }
  inline bool operator == (lazy lz) { return rep == lz.rep; }
  inline bool operator != (lazy lz) { return rep != lz.rep; }
  inline friend tm_ostream& operator << (tm_ostream& out, lazy lz) {
    return out << ((tree) lz); }
};
ABSTRACT_NULL_CODE(lazy);
lazy make_lazy (edit_env env, tree t, path ip);
lazy make_lazy_box (box b);

#endif // defined FORMATTER_H
