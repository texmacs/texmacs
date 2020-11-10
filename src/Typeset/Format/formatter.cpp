
/******************************************************************************
* MODULE     : formatter.cpp
* DESCRIPTION: formatting trees in a lazy way.
*              extra formatting done is by calling 'produce'
*              with new formatting information as a parameter
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "formatter.hpp"
#include "Format/format.hpp"

int format_count= 0;
int lazy_count= 0;

/******************************************************************************
* Standard formatting types
******************************************************************************/

format
make_format_none () {
  return tm_new<format_none_rep> ();
}

format
make_format_width (SI width) {
  return tm_new<format_width_rep> (width);
}

format
make_format_cell (SI width, int vpos, SI depth, SI height) {
  return tm_new<format_cell_rep> (width, vpos, depth, height);
}

format
make_format_vstream (SI w, array<line_item> bef, array<line_item> aft) {
  return tm_new<format_vstream_rep> (w, bef, aft);
}

/*
format
make_query_width () {
  return tm_new<format_none_rep> (QUERY_WIDTH);
}
*/

format
make_query_vstream_width (array<line_item> bef, array<line_item> aft) {
  return tm_new<query_vstream_width_rep> (bef, aft);
}

/******************************************************************************
* The lazy class
******************************************************************************/

lazy
lazy_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;

  if ((request == LAZY_BOX) && (fm->type == FORMAT_CELL)) {
    format_cell fc= (format_cell) fm;
    lazy tmp= produce (LAZY_VSTREAM, make_format_vstream (fc->width, 0, 0));
    return tmp->produce (request, fm);
  }

  if ((request == LAZY_BOX) && (fm->type == FORMAT_WIDTH)) {
    format_width fw= (format_width) fm;
    lazy tmp= produce (LAZY_VSTREAM, make_format_vstream (fw->width, 0, 0));
    return tmp->produce (request, fm);
  }

  failed_error << "The lazy structure was " << ((tree) (*this)) << "\n";
  failed_error << "The format was " << ((tree) fm) << "\n";
  FAILED ("invalid production");
  return lazy ();
}

format
lazy_rep::query (lazy_type request, format fm) {
  (void) request;
  failed_error << "The lazy structure was " << ((tree) (*this)) << "\n";
  failed_error << "The format was " << ((tree) fm) << "\n";
  FAILED ("invalid query");
  return format ();
}

void
lazy_rep::propagate () {
}

void
lazy_rep::append (lazy lz) {
  (void) lz;
  failed_error << "The lazy structure was " << ((tree) (*this)) << "\n";
  FAILED ("lazy structure is not a stream");
}

struct lazy_box_rep: public lazy_rep {
  box b;
  lazy_box_rep (box b2): lazy_rep (LAZY_BOX, b2->ip), b (b2) {}
  operator tree () { return (tree) b; }
};

struct lazy_box {
  EXTEND_NULL(lazy,lazy_box);
};
EXTEND_NULL_CODE(lazy,lazy_box);

lazy::operator box () {
  lazy lz= rep->produce (LAZY_BOX, make_format_none ());
  lazy_box lb= (lazy_box) lz;
  return lb->b;
}

lazy
make_lazy_box (box b) {
  return tm_new<lazy_box_rep> (b);
}
