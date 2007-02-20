
/******************************************************************************
* MODULE     : lazy_typeset.hpp
* DESCRIPTION: Lazy typesetting of various primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LAZY_TYPESET_H
#define LAZY_TYPESET_H
#include "formatter.hpp"
#include "Format/line_item.hpp"

struct lazy_document_rep: public lazy_rep {
  array<lazy> par;   // the paragraphs

  lazy_document_rep (edit_env env, tree t, path ip);
  inline operator tree () { return "Document"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_document {
  EXTEND_NULL(lazy,lazy_document);
  inline lazy_document (edit_env env, tree t, path ip):
    rep (new lazy_document_rep (env, t, ip)) { rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_document);

struct lazy_surround_rep: public lazy_rep {
  array<line_item> a;    // left surrounding
  array<line_item> b;    // right surrounding
  lazy             par;  // the surrounded paragraph

  lazy_surround_rep (edit_env env, tree t, path ip);
  lazy_surround_rep (array<line_item> a, array<line_item> b, lazy p, path ip);
  inline operator tree () { return "Surround"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_surround {
  EXTEND_NULL(lazy,lazy_surround);
  inline lazy_surround (edit_env env, tree t, path ip):
    rep (new lazy_surround_rep (env, t, ip)) { rep->ref_count= 1; }
  inline lazy_surround (array<line_item> a, array<line_item> b,
			lazy par, path ip):
    rep (new lazy_surround_rep (a, b, par, ip)) { rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_surround);

#endif // defined LAZY_TYPESET_H
