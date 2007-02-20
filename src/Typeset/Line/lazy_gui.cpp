
/******************************************************************************
* MODULE     : lazy_gui.cpp
* DESCRIPTION: Lazy typesetting of GUI primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Line/lazy_typeset.hpp"
#include "Line/lazy_vstream.hpp"
#include "Format/format.hpp"
#include "Stack/stacker.hpp"
#include "Boxes/construct.hpp"
#include "analyze.hpp"

void get_canvas_horizontal (edit_env env, tree attrs, SI bx1, SI bx2,
			    SI& x1, SI& x2, SI& scx);
void get_canvas_vertical   (edit_env env, tree attrs, SI by1, SI by2,
			    SI& y1, SI& y2, SI& scy);
box put_scroll_bars (edit_env env, box b, path ip, tree attrs,
		     box inner, tree xt, tree yt, SI scx, SI scy);

/******************************************************************************
* Canvases
******************************************************************************/

struct lazy_canvas_rep: public lazy_rep {
  edit_env env;  // "current" environment
  tree xt;       // reference to the tree with the horizontal scroll
  tree yt;       // reference to the tree with the vertical scroll
  tree attrs;    // canvas dimensions and scrolling
  lazy par;      // the canvas body

  lazy_canvas_rep (edit_env env, tree xt, tree yt, tree a, lazy p, path ip);
  inline operator tree () { return "Canvas"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_canvas {
EXTEND_NULL(lazy,lazy_canvas);
  inline lazy_canvas (edit_env env, tree xt, tree yt,
		      tree attrs, lazy par, path ip):
    rep (new lazy_canvas_rep (env, xt, yt, attrs, par, ip)) {
      rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_canvas);

lazy_canvas_rep::lazy_canvas_rep (
  edit_env env2, tree xt2, tree yt2, tree attrs2, lazy par2, path ip):
    lazy_rep (LAZY_CANVAS, ip),
    env (env2), xt (xt2), yt (yt2), attrs (attrs2), par (par2) {}

format
lazy_canvas_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    format body_fm= par->query (request, fm);
    format_width fmw= (format_width) body_fm;
    SI width= fmw->width;
    tree old1= env->local_begin (PAGE_MEDIUM, "papyrus");
    tree old2= env->local_begin (PAR_LEFT, "0tmpt");
    tree old3= env->local_begin (PAR_RIGHT, "0tmpt");
    tree old4= env->local_begin (PAR_MODE, "justify");
    tree old5= env->local_begin (PAR_NO_FIRST, "true");
    tree old6= env->local_begin (PAR_WIDTH, tree (TMLEN, as_string (width)));
    SI x1, x2, scx;
    get_canvas_horizontal (env, attrs, 0, fmw->width, x1, x2, scx);
    env->local_end (PAR_WIDTH, old6);
    env->local_end (PAR_NO_FIRST, old5);
    env->local_end (PAR_MODE, old4);
    env->local_end (PAR_RIGHT, old3);
    env->local_end (PAR_LEFT, old2);
    env->local_end (PAGE_MEDIUM, old1);
    return make_format_width (x2 - x1);
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_canvas_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM || request == LAZY_BOX) {
    SI delta= 0;
    if (N(attrs) > 6) {
      SI w  = env->as_length (attrs[6]);
      SI pad= env->as_length (attrs[7]);
      SI bor= 6*env->get_int (SFACTOR) * PIXEL;
      if (is_atomic (attrs[8]))
	if (ends (attrs[8]->label, "w") || ends (attrs[8]->label, "e"))
	  delta= max (0, w + pad);
      delta += 2 * bor;
    }
    format bfm= fm;
    if (request == LAZY_VSTREAM) {
      format_vstream fvs= (format_vstream) fm;
      bfm= make_format_width (fvs->width - delta);
    }
    box b= (box) par->produce (LAZY_BOX, bfm);
    format_width fmw= (format_width) bfm;
    SI width= fmw->width + delta;
    tree old1= env->local_begin (PAGE_MEDIUM, "papyrus");
    tree old2= env->local_begin (PAR_LEFT, "0tmpt");
    tree old3= env->local_begin (PAR_RIGHT, "0tmpt");
    tree old4= env->local_begin (PAR_MODE, "justify");
    tree old5= env->local_begin (PAR_NO_FIRST, "true");
    tree old6= env->local_begin (PAR_WIDTH, tree (TMLEN, as_string (width)));
    SI x1, x2, scx;
    get_canvas_horizontal (env, attrs, b->x1, b->x2, x1, x2, scx);
    SI y1, y2, scy;
    get_canvas_vertical (env, attrs, b->y1, b->y2, y1, y2, scy);
    env->local_end (PAR_WIDTH, old6);
    env->local_end (PAR_NO_FIRST, old5);
    env->local_end (PAR_MODE, old4);
    env->local_end (PAR_RIGHT, old3);
    env->local_end (PAR_LEFT, old2);
    env->local_end (PAGE_MEDIUM, old1);
    path dip= (N(attrs) > 6? decorate (ip): ip);
    box rb= clip_box (dip, b, x1, y1, x2, y2, xt, yt, scx, scy);
    if (N(attrs) > 6)
      rb= put_scroll_bars (env, rb, ip, attrs, b, xt, yt, scx, scy);
    if (request == LAZY_BOX) return make_lazy_box (rb);
    else {
      array<page_item> l;
      l << page_item (rb);
      return lazy_vstream (ip, "", l, stack_border ());
    }
  }
  return lazy_rep::produce (request, fm);
}

lazy
make_lazy_canvas (edit_env env, tree t, path ip) {
  int i, n= N(t);
  tree attrs (TUPLE, n-1);
  for (int i=0; i<4; i++)
    attrs[i]= env->exec (t[i]);
  tree xt = env->expand (t[4]);
  tree yt = env->expand (t[5]);
  attrs[4]= env->exec (xt);
  attrs[5]= env->exec (yt);
  for (i=6; i<n-1; i++)
    attrs[i]= env->exec (t[i]);    
  lazy par= make_lazy (env, t[n-1], descend (ip, n-1));
  return lazy_canvas (env, xt, yt, attrs, par, ip);
}

/******************************************************************************
* Highlighting
******************************************************************************/

struct lazy_highlight_rep: public lazy_rep {
  edit_env env;             // "current" environment
  lazy par;                 // the highlighted body
  SI w, xpad, ypad;         // spacing parameters
  color bg, sunny, shadow;  // colors
  lazy_highlight_rep (edit_env env2, lazy par2, path ip,
		      SI w2, SI xpad2, SI ypad2,
		      color bg2, color sunny2, color shadow2):
    lazy_rep (LAZY_HIGHLIGHT, ip), env (env2), par (par2),
    w (w2), xpad (xpad2), ypad (ypad2),
    bg (bg2), sunny (sunny2), shadow (shadow2) {}
  
  inline operator tree () { return "Highlight"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_highlight {
EXTEND_NULL(lazy,lazy_highlight);
  lazy_highlight (edit_env env, lazy par, path ip,
		  SI w, SI xpad, SI ypad,
		  color bg, color sunny, color shadow):
    rep (new lazy_highlight_rep (env, par, ip, w, xpad, ypad,
				 bg, sunny, shadow)) {
      rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_highlight);

format
lazy_highlight_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    format body_fm= par->query (request, fm);
    format_width fmw= (format_width) body_fm;
    return make_format_width (fmw->width + 2 * (w + xpad));
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_highlight_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM || request == LAZY_BOX) {
    format bfm= fm;
    if (request == LAZY_VSTREAM) {
      format_vstream fvs= (format_vstream) fm;
      bfm= make_format_width (fvs->width - 2 * (w + xpad));
    }
    box b = (box) par->produce (LAZY_BOX, bfm);
    box hb= highlight_box (ip, b, w, xpad, ypad, bg, sunny, shadow);
    if (request == LAZY_BOX) return make_lazy_box (hb);
    else {
      array<page_item> l;
      l << page_item (hb);
      return lazy_vstream (ip, "", l, stack_border ());
    }
  }
  return lazy_rep::produce (request, fm);
}

lazy
make_lazy_highlight (edit_env env, tree t, path ip) {
  SI    w     = env->as_length (env->exec (t[0]));
  SI    xpad  = env->as_length (env->exec (t[1]));
  SI    ypad  = env->as_length (env->exec (t[2]));
  color bg    = env->dis->get_color (env->exec_string (t[3]));
  color sunny = env->dis->get_color (env->exec_string (t[4]));
  color shadow= env->dis->get_color (env->exec_string (t[5]));
  lazy par= make_lazy (env, t[6], descend (ip, 6));
  return lazy_highlight (env, par, ip, w, xpad, ypad, bg, sunny, shadow);
}
