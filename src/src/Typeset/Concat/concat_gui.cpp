
/******************************************************************************
* MODULE     : concat_gui.cpp
* DESCRIPTION: Typesetting GUI markup
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"
#include "formatter.hpp"
#include "analyze.hpp"

SI resize (edit_env env, SI old, SI minimum, SI maximum, tree new_size);

/******************************************************************************
* Scrollable canvases
******************************************************************************/

void
get_canvas_horizontal (edit_env env, tree attrs, SI bx1, SI bx2,
		       SI& x1, SI& x2, SI& scx)
{
  x1= resize (env, bx1, bx1, bx2, attrs[0]);
  x2= resize (env, bx2, bx1, bx2, attrs[2]);
  if (N(attrs) > 6) {
    if (is_atomic (attrs[8]))
      if (ends (attrs[8]->label, "w") || ends (attrs[8]->label, "e")) {
	SI d= env->as_length (attrs[6]) + env->as_length (attrs[7]);
	x2= max (x1, x2-d);
      }
    SI border= 6*env->get_int (SFACTOR) * PIXEL;
    x1 += border;
    x2 -= border;
  }
  if (is_atomic (attrs[4]) && ends (attrs[4]->label, "%")) {
    double p= as_double (attrs[4]->label (0, N(attrs[4]->label)-1)) / 100.0;
    SI d = ((x2 - x1) - (bx2 - bx1));
    SI dx= (d >= 0? 0: (SI) (p * d));
    scx  = dx + x1 - bx1;
  }
  else scx= -env->as_length (attrs[4]);
}

void
get_canvas_vertical (edit_env env, tree attrs, SI by1, SI by2,
		     SI& y1, SI& y2, SI& scy)
{
  y1= resize (env, by1, by1, by2, attrs[1]);
  y2= resize (env, by2, by1, by2, attrs[3]);
  if (N(attrs) > 6) {
    if (is_atomic (attrs[8]))
      if (starts (attrs[8]->label, "n") || starts (attrs[8]->label, "s")) {
	SI d= env->as_length (attrs[6]) + env->as_length (attrs[7]);
	y2= max (y1, y2-d);
      }
    SI border= 6*env->get_int (SFACTOR) * PIXEL;
    y1 += border;
    y2 -= border;
  }
  if (is_atomic (attrs[4]) && ends (attrs[5]->label, "%")) {
    double p= as_double (attrs[5]->label (0, N(attrs[5]->label)-1)) / 100.0;
    SI d = ((y2 - y1) - (by2 - by1));
    SI dy= (d >= 0? d: (SI) (p * d));
    scy  = dy + y1 - by1;
  }
  else scy= -env->as_length (attrs[5]);
}

box
make_hor_bar (edit_env env, path ip, SI x1, SI x2, SI h, SI border,
	      SI X1, SI X2)
{
  box mask1= empty_box (ip, x1, border, x2, h-border);
  box mask2= empty_box (ip, X1, 2*border, X2, h-2*border);
  box hl1  = highlight_box (ip, mask1, border, 0, 0, env->dis->light_grey,
			    env->dis->grey, env->dis->white);
  box hl2  = highlight_box (ip, mask2, border, 0, 0, env->dis->light_grey,
			    env->dis->white, env->dis->grey);
  array<box> bs (2);
  array<SI>  xs (2);
  array<SI>  ys (2);
  bs[0]= hl1; bs[1]= hl2;
  xs[0]= xs[1]= 0;
  ys[0]= ys[1]= 0;
  return composite_box (ip, bs, xs, ys);
}

box
make_ver_bar (edit_env env, path ip, SI y1, SI y2, SI w, SI border,
	      SI Y1, SI Y2)
{
  box mask1= empty_box (ip, border, y1, w-border, y2);
  box mask2= empty_box (ip, 2*border, Y1, w-2*border, Y2);
  box hl1  = highlight_box (ip, mask1, border, 0, 0, env->dis->light_grey,
			    env->dis->grey, env->dis->white);
  box hl2  = highlight_box (ip, mask2, border, 0, 0, env->dis->light_grey,
			    env->dis->white, env->dis->grey);
  array<box> bs (2);
  array<SI>  xs (2);
  array<SI>  ys (2);
  bs[0]= hl1; bs[1]= hl2;
  xs[0]= xs[1]= 0;
  ys[0]= ys[1]= 0;
  return composite_box (ip, bs, xs, ys);
}

box
put_scroll_bars (edit_env env, box b, path ip, tree attrs,
		 box inner, tree xt, tree yt, SI scx, SI scy)
{
  path dip= decorate (ip);
  SI   w  = env->as_length (attrs[6]);
  SI   pad= env->as_length (attrs[7]);
  SI   bor= 6*env->get_int (SFACTOR) * PIXEL;
  int  hor= 0;
  int  ver= 0;
  if (is_atomic (attrs[8]) && starts (attrs[8]->label, "s")) hor= -1;
  if (is_atomic (attrs[8]) && starts (attrs[8]->label, "n")) hor=  1;
  if (is_atomic (attrs[8]) && ends   (attrs[8]->label, "w")) ver= -1;
  if (is_atomic (attrs[8]) && ends   (attrs[8]->label, "e")) ver=  1;
  array<box> bs (1);
  array<SI>  xs (1);
  array<SI>  ys (1);
  bs[0]= highlight_box (dip, b, bor, 0, 0, env->dis->white,
			env->dis->grey, env->dis->grey);
  xs[0]= (ver < 0? w+pad: 0) - bor;
  ys[0]= (hor < 0? w+pad: 0) + bor;
  if (hor != 0 && inner->w() > b->w()) {
    SI dx= b->x1 - inner->x1 - scx;
    double start= 0.0, end= 1.0;
    start= ((double) dx) / ((double) inner->w());
    end  = start + ((double) b->w()) / ((double) inner->w());
    SI X1= b->x1 + bor + ((SI) (start * (b->w() - 2*bor)));
    SI X2= b->x1 + bor + ((SI) (end   * (b->w() - 2*bor)));
    box hor_bar= make_hor_bar (env, dip, b->x1, b->x2, w, bor, X1, X2);
    hor_bar= scrollbar_box (dip, hor_bar, false, X2-X1, xt);
    bs << hor_bar;
    xs << (ver < 0? w+pad: 0) - bor;
    ys << (hor < 0? b->y1: b->y2+pad+2*bor);
  }
  if (ver != 0 && inner->h() > b->h()) {
    SI dy= b->y1 - inner->y1 - scy;
    double start= 0.0, end= 1.0;
    start= ((double) dy) / ((double) inner->h());
    end  = start + ((double) b->h()) / ((double) inner->h());
    SI Y1= b->y1 + bor + ((SI) (start * (b->h() - 2*bor)));
    SI Y2= b->y1 + bor + ((SI) (end   * (b->h() - 2*bor)));
    box ver_bar= make_ver_bar (env, dip, b->y1, b->y2, w, bor, Y1, Y2);
    ver_bar= scrollbar_box (dip, ver_bar, true, Y2-Y1, yt);
    bs << ver_bar;
    xs << (ver < 0? b->x1-2*bor: b->x2+pad);
    ys << (hor < 0? w+pad: 0) + bor;
  }
  return composite_box (ip, bs, xs, ys);
}

void
concater_rep::typeset_scrollable_canvas (tree t, path ip) {
  // IDEA: set left, right, bottom, top environment variables
  //       and allow doing computations with them
  int i, n= N(t);
  tree attrs (TUPLE, n-1);
  for (i=0; i<4; i++)
    attrs[i]= env->exec (t[i]);
  tree xt = env->expand (t[4]);
  tree yt = env->expand (t[5]);
  attrs[4]= env->exec (xt);
  attrs[5]= env->exec (yt);
  for (i=6; i<n-1; i++)
    attrs[i]= env->exec (t[i]);    
  box  b = typeset_as_concat (env, t[n-1], descend (ip, n-1));
  SI x1, y1, x2, y2, scx, scy;
  get_canvas_horizontal (env, attrs, b->x1, b->x2, x1, x2, scx);
  get_canvas_vertical (env, attrs, b->y1, b->y2, y1, y2, scy);
  path dip= (n > 7? decorate (ip): ip);
  box cb= clip_box (dip, b, x1, y1, x2, y2, xt, yt, scx, scy);
  if (n > 7) cb= put_scroll_bars (env, cb, ip, attrs, b, xt, yt, scx, scy);
  print (STD_ITEM, cb);
}

/******************************************************************************
* Highlighting
******************************************************************************/

void
concater_rep::typeset_highlight (tree t, path ip) {
  SI    w     = env->as_length (env->exec (t[0]));
  SI    xpad  = env->as_length (env->exec (t[1]));
  SI    ypad  = env->as_length (env->exec (t[2]));
  color bg    = env->dis->get_color (env->exec_string (t[3]));
  color sunny = env->dis->get_color (env->exec_string (t[4]));
  color shadow= env->dis->get_color (env->exec_string (t[5]));
  box   b     = typeset_as_concat (env, t[6], descend (ip, 6));
  print (STD_ITEM, highlight_box (ip, b, w, xpad, ypad, bg, sunny, shadow));
}
