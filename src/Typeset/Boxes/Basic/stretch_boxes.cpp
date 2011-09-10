
/******************************************************************************
* MODULE     : stretch.cpp
* DESCRIPTION: boxes whose dimensions are (partially) set by the user.
*                - empty and plain boxes
*                - parenthesis boxes
*                - overline and underline like boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "Boxes/construct.hpp"

static void
get_wide_parameters (SI x1, SI x2, SI penw, SI& width, SI& height) {
  if (x2 - x1 >= 20 * penw) width= x2 - x1 - 8 * penw;
  else if (x2 - x1 <= 2 * penw) width= 2 * penw;
  else width= 2 * penw + ((10 * (x2 - x1 - 2 * penw)) / 18);
  int ratio = width / penw;
  int srrat = (int) sqrt ((double) ratio);
  height= srrat * penw;
}

box
wide_hat_box (path ip, SI x1, SI x2, SI penw, color col) {
  SI width, height;
  get_wide_parameters (x1, x2, penw, width, height);
  array<box> bs (2);
  array<SI>  xs (2);
  array<SI>  ys (2);
  xs[0]= ys[0]= xs[1]= ys[1]= 0;
  bs[0]= line_box (decorate_middle (ip), 0, 0, width/2, height, penw, col);
  bs[1]= line_box (decorate_middle (ip), width/2, height, width, 0, penw, col);
  return composite_box (ip, bs, xs, ys);
}

box
wide_tilda_box (path ip, SI x1, SI x2, SI penw, color col) {
  SI width, height, w, h, uw, uh, ww, hh;
  get_wide_parameters (x1, x2, penw, width, height);
  h = height/2;
  uh= h;
  hh= (SI) (0.8660254 * ((double) h));
  w = width;
  uw= (SI) (((double) w) / 4.2679492);
  ww= (SI) (1.1339746 * ((double) h));
  array<box> bs (3);
  array<SI>  xs (3);
  array<SI>  ys (3);
  xs[0]= ys[0]= xs[1]= ys[1]= xs[2]= ys[2]= 0;
  bs[0]= arc_box (decorate_middle (ip),
		  0, -h, 2*uw, h, 60<<6, 180<<6, penw, col);
  bs[1]= line_box (decorate_middle (ip),
		   3*uw/2, hh, w-(3*uw/2), h-hh, penw, col);
  bs[2]= arc_box (decorate_middle (ip),
		  w- (2*uw), 0, w, 2*h, 240<<6, 360<<6, penw, col);
  return composite_box (ip, bs, xs, ys);
}

box
wide_bar_box (path ip, SI x1, SI x2, SI penw, color col) {
  return line_box (ip, 0, 0, max (penw, x2- x1), 0, penw, col);
}

box
wide_vect_box (path ip, SI x1, SI x2, SI penw, color col) {
  SI width, height, arrow= 2*penw, delta=penw/2;
  get_wide_parameters (x1, x2, penw, width, height);
  height= 10*penw;
  array<box> bs (3);
  array<SI>  xs (3);
  array<SI>  ys (3);
  xs[0]= ys[0]= xs[1]= ys[1]= xs[2]= ys[2]= 0;
  bs[0]= line_box (decorate_middle (ip), 0, arrow, width, arrow, penw, col);
  bs[1]= line_box (decorate_middle (ip),
		   width- arrow- delta, 0, width, arrow, penw, col);
  bs[2]= line_box (decorate_middle (ip),
		   width+ delta- arrow, 2*arrow, width, arrow, penw, col);
  return composite_box (ip, bs, xs, ys);
}

box
wide_check_box (path ip, SI x1, SI x2, SI penw, color col) {
  SI width, height;
  get_wide_parameters (x1, x2, penw, width, height);
  array<box> bs (2);
  array<SI>  xs (2);
  array<SI>  ys (2);
  xs[0]= ys[0]= xs[1]= ys[1]= 0;
  bs[0]= line_box (decorate_middle (ip), 0, height, width/2, 0, penw, col);
  bs[1]= line_box (decorate_middle (ip), width/2, 0, width, height, penw, col);
  return composite_box (ip, bs, xs, ys);
}

box
wide_breve_box (path ip, SI x1, SI x2, SI penw, color col) {
  SI width, height;
  get_wide_parameters (x1, x2, penw, width, height);
  return arc_box (ip, 0, 0, width, 2*height, 180<<6, 360<<6, penw, col);
}

box
wide_squbr_box (path ip, SI x1, SI x2, SI penw, color col) {
  path dip= decorate_middle (ip);
  SI width= max (x2-x1, 6*penw), height= 6*penw;
  array<box> bs (3);
  array<SI>  xs (3);
  array<SI>  ys (3);
  xs[0]= ys[0]= xs[1]= ys[1]= xs[2]= ys[2]= 0;
  bs[0]= line_box (dip, 0, 0, width, 0, penw, col);
  bs[1]= line_box (dip, 0, height, 0, 0, penw/2, col);
  bs[2]= line_box (dip, width, height, width, 0, penw/2, col);
  return composite_box (ip, bs, xs, ys);
}

box
wide_sqobr_box (path ip, SI x1, SI x2, SI penw, color col) {
  path dip= decorate_middle (ip);
  SI width= max (x2-x1, 6*penw), height= 6*penw;
  array<box> bs (3);
  array<SI>  xs (3);
  array<SI>  ys (3);
  xs[0]= ys[0]= xs[1]= ys[1]= xs[2]= ys[2]= 0;
  bs[0]= line_box (dip, 0, height, width, height, penw, col);
  bs[1]= line_box (dip, 0, height, 0, 0, penw/2, col);
  bs[2]= line_box (dip, width, height, width, 0, penw/2, col);
  return composite_box (ip, bs, xs, ys);
}
