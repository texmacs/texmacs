
/******************************************************************************
* MODULE     : aligned_widget.cpp
* DESCRIPTION: aligned widgets
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "Widkit/wk_widget.hpp"

wk_widget
aligned_widget (array<wk_widget> lhs, array<wk_widget> rhs,
		SI hsep, SI vsep, SI lpad, SI rpad)
{
  ASSERT (N(lhs) > 0, "at least one widget expected");
  ASSERT (N(lhs) == N(rhs), "sizes don't match");
  int i, n= N (lhs);
  SI max_w= 0;
  for (i=0; i<n; i++) {
    SI ww=1280*PIXEL, hh=64*PIXEL;
    lhs[i] << get_size (ww, hh, 0);
    max_w= max (ww, max_w);
  }

  array<wk_widget> fields_w (2*n-1);
  for (i=0; i<n; i++) {
    SI ww=1280*PIXEL, hh=64*PIXEL;
    lhs[i] << get_size (ww, hh, 0);
    array<wk_widget> line_w (5);
    array<string> line_n (5);
    line_w[0]= glue_wk_widget (false, false, lpad + max_w - ww);
    line_w[1]= lhs[i];
    line_n[1]= "prompt";
    line_w[2]= glue_wk_widget (false, false, hsep);
    line_w[3]= rhs[i];
    line_n[3]= "input";
    line_w[4]= glue_wk_widget (false, false, rpad);
    fields_w[2*i]= horizontal_list (line_w, line_n);
    if (i < n-1) fields_w[2*i + 1]= glue_wk_widget (false, false, 0, vsep);
  }

  return vertical_list (fields_w);
}
