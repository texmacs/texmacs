
/******************************************************************************
* MODULE     : load_tfm.h
* DESCRIPTION: load TeX font metric file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LOAD_TFM_H
#define LOAD_TFM_H
#include "file.hpp"
#include "tex_files.hpp"
#include "resource.hpp"
#include "bitmap_font.hpp"

RESOURCE(tex_font_metric);

struct tex_font_metric_rep: rep<tex_font_metric> {
  HN      lf, lh, bc, ec;
  HN      nw, nh, nd, ni;
  HN      nl, nk, ne, np;

  SI*     header;
  SI*     char_info;
  SI*     width;
  SI*     height;
  SI*     depth;
  SI*     italic;
  SI*     lig_kern;
  SI*     kern;
  SI*     exten;
  SI*     param;

  SI      left     , right;
  SI      left_prog, right_prog;

  int     size; // original size (needed to compute magnification of pk font)

  /***************************************************************************/

  tex_font_metric_rep (string name);
  ~tex_font_metric_rep ();

  SI      w (QN c);
  SI      h (QN c);
  SI      d (QN c);
  SI      i (QN c);
  SI      tag (QN c);
  SI      rem (QN c);
  int     list_len (QN c);
  QN      nth_in_list (QN c, int n);
  QN      top (QN c);
  QN      mid (QN c);
  QN      bot (QN c);
  QN      rep (QN c);

  SI      design_size ();
  double  slope ();
  SI      spc ();
  SI      spc_stretch ();
  SI      spc_shrink ();
  SI      x_height ();
  SI      spc_quad ();
  SI      spc_extra ();
  SI      parameter (int i);

  void    execute (SI* s, int n, SI* buf, SI* ker, int& m);
  void    get_xpositions (int* s, int n, double unit, SI* xpos);
};

tex_font_metric load_tfm (url file_name, string family, int size);

#endif // defined LOAD_TFM_H
