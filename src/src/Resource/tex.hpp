
/******************************************************************************
* MODULE     : tex.hpp
* DESCRIPTION: tex font metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TEX_H
#define TEX_H
#include "resource.hpp"
#include "bitmap_font.hpp"

RESOURCE(tex_font_metric);
typedef int SI;
typedef unsigned short HN;
typedef unsigned char QN;
typedef int wm_object;
typedef wm_object bitmap;
typedef wm_object pixmap;

/******************************************************************************
* The tex_font_metric data type
******************************************************************************/

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

tex_font_metric load_tfm (string family, int size);

/******************************************************************************
* Loading a TeX font
******************************************************************************/

void load_tex (string family, int size, int dpi, int dsize,
	       tex_font_metric& tfm, font_glyphs& pk);

#endif // defined TEX_H
