
/******************************************************************************
* MODULE     : load_tfm.h
* DESCRIPTION: load TeX font metric file
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LOAD_TFM_H
#define LOAD_TFM_H
#include "file.hpp"
#include "tex_files.hpp"
#include "resource.hpp"
#include "bitmap_font.hpp"

RESOURCE(tex_font_metric);

struct tex_font_metric_rep: rep<tex_font_metric> {
  N16     lf, lh, bc, ec;
  N16     nw, nh, nd, ni;
  N16     nl, nk, ne, np;

  Z32*    header;
  Z32*    char_info;
  Z32*    width;
  Z32*    height;
  Z32*    depth;
  Z32*    italic;
  Z32*    lig_kern;
  Z32*    kern;
  Z32*    exten;
  Z32*    param;

  Z32     left     , right;
  Z32     left_prog, right_prog;

  int     size; // original size (needed to compute magnification of pk font)

  /***************************************************************************/

  tex_font_metric_rep (string name);
  ~tex_font_metric_rep ();

  Z32     w (N8 c);
  Z32     h (N8 c);
  Z32     d (N8 c);
  Z32     i (N8 c);
  Z32     tag (N8 c);
  Z32     rem (N8 c);
  int     list_len (N8 c);
  N8      nth_in_list (N8 c, int n);
  N8      top (N8 c);
  N8      mid (N8 c);
  N8      bot (N8 c);
  N8      rep (N8 c);

  Z32     design_size ();
  double  slope ();
  Z32     spc ();
  Z32     spc_stretch ();
  Z32     spc_shrink ();
  Z32     x_height ();
  Z32     spc_quad ();
  Z32     spc_extra ();
  Z32     parameter (int i);

  void    execute (Z32* s, int n, Z32* buf, Z32* ker, int& m);
  void    get_xpositions (int* s, int n, double unit, SI* xpos, bool ligf);
};

font_metric tfm_font_metric (tex_font_metric tfm);
tex_font_metric load_tfm (url file_name, string family, int size);

#endif // defined LOAD_TFM_H
