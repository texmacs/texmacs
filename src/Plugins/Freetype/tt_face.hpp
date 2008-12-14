
/******************************************************************************
* MODULE     : tt_face.hpp
* DESCRIPTION: resources for true type faces, gliefs and metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TT_FACE_H
#define TT_FACE_H
#include "bitmap_font.hpp"
#include "Freetype/free_type.hpp"
#include "hashmap.hpp"

#ifdef USE_FREETYPE

RESOURCE(tt_face);

struct tt_face_rep: rep<tt_face> {
  bool bad_face;
  FT_Face ft_face;
  tt_face_rep (string name);
};

struct tt_font_metric_rep: font_metric_rep {
  bool bad_metric;
  tt_face face;
  int size, dpi;
  hashmap<int,pointer> fnm;
  //metric* fnm;
  //bool* done;
  tt_font_metric_rep (string name, string family, int size, int dpi);
  metric& get (int char_code);
};

struct tt_font_glyphs_rep: font_glyphs_rep {
  bool bad_glyphs;
  tt_face face;
  int size, dpi;
  hashmap<int,glyph> fng;
  //glyph* fng;
  //bool* done;
  tt_font_glyphs_rep (string name, string family, int size, int dpi);
  glyph& get (int char_code);
};

tt_face load_tt_face (string name);
font_metric tt_font_metric (string family, int size, int dpi);
font_glyphs tt_font_glyphs (string family, int size, int dpi);

#endif // USE_FREETYPE

#endif // defined TT_FACE_H
