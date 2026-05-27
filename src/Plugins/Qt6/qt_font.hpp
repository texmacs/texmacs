
/******************************************************************************
* MODULE     : qt_font.hpp
* DESCRIPTION: Qt fonts
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QT_FONT_H
#define QT_FONT_H

#include <QFont>
#include <QFontMetrics>

#include "font.hpp"

/******************************************************************************
* The qt_font representation class
******************************************************************************/

struct qt_font_rep: font_rep {
  string family;
  int    size;
  int    dpi;
  QFont  qfn;
  QFontMetricsF qfm;
  
  qt_font_rep (string name, string family, int size, int dpi);
  bool supports (string c);
  void get_extents (string s, metric& ex);
  void draw_fixed (renderer ren, string s, SI x, SI y);
  font magnify (double zoomx, double zoomy);
  //void  advance_glyph (string s, int& pos, bool ligf);
  //glyph get_glyph (string s);
  //int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);
};

#endif // defined QT_FONT_H
