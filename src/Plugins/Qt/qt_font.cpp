
/******************************************************************************
* MODULE     : qt_font.cpp
* DESCRIPTION: Qt fonts
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Qt/qt_font.hpp"
#include "Qt/qt_utilities.hpp"
#include "Qt/qt_renderer.hpp"

#include "analyze.hpp"
#include "dictionary.hpp"

#define MAGN (dpi * PIXEL / 72.0)
#define ROUND(x) ((SI) round (x * MAGN))
#define FLOOR(x) ((SI) floor (x * MAGN))
#define CEIL(x)  ((SI) ceil  (x * MAGN))

/******************************************************************************
* The implementation
******************************************************************************/

qt_font_rep::qt_font_rep (string name, string family2, int size2, int dpi2):
  font_rep (name), family (family2), size (size2), dpi (dpi2),
  qfn (to_qstring (family), size),
  //qfn (to_qstring (family), size, QFont::Normal, false),
  qfm (qfn)
{
  type= FONT_TYPE_QT;

  // get main font parameters
  y1= FLOOR (-qfm.descent ());
  y2= CEIL  (qfm.ascent () + 1);
  display_size = y2-y1;
  design_size  = size << 8;

  // get character dimensions
  metric ex;
  yx           = ROUND (qfm.xHeight ());
  get_extents ("M", ex);
  wquad        = ex->x2;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute other widths
  wpt          = (dpi*PIXEL)/72;
  hpt          = (dpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;

  // get fraction bar parameters
  get_extents ("-", ex);
  yfrac= (ex->y3 + ex->y4) >> 1;

  // get space length
  get_extents (" ", ex);
  spc= space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (ex->x2-ex->x1)<<1);
  extra   = spc;
  math_spc= spc;
  sep     = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size);
  if (slope<0.15) slope= 0.0;
}

bool
qt_font_rep::supports (string c) {
  QString qs= utf8_to_qstring (cork_to_utf8 (c));
  return qs.length () == 1 && qfm.inFont (qs[0]);
}

void
qt_font_rep::get_extents (string s, metric& ex) {
  QString qs  = utf8_to_qstring (cork_to_utf8 (s));
  QRectF  rect= qfm.tightBoundingRect (qs);
  qreal   w   = qfm.width (qs);
  ex->x1= 0;
  ex->x2= ROUND (w);
  ex->y1= FLOOR (-rect.bottom ());
  ex->y2= CEIL  (-rect.top ());
  ex->x3= FLOOR (rect.left ());
  ex->x4= CEIL  (rect.right ());
  ex->y3= ex->y1;
  ex->y4= ex->y2;
}

void
qt_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  if (N(s)!=0) {
    QString qs= utf8_to_qstring (cork_to_utf8 (s));
    double zoom= dpi / (std_shrinkf * 72.0);
    qt_renderer_rep* qren= (qt_renderer_rep*) ren->get_handle ();
    qren -> draw (qfn, qs, x, y, zoom);
  }
}

font
qt_font_rep::magnify (double zoomx, double zoomy) {
  if (zoomx != zoomy) return poor_magnify (zoomx, zoomy);
  return qt_font (family, size, (int) round (dpi * zoomx));
}

/******************************************************************************
* Interface
******************************************************************************/

font
qt_font (string family, int size, int dpi) {
  string name= "qt:" * family * as_string (size) * "@" * as_string (dpi);
  if (font::instances -> contains (name)) return font (name);
  else return tm_new<qt_font_rep> (name, family, size, dpi);
}
