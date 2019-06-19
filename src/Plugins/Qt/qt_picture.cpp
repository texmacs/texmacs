
/******************************************************************************
* MODULE     : qt_picture.cpp
* DESCRIPTION: QT pictures
* COPYRIGHT  : (C) 2013 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_picture.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "qt_utilities.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "scheme.hpp"
#include "frame.hpp"
#include "effect.hpp"

#include <QObject>
#include <QWidget>
#include <QPaintDevice>
#include <QPixmap>

/******************************************************************************
* Abstract Qt pictures
******************************************************************************/

qt_picture_rep::qt_picture_rep (const QImage& im, int ox2, int oy2):
  pict (im), w (im.width ()), h (im.height ()), ox (ox2), oy (oy2) {}

picture_kind qt_picture_rep::get_type () { return picture_native; }
void* qt_picture_rep::get_handle () { return (void*) this; }

int qt_picture_rep::get_width () { return w; }
int qt_picture_rep::get_height () { return h; }
int qt_picture_rep::get_origin_x () { return ox; }
int qt_picture_rep::get_origin_y () { return oy; }
void qt_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
qt_picture_rep::internal_get_pixel (int x, int y) {
  return (color) pict.pixel (x, h - 1 - y);
}

void
qt_picture_rep::internal_set_pixel (int x, int y, color c) {
  pict.setPixel (x, h - 1 - y, c);
}

picture
qt_picture (const QImage& im, int ox, int oy) {
  return (picture) tm_new<qt_picture_rep,QImage,int,int> (im, ox, oy);
}

picture
as_qt_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  picture ret= qt_picture (QImage (pic->get_width (), pic->get_height (),
                                   QImage::Format_ARGB32),
                           pic->get_origin_x (), pic->get_origin_y ());
  ret->copy_from (pic);
  return ret;
}

picture
as_native_picture (picture pict) {
  return as_qt_picture (pict);
}

QImage*
xpm_image (url file_name) {
  picture p= load_xpm (file_name);
  qt_picture_rep* rep= (qt_picture_rep*) p->get_handle ();
  return &(rep->pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  return qt_picture (QImage (w, h, QImage::Format_ARGB32), ox, oy);
}

void
qt_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  p= as_qt_picture (p);
  qt_picture_rep* pict= (qt_picture_rep*) p->get_handle ();
  int x0= pict->ox, y0= pict->h - 1 - pict->oy;
  decode (x, y);
  qreal old_opacity= painter->opacity ();
  painter->setOpacity (qreal (alpha) / qreal (255));
  painter->drawImage (x - x0, y - y0, pict->pict);
  painter->setOpacity (old_opacity);
}

/******************************************************************************
* Rendering on images
******************************************************************************/

qt_image_renderer_rep::qt_image_renderer_rep (picture p, double zoom):
  qt_renderer_rep (new QPainter ()), pict (p)
{
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox = pox * pixel;
  oy = poy * pixel;
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;

  qt_picture_rep* handle= (qt_picture_rep*) pict->get_handle ();
  QImage& im (handle->pict);
#if (QT_VERSION >= 0x040800)
  im.fill (QColor (0, 0, 0, 0));
#else
  im.fill ((uint) 0);
#endif
  painter->begin (&im);
}

qt_image_renderer_rep::~qt_image_renderer_rep () {
  painter->end();
  delete painter;
  painter = NULL;
}

void
qt_image_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (zoom);
}

void*
qt_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<qt_image_renderer_rep> (p, zoomf);
}

/******************************************************************************
* Loading pictures
******************************************************************************/

QImage*
get_image (url u, int w, int h, tree eff) {
  QImage *pm = NULL;
  if (qt_supports (u))
    pm= new QImage (utf8_to_qstring (concretize (u)));
  else {
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    pm= new QImage (utf8_to_qstring (as_string (temp)));
    remove (temp);
  }
  if (pm == NULL || pm->isNull ()) {
      if (pm != NULL) delete pm;
      cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
      return NULL;
  }
  if (pm->width () != w || pm->height () != h)
    (*pm)= pm->scaled (w, h);
  if (eff != "") {
    effect e= build_effect (eff);
    picture src= qt_picture (*pm, 0, 0);
    array<picture> a;
    a << src;
    picture pic= e->apply (a, 5*PIXEL);
    picture dest= as_qt_picture (pic);
    qt_picture_rep* rep= (qt_picture_rep*) dest->get_handle ();
    QImage *trf= (QImage*) &(rep->pict);
    delete pm;
    pm= new QImage (trf->copy ());
  }
  return pm;
}

picture
load_picture (url u, int w, int h) {
  QImage* im= get_image (u, w, h, "");
  if (im == NULL) return error_picture (w, h);
  return qt_picture (*im, 0, 0);
}

picture
qt_load_xpm (url file_name) {
  string sss;
  if (retina_icons > 1 && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 4), "_x2.png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
  }
  if (sss == "" && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 3), "png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
  }
  if (sss == "")
    load_string ("$TEXMACS_PIXMAP_PATH" * file_name, sss, false);
  if (sss == "")
    load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", sss, true);
  c_string buf (sss);
  QImage pm;
  pm.loadFromData ((uchar*) (char*) buf, N(sss));
  return qt_picture (pm, 0, 0);
}

/******************************************************************************
* Applying effects to existing pictures
******************************************************************************/

void
qt_apply_effect (tree eff, array<url> src, url dest, int w, int h) {
  array<picture> a;
  for (int i=0; i<N(src); i++)
    a << load_picture (src[i], w, h);
  effect  e= build_effect (eff);
  picture t= e->apply (a, PIXEL);
  picture q= as_qt_picture (t);
  qt_picture_rep* pict= (qt_picture_rep*) q->get_handle ();
  pict->pict.save (utf8_to_qstring (concretize (dest)));
}
