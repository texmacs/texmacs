
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
  return (picture) tm_new<qt_picture_rep> (im, ox, oy);
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
pixmap_picture (int w, int h, int ox, int oy) {
  return qt_picture (QImage (w, h, QImage::Format_ARGB32), ox, oy);
}

picture
scalable_picture (int w, int h, int ox, int oy) {
  (void) w; (void) h; (void) ox; (void) oy;
  FAILED ("not yet implemented");
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

qt_image_renderer_rep::qt_image_renderer_rep (picture p, renderer m):
  qt_renderer_rep (new QPainter ()), pict (p)
{
  ox = m->ox;
  oy = m->oy;
  cx1= m->cx1;
  cy1= m->cy1;
  cx2= m->cx2;
  cy2= m->cy2;
  is_screen= m->is_screen;
  zoomf= m->zoomf;
  shrinkf= m->shrinkf;
  pixel= m->pixel;
  thicken= m->thicken;

  /*
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y () - 1;
  ox  += pox * pixel;
  oy  += poy * pixel;
  cx1 += pox * pixel;
  cy1 += poy * pixel;
  cx2 += pox * pixel;
  cy2 += poy * pixel;
  */

  SI x0= 0, y0= 0;
  m->decode (x0, y0);
  int x1b= x0 - p->get_origin_x ();
  int y2b= y0 + p->get_origin_y () - (p->get_height () - 1);
  ox  -= x1b * pixel;
  oy  += y2b * pixel;
  cx1 -= x1b * pixel;
  cy1 += y2b * pixel;
  cx2 -= x1b * pixel;
  cy2 += y2b * pixel;

  if (m->is_printer ()) set_zoom_factor (1.0);

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

void*
qt_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<qt_image_renderer_rep> (p, zoomf);
}

renderer
picture_renderer (picture p, renderer m) {
  return (renderer) tm_new<qt_image_renderer_rep> (p, m);
}

void
delete_renderer (renderer ren) {
  tm_delete (ren);
}

/******************************************************************************
* Loading pictures
******************************************************************************/

struct qt_cache_image_rep: cache_image_element_rep {
  qt_cache_image_rep (int w2, int h2, time_t time2, QImage *ptr2):
    cache_image_element_rep (w2, h2, time2, ptr2) {}
  virtual ~qt_cache_image_rep () {
    delete static_cast<QImage*> (ptr); }
};

QImage*
get_image (url u, int w, int h) {
  QImage *pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w) << as_string (h) << "qt-image" ;
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci))
    pm= static_cast<QImage*> (ci->ptr);
  else {
    // rendering
    if (qt_supports (u))
      pm= new QImage (utf8_to_qstring (concretize (u)));
    else if (suffix (u) == "ps" ||
             suffix (u) == "eps" ||
             suffix (u) == "pdf") {
      url temp= url_temp (".png");
      image_to_png (u, temp, w, h);
/*
      string idstr= eval_system ("identify",u);
      int i=0;
      int a=0,b=0;
      while(i<N(idstr)) {
        b=0;
        if(idstr[i]==' ') {
          i++;
          a=i;
          while(i<N(idstr) && idstr[i]>'0' && idstr[i]<'9')
            i++;
          if(i>=N(idstr))
            break;
          if(idstr[i] != 'x')
            continue;
          i++;
          b=i;
          while(i<N(idstr) && idstr[i]>'0' && idstr[i]<'9')
            i++;
          if(i<N(idstr) && idstr[i]==' ')
            break;
        }
        i++;
      }
      int iw,ih;
      if(b>0) {
        iw=as_int(idstr(a,b-1));
        ih=as_int(idstr(b,i));
      } else {
        int bbx1,bby1,bbx2,bby2;
        ps_bounding_box(u,bbx1,bby1,bbx2,bby2);
        iw=bbx2-bbx1;
        ih=bby2-bby1;
      }

//      float resx = 72*w/((bbx2-bbx1)*(cx2-cx1));
//      float resy = 72*h/((bby2-bby1)*(cy2-cy1));
      float resx = 144*w/(iw*(cx2-cx1));
      float resy = 144*h/(ih*(cy2-cy1));

      url temp= url_temp (".png");
      system ("convert -density " * as_string(resx) * "x" * as_string(resy)
             * " -scale 50% -crop " * as_string(w) * "x" * as_string(h)
             * "+" * as_string (cx1*w/(cx2-cx1))
             * "+" * as_string ((1-cy2)*h/(cy2-cy1))
             * "! -background white -flatten", u, temp);
*/
      pm= new QImage (to_qstring (as_string (temp)));
      remove (temp);
    }
    if (pm == NULL || pm->isNull ()) {
      if (pm != NULL) {
        delete pm;
        pm= NULL;
      }
      if (as_bool (call ("file-converter-exists?", u, "x.png"))) {
        url temp= url_temp (".png");
        call ("file-convert", object (u), object (temp));
        pm= new QImage (to_qstring (as_string (temp)));
        remove (temp);
      }
      if (pm == NULL || pm->isNull ()) {
        if (pm != NULL) delete pm;
        cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
        return NULL;
      }
    }

    if (pm->width () != w || pm->height () != h)
      (*pm)= pm->scaled (w, h);
    ci = tm_new<qt_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }
  return pm;
}

picture
load_picture (url u, int w, int h) {
  QImage* im= get_image (u, w, h);
  if (im == NULL) return error_picture (w, h);
  return qt_picture (*im, 0, 0);
}

picture
qt_load_xpm (url file_name) {
  string sss;
  if (suffix (file_name) == "xpm") {
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
