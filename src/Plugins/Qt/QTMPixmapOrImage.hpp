
/******************************************************************************
* MODULE     : QTMPixmapOrImage.hpp
* DESCRIPTION: Union of QPixmap and QImage for headless mode
* COPYRIGHT  : (C) 2022 Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMPIXMAPORIMAGE_HPP
#define QTMPIXMAPORIMAGE_HPP

#include "boot.hpp"
#include <QPixmap>
#include <QImage>

// if QTMPIXMAPS is defined we use QPixmap for characters
// otherwise we use QImage (which supports alpha also under X11)

#if QT_VERSION >= 0x060000
#undef QTMPIXMAPS
#else
#ifdef Q_OS_MAC
#define QTMPIXMAPS
#else
#undef QTMPIXMAPS
#endif
#endif

struct QTMPixmapOrImage {
  void* rep;
  QTMPixmapOrImage () {
    if (headless_mode) rep= (void*) new QImage ();
    else rep= (void*) new QPixmap ();
  }
  ~QTMPixmapOrImage () {
    if (headless_mode) delete (QImage*) rep;
    else delete (QPixmap*) rep;
  }
  QTMPixmapOrImage (int w, int h) {
    if (headless_mode)
      rep= (void*) new QImage (w, h, QImage::Format_ARGB32);
    else
      rep= (void*) new QPixmap (w, h);
  }
  QTMPixmapOrImage (QSize s) {
    if (headless_mode)
      rep= (void*) new QImage (s, QImage::Format_ARGB32);
    else
      rep= (void*) new QPixmap (s);
  }
  QTMPixmapOrImage (const QPixmap& px): rep ((void*) new QPixmap (px)) {}
  QTMPixmapOrImage (const QImage& im): rep ((void*) new QImage (im)) {}
  QTMPixmapOrImage (const QTMPixmapOrImage& pxim) {
    if (headless_mode) rep= (void*) new QImage ();
    else rep= (void*) new QPixmap ();
    if (headless_mode)
      *((QImage*) rep)= *((QImage*) pxim.rep);
    else
      *((QPixmap*) rep)= *((QPixmap*) pxim.rep);    
  }
  QTMPixmapOrImage& operator=(const QTMPixmapOrImage& pxim) {
    if (headless_mode)
      *((QImage*) rep)= *((QImage*) pxim.rep);
    else
      *((QPixmap*) rep)= *((QPixmap*) pxim.rep);
  }
  void fill (const QColor& c) {
    if (headless_mode)
      ((QImage*) rep)->fill (c);
    else
      ((QPixmap*) rep)->fill (c);
  }
  bool isNull () {
    return headless_mode ?
      ((QImage*) rep)->isNull () : ((QPixmap*) rep)->isNull ();
  }
  QImage* QImage_ptr () {
    ASSERT (headless_mode, "internal bug in QTMPixmapOrImage::QImage_ptr");
    return (QImage*) rep;
  }
  QPixmap* QPixmap_ptr () {
    ASSERT (!headless_mode, "internal bug in QTMPixmapOrImage::QPixmap_ptr");
    return (QPixmap*) rep;
  }
  void* void_ptr () {
    return rep;
  } 
};

#ifdef QTMPIXMAPS
#define QTMImage QTMPixmapOrImage
#else
#define QTMImage QImage
#endif

#endif // defined QT_RENDERER_HPP
