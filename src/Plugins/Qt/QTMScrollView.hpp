
/******************************************************************************
 * MODULE     : QTMScrollView.hpp
 * DESCRIPTION: QT Texmacs abstract scroll view widget
 * COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMSCROLLVIEW_HPP
#define QTMSCROLLVIEW_HPP

#include <QAbstractScrollArea>
#include <QRect>

class QResizeEvent;
class QPaintEvent;


/*! Scroll view widget.

 The current structure of the central texmacs widget (the canvas) is the 
 following: the canvas is a derived class of QTMScrollView which being
 a QAbstractScrollArea owns a central widget called the "viewport".
 QAbstractScrollArea coordinates the viewport with the scrollbars and maintains
 informations like the real extent of the working surface and the current 
 origin which can be acted upon via the scrollbars. This setup has been 
 augmented via another widget child of the viewport which we call the 
 "surface". The only purpose of this widget is to provide automatic centering
 of the working area inside the viewport. To support this we "un-wired" the
 event redirection built-in in QAbstractScrollArea (from the viewport widget 
 to the QAbstractScrollArea) and re-wired event redirection from the surface
 to the QTMScrollView. All relevant events like resize, I/O events and the 
 like which are sent to the surface are resent to the QTMScrollView for 
 handling. This allows to concentrate all the logic in only one object.
 See QTMSurface::event for info about the redirected events.
*/
class QTMScrollView : public QAbstractScrollArea {
  Q_OBJECT

  QRect    p_extents;   // The size of the virtual area where things are drawn.
  QPoint    p_origin;   // The offset into that area
  QWidget* p_surface;
  
public:
  
  QTMScrollView (QWidget *_parent = NULL);
  virtual ~QTMScrollView () { }

  QPoint  origin () { return p_origin; }
  void setOrigin (QPoint newOrigin);
  
  QRect   extents () { return p_extents; }
  void setExtents (QRect newExtents);

  QWidget* surface () { return p_surface; }
  
  void ensureVisible (int cx, int cy, int mx = 50, int my = 50);
  
    // Viewport/contents position converters.
  QPoint viewportToContents (QPoint const& pos) const { return pos + p_origin; }
  QPoint contentsToViewport (QPoint const& pos) const { return pos - p_origin; }
  
protected:
  
  // Scrollbar stabilization.
  void updateScrollBars();
  
  // Scroll area updater.
  void scrollContentsBy (int dx, int dy);
  
  virtual bool viewportEvent (QEvent *e);
  virtual bool surfaceEvent (QEvent *e);
  virtual bool event (QEvent *e);

  friend class QTMSurface;
};

#endif // QTMSCROLLVIEW_HPP
