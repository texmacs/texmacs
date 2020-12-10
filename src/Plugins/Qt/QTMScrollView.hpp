
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
 augmented via another widget child of the scrollview which we call the 
 "surface", with the purpose of centering the working area. See the 
 documentation for QTMSurface for more info on this.
*/
class QTMScrollView : public QAbstractScrollArea {
  Q_OBJECT

  bool   editor_flag;   // Set to true for editor widgets
  QRect    p_extents;   // The size of the virtual area where things are drawn.
  QPoint    p_origin;   // The offset into that area
  QWidget* p_surface;   // Actual drawing area, centered (or not) in the scrollarea
  
public:
  
  QTMScrollView (QWidget *_parent = NULL);

  QPoint  origin () { return p_origin; }
  void setOrigin (QPoint newOrigin);
  
  QRect   extents () { return p_extents; }
  void setExtents (QRect newExtents);

  QWidget* surface () const { return p_surface; }
  
  void ensureVisible (int cx, int cy, int mx = 50, int my = 50);
  
    // Viewport/contents position converters.
  QPoint viewportToContents (QPoint const& pos) const { return pos + p_origin; }
  QPoint contentsToViewport (QPoint const& pos) const { return pos - p_origin; }
  
protected:
  
  void updateScrollBars();
  void scrollContentsBy (int dx, int dy);
  
  virtual void resizeEventBis (QResizeEvent *e);
  virtual bool viewportEvent (QEvent *e);
  virtual bool surfaceEvent (QEvent *e);
  virtual bool event (QEvent *e);

  friend class QTMSurface;
  friend class qt_simple_widget_rep;
};


#endif // QTMSCROLLVIEW_HPP
