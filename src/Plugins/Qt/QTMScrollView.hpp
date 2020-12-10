
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
 origin which can be acted upon via the scrollbars.
*/

class QTMScrollView : public QAbstractScrollArea {
  Q_OBJECT

  bool   editor_flag;   // Set to true for editor widgets
  QRect    p_extents;   // The size of the virtual area where things are drawn.
  QPoint    p_origin;   // The offset into that area
  
public:
  
  QTMScrollView (QWidget *_parent = NULL);

  QPoint  origin () { return p_origin; }
  void setOrigin (QPoint newOrigin);
  
  QRect   extents () { return p_extents; }
  void setExtents (QRect newExtents);

  QWidget* surface () const { return viewport (); }
  
  void ensureVisible (int cx, int cy, int mx = 50, int my = 50);
  
    // Viewport/contents position converters.
  QPoint viewportToContents (QPoint const& pos) const { return pos + p_origin + p_extents.topLeft(); }
  QPoint contentsToViewport (QPoint const& pos) const { return pos - p_origin - p_extents.topLeft(); }
  
protected:
  
  void updateScrollBars();
  void scrollContentsBy (int dx, int dy);
  
  virtual bool event (QEvent *event);
  virtual void resizeEventBis (QResizeEvent *e);

  friend class qt_simple_widget_rep;
};


#endif // QTMSCROLLVIEW_HPP
