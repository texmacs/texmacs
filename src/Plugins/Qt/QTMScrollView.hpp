
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

// Forward declarations.
class QResizeEvent;
class QPaintEvent;


//----------------------------------------------------------------------------
// QTMScrollView -- abstract scroll view widget.

class QTMScrollView : public QAbstractScrollArea {
  Q_OBJECT

  QRect p_extents;
  QPoint p_origin;
  QWidget *p_surface;
  
public:
  
  QTMScrollView (QWidget *_parent = NULL);
  virtual ~QTMScrollView();
  
  void setOrigin ( QPoint newOrigin ) ;
  void setExtents ( QRect newExtents ) ;
  QRect extents() { return p_extents; }
  QWidget* surface() ;
  QPoint origin() { return p_origin; }
  
  // Scrolls contents so that given point is visible.
  void ensureVisible (int cx, int cy, int mx = 50, int my = 50);
  
  // Viewport/contents position converters.
  QPoint viewportToContents ( QPoint const& pos) const { return pos + p_origin; };
  QPoint contentsToViewport ( QPoint const& pos) const { return pos - p_origin; };
  
protected:
  
  // Scrollbar stabilization.
  void updateScrollBars();
  
  // Scroll area updater.
  void scrollContentsBy(int dx, int dy);
  
  virtual bool viewportEvent(QEvent *e);
  virtual bool surfaceEvent(QEvent *e);
  virtual bool event(QEvent *e);

  friend class QTMSurface;
};

#endif // QTMSCROLLVIEW_HPP
