
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

/*! Scroll view widget.

 The current structure of the central texmacs widget (the canvas) is the 
 following: the canvas is a derived class of QTMScrollView which being
 a QAbstractScrollArea owns a central widget called the "viewport".
 This setup has been augmented via another widget child of the scrollview 
 which we call the "surface", with the purpose of centering the working area. 
 See the documentation for QTMSurface for more info on this.
*/

class QTMScrollView : public QAbstractScrollArea {
  Q_OBJECT

  QWidget* p_surface;   // Actual drawing area, centered (or not) in the scrollarea
  
public:
  
  QTMScrollView (QWidget *_parent = NULL);

  QWidget* surface () { return p_surface; }
  
protected:
  
  virtual bool viewportEvent (QEvent *e);
  virtual bool surfaceEvent (QEvent *e);

  friend class QTMSurface;
};


#endif // QTMSCROLLVIEW_HPP
