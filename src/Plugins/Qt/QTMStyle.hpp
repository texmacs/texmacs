
/******************************************************************************
* MODULE     : QTMStyle.hpp
* DESCRIPTION: QT Texmacs custom style (for some elements)
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTMSTYLE_HPP
#define QTMSTYLE_HPP

#include <QStyle>

// custom style to override some Qt "features" like
// frame around widgets in the status bar

class QTMProxyStyle: public QStyle {
  Q_OBJECT
protected:
  QStyle* base;
                
public:
  explicit QTMProxyStyle (QStyle* _base = NULL);
  ~QTMProxyStyle ();

  QStyle *baseStyle() const;
  
  void drawComplexControl (ComplexControl control, const QStyleOptionComplex* option, QPainter* painter, const QWidget* widget = 0) const;
  void drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget = 0)  const;
  void drawItemPixmap (QPainter* painter, const QRect& rect, int alignment, const QPixmap& pixmap) const;
  void drawItemText (QPainter* painter, const QRect& rect, int alignment, const QPalette& pal, bool enabled, const QString& text, QPalette::ColorRole textRole = QPalette::NoRole) const;
  void drawPrimitive (PrimitiveElement elem, const QStyleOption* option, QPainter* painter, const QWidget* widget = 0) const;
  QPixmap generatedIconPixmap (QIcon::Mode iconMode, const QPixmap& pixmap, const QStyleOption* option) const;
  SubControl hitTestComplexControl (ComplexControl control, const QStyleOptionComplex* option, const QPoint& pos, const QWidget* widget = 0) const;
  QRect itemPixmapRect (const QRect& rect, int alignment, const QPixmap& pixmap) const;
  QRect itemTextRect (const QFontMetrics& metrics, const QRect& rect, int alignment, bool enabled, const QString& text) const;
  int pixelMetric (PixelMetric metric, const QStyleOption* option = 0, const QWidget* widget = 0) const;
  void polish (QWidget* widget);
  void polish (QApplication* app);
  void polish (QPalette& pal);
  QSize sizeFromContents (ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget = 0) const;
  QIcon standardIcon (StandardPixmap standardIcon, const QStyleOption* option = 0, const QWidget* widget = 0) const;
  QPalette standardPalette () const;
  QPixmap standardPixmap (StandardPixmap standardPixmap, const QStyleOption* option = 0, const QWidget* widget = 0) const;
  int styleHint (StyleHint hint, const QStyleOption* option = 0, const QWidget* widget = 0, QStyleHintReturn* returnData = 0) const;
  QRect subControlRect (ComplexControl control, const QStyleOptionComplex* option, SubControl subControl, const QWidget* widget = 0) const;
  QRect subElementRect (SubElement element, const QStyleOption* option, const QWidget* widget = 0) const;
  void unpolish (QWidget* widget);
  void unpolish (QApplication* app);
};

class QTMStyle: public QTMProxyStyle {
  Q_OBJECT

public:
  inline QTMStyle (QStyle* _style = NULL): QTMProxyStyle (_style) {}
  inline ~QTMStyle () {}

  void drawComplexControl (ComplexControl control, const QStyleOptionComplex* option, QPainter* painter, const QWidget* widget = 0) const;
  void drawPrimitive (PrimitiveElement element, const QStyleOption *option, QPainter *painter, const QWidget *widget) const;
  int pixelMetric (PixelMetric metric, const QStyleOption *opt, const QWidget *widget) const;
  QSize sizeFromContents (ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget = 0) const;
 // void drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget = 0)  const;
  int styleHint (StyleHint hint, const QStyleOption* option = 0, const QWidget* widget = 0, QStyleHintReturn* returnData = 0) const;
};

QStyle* qtmstyle ();

#endif // QTMSTYLE_HPP
