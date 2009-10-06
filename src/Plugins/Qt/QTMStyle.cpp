
/******************************************************************************
* MODULE     : QTMStyle.hpp
* DESCRIPTION: QT Texmacs custom style (for some elements)
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMStyle.hpp"
#include <QApplication>
#include <QStyleOptionMenuItem>
#include <iostream>

/******************************************************************************
* QTMProxyStyle (does not own *style)
******************************************************************************/

QTMProxyStyle::QTMProxyStyle (QStyle* _style):
  QStyle (), style (_style) {}

QTMProxyStyle::~QTMProxyStyle() {
  // delete style;
}

void
QTMProxyStyle::drawComplexControl (ComplexControl control, const QStyleOptionComplex* option, QPainter* painter, const QWidget* widget) const {
  style->drawComplexControl (control, option, painter, widget);
}

void
QTMProxyStyle::drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  style->drawControl (element, option, painter, widget);
}

void
QTMProxyStyle::drawItemPixmap (QPainter* painter, const QRect& rect, int alignment, const QPixmap& pixmap) const {
  style->drawItemPixmap (painter, rect, alignment, pixmap);
}

void
QTMProxyStyle::drawItemText (QPainter* painter, const QRect& rect, int alignment, const QPalette& pal, bool enabled, const QString& text, QPalette::ColorRole textRole) const {
  style->drawItemText (painter, rect, alignment, pal, enabled, text, textRole);
}

void
QTMProxyStyle::drawPrimitive (PrimitiveElement elem, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  style->drawPrimitive (elem, option, painter, widget);
}

QPixmap
QTMProxyStyle::generatedIconPixmap (QIcon::Mode iconMode, const QPixmap& pixmap, const QStyleOption* option) const {
  return style->generatedIconPixmap (iconMode, pixmap, option);
}

QStyle::SubControl
QTMProxyStyle::hitTestComplexControl (ComplexControl control, const QStyleOptionComplex* option, const QPoint& pos, const QWidget* widget) const {
  return style->hitTestComplexControl (control, option, pos, widget);
}

QRect
QTMProxyStyle::itemPixmapRect (const QRect& rect, int alignment, const QPixmap& pixmap) const {
  return style->itemPixmapRect (rect, alignment, pixmap);
}

QRect
QTMProxyStyle::itemTextRect (const QFontMetrics& metrics, const QRect& rect, int alignment, bool enabled, const QString& text) const {
  return style->itemTextRect (metrics, rect, alignment, enabled, text);
}

int
QTMProxyStyle::pixelMetric (PixelMetric metric, const QStyleOption* option, const QWidget* widget) const {
  return style->pixelMetric (metric, option, widget);
}

void
QTMProxyStyle::polish (QWidget* widget) {
  style->polish (widget);
}

void
QTMProxyStyle::polish (QApplication* app) {
  style->polish (app);
}

void
QTMProxyStyle::polish (QPalette& pal) {
  style->polish (pal);
}

QSize
QTMProxyStyle::sizeFromContents (ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget) const {
  return style->sizeFromContents (type, option, contentsSize, widget);
}

QIcon
QTMProxyStyle::standardIcon (StandardPixmap standardIcon, const QStyleOption* option, const QWidget* widget) const {
  return style->standardIcon (standardIcon, option, widget);
}

QPalette
QTMProxyStyle::standardPalette () const {
  return style->standardPalette ();
}

QPixmap
QTMProxyStyle::standardPixmap (StandardPixmap standardPixmap, const QStyleOption* option, const QWidget* widget) const {
  return style->standardPixmap (standardPixmap, option, widget);
}

int
QTMProxyStyle::styleHint (StyleHint hint, const QStyleOption* option, const QWidget* widget, QStyleHintReturn* returnData) const {
  return style->styleHint (hint, option, widget, returnData);
}

QRect
QTMProxyStyle::subControlRect (ComplexControl control, const QStyleOptionComplex* option, SubControl subControl, const QWidget* widget) const {
  return style->subControlRect (control, option, subControl, widget);
}

QRect
QTMProxyStyle::subElementRect (SubElement element, const QStyleOption* option, const QWidget* widget) const {
  return style->subElementRect (element, option, widget);
}

void
QTMProxyStyle::unpolish (QWidget* widget) {
  style->unpolish (widget);
}

void
QTMProxyStyle::unpolish (QApplication* app) {
  style->unpolish (app);
}

/******************************************************************************
* QTMStyle
******************************************************************************/

void
QTMStyle::drawPrimitive (PrimitiveElement element, const QStyleOption *option, QPainter *painter, const QWidget *widget) const {
  //  if (element == QStyle::PE_FrameStatusBarItem) return;
  if (element == QStyle::PE_FrameStatusBar) return;
  style->drawPrimitive(element,option,painter,widget);
}

int
QTMStyle::pixelMetric (PixelMetric metric, const QStyleOption *opt, const QWidget *widget) const {
  switch (metric) {
  case PM_ToolBarItemSpacing:
    return 0;
  default:
    return style->pixelMetric(metric,opt,widget);
  }
}

#if 0
void
QTMStyle::drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  switch (element) {
  case CE_MenuItem:
    if (const QStyleOptionMenuItem *mi =
       qstyleoption_cast<const QStyleOptionMenuItem *> (option)) {
      QStyleOptionMenuItem mi2(*mi);
      mi2.text= QString ("pippo");
      style->drawControl (element, &mi2, painter, widget);
      break;
    }
  default:
    style->drawControl (element, option, painter, widget);
  }
}
#endif

int
QTMStyle::styleHint (StyleHint hint, const QStyleOption* option, const QWidget* widget, QStyleHintReturn* returnData) const {
  switch (hint) {
    case SH_MenuBar_AltKeyNavigation:
      return 0;
      // Set SH_MenuBar_AltKeyNavigation to false. Typically this would be the job of the style that is selected.
      // However: That mechanism seems to be broken with some Qt versions. Furthermore, the Alt key is heavily
      // used within TeXmacs, so the menubar navigation gets in the way quite often.
    default:
      return style->styleHint (hint, option, widget, returnData);
  }
}

QTMStyle*
qtmstyle () {
  static QTMStyle* qtmstyle= NULL;
  if (!qtmstyle) {
    // cout << "custom style !!!!\n";
    QStyle* s= qApp->style();
    // s->setParent(NULL);
    // FIXME: if we change style at application level and
    // the old style is deleted the application will crash
    qtmstyle = new QTMStyle (s);
    // QApplication::setStyle(qtmstyle);
  }
  return qtmstyle;
}
