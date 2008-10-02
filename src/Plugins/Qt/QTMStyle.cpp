/******************************************************************************
 * MODULE     : QTMStyle.hpp
 * DESCRIPTION: QT Texmacs custom style (for some elements)
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license and comes WITHOUT
 * ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
 * If you don't have this file, write to the Free Software Foundation, Inc.,
 * 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ******************************************************************************/


#include "QTMStyle.hpp"
#include <QApplication>


#include "QTMStyle.moc"

// QTMProxyStyle does not own *style

QTMProxyStyle::QTMProxyStyle(QStyle* _style)
: QStyle(), style(_style)
{
}

QTMProxyStyle::~QTMProxyStyle()
{
//	delete style;
}

void QTMProxyStyle::drawComplexControl(ComplexControl control, const QStyleOptionComplex* option, QPainter* painter, const QWidget* widget) const
{
	style->drawComplexControl(control, option, painter, widget);
}

void QTMProxyStyle::drawControl(ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget)  const
{
	style->drawControl(element, option, painter, widget);
}

void QTMProxyStyle::drawItemPixmap(QPainter* painter, const QRect& rect, int alignment, const QPixmap& pixmap) const
{
	style->drawItemPixmap(painter, rect, alignment, pixmap);
}

void QTMProxyStyle::drawItemText(QPainter* painter, const QRect& rect, int alignment, const QPalette& pal, bool enabled, const QString& text, QPalette::ColorRole textRole) const
{
	style->drawItemText(painter, rect, alignment, pal, enabled, text, textRole);
}

void QTMProxyStyle::drawPrimitive(PrimitiveElement elem, const QStyleOption* option, QPainter* painter, const QWidget* widget) const
{
	style->drawPrimitive(elem, option, painter, widget);
}

QPixmap QTMProxyStyle::generatedIconPixmap(QIcon::Mode iconMode, const QPixmap& pixmap, const QStyleOption* option) const
{
	return style->generatedIconPixmap(iconMode, pixmap, option);
}

QStyle::SubControl QTMProxyStyle::hitTestComplexControl(ComplexControl control, const QStyleOptionComplex* option, const QPoint& pos, const QWidget* widget) const
{
	return style->hitTestComplexControl(control, option, pos, widget);
}

QRect QTMProxyStyle::itemPixmapRect(const QRect& rect, int alignment, const QPixmap& pixmap) const
{
	return style->itemPixmapRect(rect, alignment, pixmap);
}

QRect QTMProxyStyle::itemTextRect(const QFontMetrics& metrics, const QRect& rect, int alignment, bool enabled, const QString& text) const
{
	return style->itemTextRect(metrics, rect, alignment, enabled, text);
}

int QTMProxyStyle::pixelMetric(PixelMetric metric, const QStyleOption* option, const QWidget* widget) const
{
	return style->pixelMetric(metric, option, widget);
}

void QTMProxyStyle::polish(QWidget* widget)
{
	style->polish(widget);
}

void QTMProxyStyle::polish(QApplication* app)
{
	style->polish(app);
}

void QTMProxyStyle::polish(QPalette& pal)
{
	style->polish(pal);
}

QSize QTMProxyStyle::sizeFromContents(ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget) const
{
	return style->sizeFromContents(type, option, contentsSize, widget);
}

QIcon QTMProxyStyle::standardIcon(StandardPixmap standardIcon, const QStyleOption* option, const QWidget* widget) const
{
	return style->standardIcon(standardIcon, option, widget);
}

QPalette QTMProxyStyle::standardPalette() const
{
	return style->standardPalette();
}

QPixmap QTMProxyStyle::standardPixmap(StandardPixmap standardPixmap, const QStyleOption* option, const QWidget* widget) const
{
	return style->standardPixmap(standardPixmap, option, widget);
}

int QTMProxyStyle::styleHint(StyleHint hint, const QStyleOption* option, const QWidget* widget, QStyleHintReturn* returnData) const
{
	return style->styleHint(hint, option, widget, returnData);
}

QRect QTMProxyStyle::subControlRect(ComplexControl control, const QStyleOptionComplex* option, SubControl subControl, const QWidget* widget) const
{
	return style->subControlRect(control, option, subControl, widget);
}

QRect QTMProxyStyle::subElementRect(SubElement element, const QStyleOption* option, const QWidget* widget) const
{
	return style->subElementRect(element, option, widget);
}

void QTMProxyStyle::unpolish(QWidget* widget)
{
	style->unpolish(widget);
}

void QTMProxyStyle::unpolish(QApplication* app)
{
	style->unpolish(app);
}







void QTMStyle::drawPrimitive(PrimitiveElement element, const QStyleOption *option,
							 QPainter *painter, const QWidget *widget)  const
{
	//  if (element == QStyle::PE_FrameStatusBarItem) return;
	if (element == QStyle::PE_FrameStatusBar) return;
	style->drawPrimitive(element,option,painter,widget);
}  

int QTMStyle::pixelMetric(PixelMetric metric, const QStyleOption *opt, const QWidget *widget) const
{
	switch (metric) {
		case PM_ToolBarItemSpacing:
			return 0;
		default: 
			return style->pixelMetric(metric,opt,widget);
	}
}

QTMStyle *qtmstyle()
{
	static QTMStyle *qtmstyle = NULL;
	if (!qtmstyle) {
		//cout << "custom style !!!!\n";
		qtmstyle = new QTMStyle(qApp->style());
	}
	return qtmstyle;
}


