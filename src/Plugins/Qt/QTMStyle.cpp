
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
#include "tm_ostream.hpp"
#include <qdrawutil.h>

/******************************************************************************
* QTMProxyStyle (does not own *style)
******************************************************************************/

QTMProxyStyle::QTMProxyStyle (QStyle* _base):
  QStyle (), base (_base) {}

QTMProxyStyle::~QTMProxyStyle() {
  // delete style;
}

inline  QStyle *QTMProxyStyle::baseStyle() const {
  return ( base ? base : qApp->style() );
}


void
QTMProxyStyle::drawComplexControl (ComplexControl control, const QStyleOptionComplex* option, QPainter* painter, const QWidget* widget) const {
  baseStyle()->drawComplexControl (control, option, painter, widget);
}

void
QTMProxyStyle::drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  baseStyle()->drawControl (element, option, painter, widget);
}

void
QTMProxyStyle::drawItemPixmap (QPainter* painter, const QRect& rect, int alignment, const QPixmap& pixmap) const {
  baseStyle()->drawItemPixmap (painter, rect, alignment, pixmap);
}

void
QTMProxyStyle::drawItemText (QPainter* painter, const QRect& rect, int alignment, const QPalette& pal, bool enabled, const QString& text, QPalette::ColorRole textRole) const {
  baseStyle()->drawItemText (painter, rect, alignment, pal, enabled, text, textRole);
}

void
QTMProxyStyle::drawPrimitive (PrimitiveElement elem, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  baseStyle()->drawPrimitive (elem, option, painter, widget);
}

QPixmap
QTMProxyStyle::generatedIconPixmap (QIcon::Mode iconMode, const QPixmap& pixmap, const QStyleOption* option) const {
  return baseStyle()->generatedIconPixmap (iconMode, pixmap, option);
}

QStyle::SubControl
QTMProxyStyle::hitTestComplexControl (ComplexControl control, const QStyleOptionComplex* option, const QPoint& pos, const QWidget* widget) const {
  return baseStyle()->hitTestComplexControl (control, option, pos, widget);
}

QRect
QTMProxyStyle::itemPixmapRect (const QRect& rect, int alignment, const QPixmap& pixmap) const {
  return baseStyle()->itemPixmapRect (rect, alignment, pixmap);
}

QRect
QTMProxyStyle::itemTextRect (const QFontMetrics& metrics, const QRect& rect, int alignment, bool enabled, const QString& text) const {
  return baseStyle()->itemTextRect (metrics, rect, alignment, enabled, text);
}

int
QTMProxyStyle::pixelMetric (PixelMetric metric, const QStyleOption* option, const QWidget* widget) const {
  return baseStyle()->pixelMetric (metric, option, widget);
}

void
QTMProxyStyle::polish (QWidget* widget) {
  baseStyle()->polish (widget);
}

void
QTMProxyStyle::polish (QApplication* app) {
  baseStyle()->polish (app);
}

void
QTMProxyStyle::polish (QPalette& pal) {
  baseStyle()->polish (pal);
}

QSize
QTMProxyStyle::sizeFromContents (ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget) const {
  return baseStyle()->sizeFromContents (type, option, contentsSize, widget);
}

QIcon
QTMProxyStyle::standardIcon (StandardPixmap standardIcon, const QStyleOption* option, const QWidget* widget) const {
  return baseStyle()->standardIcon (standardIcon, option, widget);
}

QPalette
QTMProxyStyle::standardPalette () const {
  return baseStyle()->standardPalette ();
}

QPixmap
QTMProxyStyle::standardPixmap (StandardPixmap standardPixmap, const QStyleOption* option, const QWidget* widget) const {
  return baseStyle()->standardPixmap (standardPixmap, option, widget);
}

int
QTMProxyStyle::styleHint (StyleHint hint, const QStyleOption* option, const QWidget* widget, QStyleHintReturn* returnData) const {
  return baseStyle()->styleHint (hint, option, widget, returnData);
}

QRect
QTMProxyStyle::subControlRect (ComplexControl control, const QStyleOptionComplex* option, SubControl subControl, const QWidget* widget) const {
  return baseStyle()->subControlRect (control, option, subControl, widget);
}

QRect
QTMProxyStyle::subElementRect (SubElement element, const QStyleOption* option, const QWidget* widget) const {
  return baseStyle()->subElementRect (element, option, widget);
}

void
QTMProxyStyle::unpolish (QWidget* widget) {
  baseStyle()->unpolish (widget);
}

void
QTMProxyStyle::unpolish (QApplication* app) {
  baseStyle()->unpolish (app);
}

/******************************************************************************
* QTMStyle
******************************************************************************/

void
QTMStyle::drawPrimitive (PrimitiveElement element, const QStyleOption *opt, QPainter *p, const QWidget *widget) const {
  //  if (element == QStyle::PE_FrameStatusBarItem) return;
  switch (element) {
    case PE_FrameStatusBar : 
      return;
    case PE_PanelButtonTool:
      if ((opt->state & (State_Sunken | State_On))) {
      qDrawShadePanel(p, opt->rect,  QPalette(opt->palette.color(QPalette::Mid)),//opt->palette,
                      (opt->state & (State_Sunken | State_On)), 2,
                      &opt->palette.brush(QPalette::Mid));
      } else {
        qDrawShadePanel(p, opt->rect, opt->palette, //QPalette(opt->palette.color(QPalette::Mid)),//opt->palette,
                        (opt->state & (State_Sunken | State_On)), 0,
                        &opt->palette.brush(QPalette::Mid));
      }
      return;
    default:
      ;
  }
  baseStyle()->drawPrimitive(element,opt,p,widget);  
}


void 
QTMStyle::drawComplexControl (ComplexControl cc, const QStyleOptionComplex* opt, QPainter* p, const QWidget* widget) const {
  switch (cc) {
    case CC_ToolButton:
      if (const QStyleOptionToolButton *toolbutton
          = qstyleoption_cast<const QStyleOptionToolButton *>(opt)) {
        QRect button, menuarea;
        button = proxy()->subControlRect(cc, toolbutton, SC_ToolButton, widget);
        menuarea = proxy()->subControlRect(cc, toolbutton, SC_ToolButtonMenu, widget);
        
        State bflags = toolbutton->state & ~State_Sunken;
        
        if (bflags & State_AutoRaise) {
          if (!(bflags & State_MouseOver) || !(bflags & State_Enabled)) {
            bflags &= ~State_Raised;
          }
        }
        State mflags = bflags;
        if (toolbutton->state & State_Sunken) {
          if (toolbutton->activeSubControls & SC_ToolButton)
            bflags |= State_Sunken;
          mflags |= State_Sunken;
        }
        
        QStyleOption tool(0);
        tool.palette = toolbutton->palette;
        if (toolbutton->subControls & SC_ToolButton) {
          if (bflags & (State_Sunken | State_On | State_Raised)) {
            tool.rect = button;
            tool.state = bflags;
            proxy()->drawPrimitive(PE_PanelButtonTool, &tool, p, widget);
          }
        }
        
        if (toolbutton->state & State_HasFocus) {
          QStyleOptionFocusRect fr;
          fr.QStyleOption::operator=(*toolbutton);
          fr.rect.adjust(3, 3, -3, -3);
          if (toolbutton->features & QStyleOptionToolButton::MenuButtonPopup)
            fr.rect.adjust(0, 0, -proxy()->pixelMetric(QStyle::PM_MenuButtonIndicator,
                                                       toolbutton, widget), 0);
          proxy()->drawPrimitive(PE_FrameFocusRect, &fr, p, widget);
        }
        QStyleOptionToolButton label = *toolbutton;
        label.state = bflags;
        int fw = proxy()->pixelMetric(PM_DefaultFrameWidth, opt, widget);
        label.rect = button.adjusted(fw, fw, -fw, -fw);
        proxy()->drawControl(CE_ToolButtonLabel, &label, p, widget);
        
        if (toolbutton->subControls & SC_ToolButtonMenu) {
          tool.rect = menuarea;
          tool.state = mflags;
          if (mflags & (State_Sunken | State_On | State_Raised))
            proxy()->drawPrimitive(PE_IndicatorButtonDropDown, &tool, p, widget);
          proxy()->drawPrimitive(PE_IndicatorArrowDown, &tool, p, widget);
        } else if ((toolbutton->features & QStyleOptionToolButton::HasMenu) 
                   && (mflags & State_MouseOver))
        {
          int mbi = proxy()->pixelMetric(PM_MenuButtonIndicator, toolbutton, widget);
          QRect ir = toolbutton->rect;
          QStyleOptionToolButton newBtn = *toolbutton;
          newBtn.rect = QRect(ir.right() + 5 - mbi, ir.y() + ir.height() - mbi + 4, mbi - 6, mbi - 6);
          proxy()->drawPrimitive(PE_IndicatorArrowDown, &newBtn, p, widget);
        }
      }
      break;
    default:
      baseStyle()->drawComplexControl (cc, opt, p, widget);
  }
}

      
      
      
QSize 
QTMStyle::sizeFromContents (ContentsType type, const QStyleOption* option, const QSize& contentsSize, const QWidget* widget) const {
  QSize sz(contentsSize);
  if (type == QStyle::CT_ToolButton) {
    sz = QSize(sz.width() + 4, sz.height() + 6);
    return sz;
  }
  return baseStyle()->sizeFromContents(type, option, contentsSize, widget);
}


int
QTMStyle::pixelMetric (PixelMetric metric, const QStyleOption *opt, const QWidget *widget) const {
  switch (metric) {
  case PM_ToolBarItemSpacing:
    return 0;
  default:
    return baseStyle()->pixelMetric(metric,opt,widget);
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
      baseStyle()->drawControl (element, &mi2, painter, widget);
      break;
    }
  default:
    baseStyle()->drawControl (element, option, painter, widget);
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
      return baseStyle()->styleHint (hint, option, widget, returnData);
  }
}

QStyle*
qtmstyle () {
  static QStyle* qtmstyle= NULL;
  if (!qtmstyle) {
    qtmstyle = new QTMStyle ();
  }
  if (!qtmstyle) {
    qtmstyle = qApp->style ();
  }
  return qtmstyle;
}
