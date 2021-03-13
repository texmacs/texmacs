
/******************************************************************************
 * MODULE     : QTMStyle.hpp
 * DESCRIPTION: QT Texmacs custom style (for some elements)
 * COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_ostream.hpp"
#include "renderer.hpp"

#include "QTMStyle.hpp"

#include <QStyleOptionMenuItem>
#include <qdrawutil.h>
#include <QPainter>
#include <QPainterPath>
#include <QMainWindow>
#include <QApplication>

int
qt_zoom (int sz) {
  return (int) (retina_scale * ((double) sz));
}

QStyle*
qtmstyle () {
  static QStyle* qtmstyle = NULL;
  if (!qtmstyle)
    qtmstyle = new QTMStyle (qApp->style());
  return qtmstyle;
}

/******************************************************************************
 * QTMProxyStyle (does not own *style)
 ******************************************************************************/

QTMProxyStyle::QTMProxyStyle (QStyle* _base) : QStyle (), base (_base) { }

inline  QStyle *QTMProxyStyle::baseStyle() const {
  return ( base ? base : qApp->style() );
}

#if (QT_VERSION >= 0x050000)
int
QTMProxyStyle::layoutSpacing (QSizePolicy::ControlType control1,
                              QSizePolicy::ControlType control2,
                              Qt::Orientation orientation,
                              const QStyleOption *option,
                              const QWidget *widget) const {
  return baseStyle()->layoutSpacing(control1, control2, orientation, option, widget);
}
#endif

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

static void qtmDrawRoundedRect(QPainter *p, const QRectF &rect, qreal xRadius, qreal yRadius,
                               Qt::SizeMode mode)
{
  QRectF r = rect.normalized();
  
  if (r.isNull())
    return;
  
  if (mode == Qt::AbsoluteSize) {
    qreal w = r.width() / 2;
    qreal h = r.height() / 2;
    
    xRadius = 100 * qMin(xRadius, w) / w;
    yRadius = 100 * qMin(yRadius, h) / h;
  } else {
    if (xRadius > 100)                          // fix ranges
      xRadius = 100;
    
    if (yRadius > 100)
      yRadius = 100;
  }
  
  QPainterPath path;
  
  if (xRadius <= 0 || yRadius <= 0) {             // add normal rectangle
    path.addRect(r);
  } else {
    qreal x = r.x();
    qreal y = r.y();
    qreal w = r.width();
    qreal h = r.height();
    qreal rxx2 = w*xRadius/100;
    qreal ryy2 = h*yRadius/100;
    
#ifdef Q_OS_LINUX
    // There is a bug (probably in arcTo) for small sizes.
    // We use a rough linear approx.
    rxx2 /= 4;
    ryy2 /= 4;
    path.moveTo(x+rxx2,y);
    path.lineTo(x+w-rxx2, y);
    path.lineTo(x+w, y+ryy2);
    path.lineTo(x+w, y+h-ryy2);    
    path.lineTo(x+w-rxx2, y+h);    
    path.lineTo(x+rxx2, y+h);    
    path.lineTo(x, y+h-ryy2);    
    path.lineTo(x, y+ryy2);    
    path.closeSubpath();
#else
    path.moveTo(x+rxx2,y);
    path.arcMoveTo(x, y, rxx2, ryy2, 90);
    path.arcTo(x, y, rxx2, ryy2, 90, 90);
    path.arcTo(x, y+h-ryy2, rxx2, ryy2, 2*90, 90);
    path.arcTo(x+w-rxx2, y+h-ryy2, rxx2, ryy2, 3*90, 90);
    path.arcTo(x+w-rxx2, y, rxx2, ryy2, 0, 90);
    path.closeSubpath();
#endif
  }
  
  p->drawPath(path);
}

static void qtmDrawShadeRoundPanel(QPainter *p, const QRect &r,
                                   const QPalette &pal, bool sunken,
                                   int lineWidth, const QBrush *fill)
{
  if (r.width() == 0 || r.height() == 0)
    return;
  if (!(r.width() > 0 && r.height() > 0 && lineWidth >= 0)) {
    qWarning("qtmDrawShadeRoundPanel: Invalid parameters");
  }
  
  QColor shade = pal.dark().color();
  QColor light = pal.light().color();
  if (fill) {
    if (fill->color() == shade)
      shade = pal.shadow().color();
    if (fill->color() == light)
      light = pal.midlight().color();
  }
  
  QPen oldPen = p->pen();                        // save pen
  QBrush oldBrush = p->brush();                  // save brush  
  QRect rect(r);
  int border = 8;
  
  p->setPen(Qt::NoPen);
  
  if (sunken) {
    p->setBrush(light);
    qtmDrawRoundedRect(p,rect,border,border, Qt::AbsoluteSize);
    //    p->drawRoundedRect(rect,border,border, Qt::AbsoluteSize);
    rect.adjust(0,0,-1,-1);
    p->setBrush(shade);
    qtmDrawRoundedRect(p,rect,border,border, Qt::AbsoluteSize);
    //    p->drawRoundedRect(rect,border,border, Qt::AbsoluteSize);
    rect.adjust(1,1,0,0);
  }
  
  p->setBrush(fill ? *fill : shade);
  qtmDrawRoundedRect(p,rect,border,border, Qt::AbsoluteSize);
  //  p->drawRoundedRect(rect,border,border, Qt::AbsoluteSize);
  
  p->setPen(oldPen);                        // restore pen
  p->setBrush(oldBrush);                        // restore brush
}



void
QTMStyle::drawPrimitive (PrimitiveElement element, const QStyleOption *opt, QPainter *p, const QWidget *widget) const {
  //  if (element == QStyle::PE_FrameStatusBarItem) return;
  switch (element) {
    case PE_FrameStatusBar : 
      return;
    case PE_PanelButtonTool:
      if ((opt->state & (State_Sunken | State_On))) {
        qtmDrawShadeRoundPanel(p, opt->rect,  QPalette(opt->palette.color(QPalette::Mid)),//opt->palette,
                        (opt->state & (State_Sunken | State_On)), 2,
                        &opt->palette.brush(QPalette::Mid));
      } else {
        qtmDrawShadeRoundPanel(p, opt->rect, opt->palette, //QPalette(opt->palette.color(QPalette::Mid)),//opt->palette,
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
  switch (type) {
    case CT_LineEdit:
      sz = QSize(sz.width() + 2, sz.height() + 2);
      break;
      
    case CT_ToolButton:
      sz = QSize(sz.width() + 4, sz.height() + 6);
      break;
      
    default:
      sz = baseStyle()->sizeFromContents(type, option, contentsSize, widget);
  }
  return sz;
}


int
QTMStyle::pixelMetric (PixelMetric metric, const QStyleOption *opt, const QWidget *widget) const {
  switch (metric) {
    case PM_ToolBarItemSpacing:
      return 0;
    case PM_ToolBarIconSize:
      return 17;
  //  case PM_ToolBarFrameWidth:
  //    return 2;
    default:
      ;
  }
  return baseStyle()->pixelMetric(metric,opt,widget);
}

void
QTMStyle::drawControl (ControlElement element, const QStyleOption* option, QPainter* painter, const QWidget* widget) const {
  switch (element) {
#if 0
    case CE_MenuItem:
      if (const QStyleOptionMenuItem *mi =
          qstyleoption_cast<const QStyleOptionMenuItem *> (option)) {
        QStyleOptionMenuItem mi2(*mi);
        mi2.text= QString ("pippo");
        baseStyle()->drawControl (element, &mi2, painter, widget);
        break;
      }
#endif

#if (QT_VERSION < 0x050000)
    case CE_ToolBar: {
#ifdef UNIFIED_TOOLBAR
      if (use_unified_toolbar &&
          (widget) && (widget->windowTitle() == "mode toolbar")) {

          // For unified tool bars, draw nothing.
          if (QMainWindow * mainWindow = qobject_cast<QMainWindow *>(widget->window())) {
            if ((mainWindow->unifiedTitleAndToolBarOnMac()) && 
                (widget->parent()->objectName() != "central widget"))
              break;
          }

          //QColor mainWindowGradientBeginActive (150, 150, 150);
          //QColor mainWindowGradientBegin (200, 200, 200);
          //QColor mainWindowGradientEnd (232, 232, 232);

          QColor mainWindowGradientBeginActive (222, 222, 222);
          QColor mainWindowGradientEndActive (202, 202, 202);
          QColor mainWindowGradientBegin (236, 236, 236);
          QColor mainWindowGradientEnd (226, 226, 226);
  
          if (widget->window()->isActiveWindow())
            mainWindowGradientBegin = mainWindowGradientBeginActive;
          if (widget->window()->isActiveWindow())
            mainWindowGradientEnd = mainWindowGradientEndActive;

          // draw background gradient
          QLinearGradient linearGrad;
          if (option->state & State_Horizontal)
            linearGrad = QLinearGradient(0, option->rect.top(), 0, option->rect.bottom());
          else
            linearGrad = QLinearGradient(option->rect.left(), 0,  option->rect.right(), 0);
          
          linearGrad.setColorAt(0, mainWindowGradientBegin);
          linearGrad.setColorAt(1, mainWindowGradientEnd);
          painter->fillRect(option->rect, linearGrad);
        }
#endif // UNIFIED_TOOLBAR
    } break;
#else
  case CE_ToolBar:
#ifdef UNIFIED_TOOLBAR
    if (use_unified_toolbar &&
        (widget) && (widget->windowTitle() == "main toolbar"))
      break;
#endif // UNIFIED_TOOLBAR

    if ((widget) && (widget->windowTitle() == "main toolbar"))
      painter->fillRect(option->rect, QColor (208, 208, 208));
      //painter->fillRect(option->rect, QColor (192, 192, 192));
    else if ((widget) && (widget->windowTitle() == "mode toolbar"))
      painter->fillRect(option->rect, QColor (224, 224, 224));
      //painter->fillRect(option->rect, QColor (216, 216, 216));
    else if ((widget) && (widget->windowTitle() == "focus toolbar"))
      painter->fillRect(option->rect, QColor (240, 240, 240));
    else 
      baseStyle()->drawControl (element, option, painter, widget);
    break;
#endif
      
    default:
      baseStyle()->drawControl (element, option, painter, widget);
  }
}

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

