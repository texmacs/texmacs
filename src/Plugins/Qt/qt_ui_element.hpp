
/******************************************************************************
 * MODULE     : qt_ui_element.hpp
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QT_UI_ELEMENT_HPP
#define QT_UI_ELEMENT_HPP

#include "qt_widget.hpp"
#include "ntuple.hpp"
#include "promise.hpp"
#include "url.hpp"
#include "Scheme/object.hpp"
#include "hashmap.hpp"

#include <QAction>


/*! Construction of UI elements / widgets.

 Most of the items in the UI are constructed by this class, although in fact,
 the actual QWidgets and layout items aren't instantiated until one of
 as_qwidget(), as_qaction(), get_qmenu() or as_qlayoutmenu() is called.
 
 A UI element is first created using the factory methods create(), these store
 the parameters for the widget until they are needed upon creation. as_*()
 is typically called by another instance of qt_ui_element_rep or sometimes
 qt_plain_window_widget_rep and qt_refresh_widget_rep, who then gets ownership
 of the QObjects returned.
 
 See the documentation of qt_widget_rep for the rationale behind the four
 methods as_qaction(), get_qmenu(), as_qlayoutmenu(), as_qwidget()
 
 NOTE: although it might seem wasteful to instantiate the QObjects "on demand",
 caching makes no sense given the current infrastructure, because TeXmacs always
 discards the scheme-created widgets as soon as they exist.
*/
class qt_ui_element_rep: public qt_widget_rep {
  
    // NOTE: automatic deletion of the blackbox upon destruction will trigger
    // deletion of all the nested widgets within.
  blackbox         load;  
  QAction* cachedAction;
  
public:  
  qt_ui_element_rep (types _type, blackbox _load);
  virtual ~qt_ui_element_rep(); 

  virtual widget make_popup_widget ();
  
  virtual QAction*         as_qaction ();
  virtual QWidget*         as_qwidget ();
  virtual QLayoutItem* as_qlayoutitem ();
  virtual QMenu*            get_qmenu ();
  
  operator tree ();

  template<class X1> static widget create (types _type, X1 x1) {
    return tm_new <qt_ui_element_rep> (_type, close_box<X1>(x1));
  }
  
  template <class X1, class X2> 
  static widget create (types _type, X1 x1, X2 x2) {
    typedef pair<X1,X2> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2)));
  }
  
  template <class X1, class X2, class X3> 
  static widget create (types _type, X1 x1, X2 x2, X3 x3) {
    typedef triple<X1,X2,X3> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3)));
  }
  
  template <class X1, class X2, class X3, class X4> 
  static widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4) {
    typedef quartet<X1,X2,X3,X4> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4)));
  }
  
  template <class X1, class X2, class X3, class X4, class X5> 
  static widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) {
    typedef quintuple<X1,X2,X3,X4,X5> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4,x5)));
  }
  
};


class qt_glue_widget_rep: public qt_widget_rep {
public:
  
  tree col;
  bool hx, vx;
  SI w,h;
  
  
  qt_glue_widget_rep (tree _col, bool _hx, bool _vx, SI _w, SI _h)
  : col(_col), hx(_hx), vx(_vx), w(_w), h(_h) 
  {}
  
  qt_glue_widget_rep () {};
  
  QPixmap render ();
  
  virtual QAction *as_qaction();
  virtual QWidget *as_qwidget();
};

#endif // defined QT_UI_ELEMENT_HPP
