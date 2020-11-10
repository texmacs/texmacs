
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

#include "ntuple.hpp"
#include "promise.hpp"
#include "url.hpp"
#include "scheme.hpp"
#include "hashmap.hpp"

#include "qt_widget.hpp"
#include "QTMMenuHelper.hpp"

#include <QPointer>
#include <QWidgetItem>

class QMenu;
class QAction;


/*! Construction of UI elements / widgets.

 Most of the items in the UI are constructed by this class, although in fact,
 the actual QWidgets and layout items aren't instantiated ("compiled") until
 one of as_qwidget(), as_qaction(), get_qmenu() or as_qlayoutmenu() is called.
 
 A UI element is first created using the factory methods create(), these store
 the parameters for the widget until they are needed upon creation. as_*()
 is typically called by another instance of qt_ui_element_rep or sometimes
 qt_plain_window_widget_rep and qt_refresh_widget_rep, who then gets ownership
 of the QObjects returned.
 
 See the documentation of qt_widget_rep for the rationale behind the four
 methods as_qaction(), get_qmenu(), as_qlayoutmenu(), as_qwidget()
 
 NOTE: although we store the QWidgets in qt_widget_rep::qwid after creating 
 them, this caching achieves nothing for the current infrastructure, because
 TeXmacs always discards widgets which are not menus or toolbars.
*/
class qt_ui_element_rep: public qt_widget_rep {
  
    // NOTE: automatic deletion of the blackbox upon destruction will trigger
    // deletion of all the nested widgets within.
  blackbox                  load;
  QPointer<QAction> cachedAction;
  QList<QAction*>* cachedActionList;
    
public:  
  qt_ui_element_rep (types _type, blackbox _load);
  virtual ~qt_ui_element_rep(); 

  virtual widget popup_window_widget (string s);
  
  virtual QAction*         as_qaction ();
  virtual QWidget*         as_qwidget ();
  virtual QLayoutItem*     as_qlayoutitem ();
  virtual QList<QAction*>* get_qactionlist();

  operator tree ();

  template<class X1> static qt_widget create (types _type, X1 x1) {
    return tm_new <qt_ui_element_rep> (_type, close_box<X1>(x1));
  }
  
  template <class X1, class X2> 
  static qt_widget create (types _type, X1 x1, X2 x2) {
    typedef pair<X1,X2> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2)));
  }
  
  template <class X1, class X2, class X3> 
  static qt_widget create (types _type, X1 x1, X2 x2, X3 x3) {
    typedef triple<X1,X2,X3> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3)));
  }
  
  template <class X1, class X2, class X3, class X4> 
  static qt_widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4) {
    typedef quartet<X1,X2,X3,X4> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4)));
  }
  
  template <class X1, class X2, class X3, class X4, class X5> 
  static qt_widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) {
    typedef quintuple<X1,X2,X3,X4,X5> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4,x5)));
  }
  
protected:
  static blackbox get_payload (qt_widget qtw, types check_type = none);
};


/*! A rectangular separator widget with a colored background. */
class qt_glue_widget_rep: public qt_widget_rep {
public:
  tree col;
  bool hx, vx;
  SI w,h;
  
  qt_glue_widget_rep (tree _col, bool _hx, bool _vx, SI _w, SI _h)
    : col (_col), hx (_hx), vx (_vx), w (_w), h (_h) { }
  
  qt_glue_widget_rep () { }
  
  QPixmap render ();
  
  virtual QAction* as_qaction ();
  virtual QWidget* as_qwidget ();
};

/*! A wrapper widget executing a quit command upon SLOT_DESTROY. */
class qt_wrapped_widget_rep : public qt_widget_rep {
  widget tmwid;
  command quit;
public:
  qt_wrapped_widget_rep (widget _tmwid, command _quit)
    : tmwid (_tmwid), quit (_quit) {
    add_child (tmwid);
  }
  QWidget* as_qwidget () { return concrete(tmwid)->as_qwidget(); }
  void send (slot s, blackbox val) {
    switch (s) {
      case SLOT_DESTROY:
        if (! is_nil (quit)) quit ();
        quit = command();
        break;
      default:
        return;
    }
    qt_widget_rep::send (s, val);
    if (DEBUG_QT_WIDGETS)
      cout << "qt_wrapped_widget_rep: sent " << slot_name (s)
           << "\t\tto widget\t" << type_as_string() << LF;
  }

};

/*! Ad-hoc command to be used with choice widgets.
 
 The command associated with a qt_ui_element::choice_widget has one parameter
 (a list of selected items).
 For the reason to be of this class, see \sa qt_toggle_command_rep.
 \sa qt_ui_element, , qt_ui_element_rep::as_qwidget, qt_ui_element_rep::choice_widget
 */
class qt_choice_command_rep: public command_rep {
  QPointer<QTMListView> qwid;
  command                cmd;
  bool              multiple;  //<! Are multiple choices allowed in the widget?
  bool              filtered;
  
public:
  qt_choice_command_rep (QTMListView* w, command c, bool m, bool f=false)
  : qwid(w), cmd(c), multiple(m), filtered(f) {}
  
  virtual void apply () {
    if (qwid) {
      QStringList selected;
      foreach (QModelIndex item, qwid->selectionModel()->selectedIndexes())
      selected << qwid->model()->data(item).toString();
      
      object l= null_object ();
      if (multiple)
        for (int i = selected.size() - 1; i >= 0; --i)
          l = cons (from_qstring (selected[i]), l);
      else if (selected.size() > 0)
        l = from_qstring (selected[0]);
      else
        l = "";
      
      if (filtered)
        cmd (list_object (l, from_qstring (qwid->filter()->filterRegExp().pattern())));
      else
        cmd (list_object (l));
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Choice"; }
};
#endif // defined QT_UI_ELEMENT_HPP
