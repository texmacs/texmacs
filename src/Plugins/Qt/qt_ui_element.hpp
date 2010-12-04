
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

/*******************************************************************************
 * ui element widget  
 *******************************************************************************/

class qt_ui_element_rep: public qt_widget_rep {
public:
  
  enum types {
    horizontal_menu, vertical_menu, horizontal_list, vertical_list,
    tile_menu, minibar_menu, menu_separator, menu_group, 
    pulldown_button, pullright_button, menu_button,
    balloon_widget, text_widget, xpm_widget
  } ;
  
  types type;
  blackbox load;
  
  qt_ui_element_rep (types _type, blackbox _load) 
  : type(_type), load(_load)  {};
  
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
  virtual widget plain_window_widget (string s);
  virtual QAction* as_qaction ();

  template<class X1> static widget create (types _type, X1 x1) {
    return tm_new <qt_ui_element_rep> (_type, close_box<X1>(x1));
  }
  
  template <class X1, class X2> 
  static widget create (types _type, X1 x1, X2 x2) {
    typedef pair<X1,X2> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T>(T(x1,x2)));
  }
  
  template <class X1, class X2, class X3> 
 static widget create (types _type, X1 x1, X2 x2, X3 x3) {
    typedef triple<X1,X2,X3> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T>(T(x1,x2,x3)));
  }
  
  template <class X1, class X2, class X3, class X4> 
  static widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4) {
    typedef quartet<X1,X2,X3,X4> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T>(T(x1,x2,x3,x4)));
  }
  
  template <class X1, class X2, class X3, class X4, class X5> 
    static widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) {
    typedef quintuple<X1,X2,X3,X4,X5> T;
    return tm_new <qt_ui_element_rep> (_type, close_box<T>(T(x1,x2,x3,x4,x5)));
  }
  
  
  static widget make_horizontal_menu (array<widget> arr) { return create (horizontal_menu, arr); }
  static widget make_vertical_menu (array<widget> arr)  { return create (vertical_menu, arr); }
  static widget make_horizontal_list (array<widget> arr) { return create (horizontal_list, arr); }
  static widget make_vertical_list (array<widget> arr) { return create (vertical_list, arr); }
  static widget make_tile_menu (array<widget> a, int cols) { return create (tile_menu, a, cols); }
  static widget make_minibar_menu (array<widget> arr) { return create (minibar_menu, arr); }
  static widget make_menu_separator (bool vertical) { return create (menu_separator, vertical); }
  static widget make_menu_group (string name, int style) { return create (menu_group , name, style); }
  static widget make_pulldown_button (widget w, promise<widget> pw) { return create (pulldown_button, w, pw); }
  static widget make_pullright_button (widget w, promise<widget> pw) { return create (pullright_button, w, pw); }
  static widget make_menu_button (widget w, command cmd, string pre, string ks, int style) { return create (menu_button, w, cmd, pre, ks, style); }
  static widget make_balloon_widget (widget w, widget help) { return create (balloon_widget, w, help); }
  static widget make_text_widget (string s, int style, color col, bool tsp) { return create (text_widget, s, style, col, tsp); }
  static widget make_xpm_widget (url file_name) { return create (xpm_widget, file_name); }
};


#endif // defined QT_UI_ELEMENT_HPP
