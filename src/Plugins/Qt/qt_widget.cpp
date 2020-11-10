
/******************************************************************************
* MODULE     : qt_widget.cpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_utilities.hpp"
#include "qt_renderer.hpp"

#include "qt_widget.hpp"
#include "qt_ui_element.hpp"
#include "qt_simple_widget.hpp"
#include "qt_tm_widget.hpp"
#include "qt_window_widget.hpp"
#include "qt_chooser_widget.hpp"
#include "qt_color_picker_widget.hpp"
#include "qt_printer_widget.hpp"

#include "window.hpp"

#include <QWidget>
#include <QWidgetItem>
#include "QTMMenuHelper.hpp"
#include "QTMWindow.hpp"


/******************************************************************************
 * qt_widget_rep: the base widget for the Qt port.
 ******************************************************************************/

template<> void
tm_delete<qt_widget_rep> (qt_widget_rep* ptr) {
  void *mem= ptr->derived_this ();
  ptr -> ~qt_widget_rep ();
  fast_delete (mem);
}

static long widget_counter = 0;

qt_widget_rep::qt_widget_rep(types _type, QWidget* _qwid)
  : widget_rep (), id (widget_counter++), qwid (_qwid), type (_type)
{
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_widget_rep: created a " << type_as_string() << LF;
}

qt_widget_rep::~qt_widget_rep() { 
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "~qt_widget_rep: deleted a " << type_as_string() << LF;

  // DON'T DO THIS! (several qt_widget_rep may have the same underlying QWidget)
  // UPD: really? when? And it wouldn't matter since we are using a guarded
  // pointer
//  delete qwid;
}

void
qt_widget_rep::add_child (widget w) {
  children << w;
}

void
qt_widget_rep::add_children (array<widget> a) {
  children << a;
}

void
qt_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_KEYBOARD_FOCUS:
    {
      check_type<bool> (val, s);
      bool focus = open_box<bool> (val);
      if (focus && qwid && !qwid->hasFocus())
        qwid->setFocus (Qt::OtherFocusReason);
    }
      break;
    case SLOT_KEYBOARD_FOCUS_ON:
    {
      string field = open_box<string>(val);
      if (qwid) {
        QWidget* target = qwid->findChild<QWidget*> (to_qstring (field));
        if (target == NULL)
          target = qwid->findChild<QWidget*> ("default focus target");
        if (target) target->setFocus(Qt::OtherFocusReason);
      }
      /* FIXME: This would be better than using QObject::findChild but it won't
       work because the array of children is only sloppily used at best
       (meaning some objects are not assigned as children of others...)
       If this is ever made to work, we'll want to handle SLOT_KEYBOARD_FOCUS_ON
       inside qt_input_text_widget_rep and possibly others

      for (int i = 0; i < N(children); ++i)
        if (!is_nil(children[i])) children[i]->send (s, val);
       */
    }
      break;
    case SLOT_NAME:
    {
        // CHECK ME!
      widget win = qt_window_widget_rep::widget_from_qwidget (qwid);
      if (! is_nil (win)) win->send (SLOT_NAME, val);
    }
      break;
    case SLOT_DESTROY:
    {
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "Resending to " << N(children) << " children" << LF;
      for (int i = 0; i < N(children); ++i)
        if (!is_nil(children[i])) children[i]->send (s, val);
    }
      break;
    default:
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "qt_widget_rep::send(), unhandled " << slot_name (s)
                      << " for widget of type: " << type_as_string() << ".\n";
  }
}

/*! Returns the actual QWidget underlying this qt_widget_rep.
 
 Implementations of this method must comply with the following:
 
  * The policy is to give ownership of the object to the caller.
  * The pointer qt_widget_rep::qwid must be set to the returned object.
  * A new QWidget is built on each call.

 */
inline QWidget*
qt_widget_rep::as_qwidget () {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_widget_rep::as_qwidget() for "
                  << type_as_string() << LF;
  return qwid;
}

/*! Returns the QAction associated to this qt_widget_rep.
 
 The default action is of type QTMAction, does nothing and is disabled.
 See the remarks about memory management and menu instantiation and insertion
 in the comments to class qt_menu_rep.
 */
QAction*
qt_widget_rep::as_qaction() {
  QAction* a = new QTMAction (NULL); 
  a->setEnabled (false);
  return a;
}

/*! Wraps the underlying QWidget into a QWidgetItem and returns it.

 The policy is to give ownership of the object to the caller.
*/
inline QLayoutItem*
qt_widget_rep::as_qlayoutitem () {
  return new QWidgetItem (as_qwidget ()); 
}


QList<QAction*>*
qt_widget_rep::get_qactionlist() {
  return NULL;
}


/*! Returns the widget as a window.
 
 Each TeXmacs widget can at some point be asked to present itself into a window.
 The scheme-originating function window_create () expects this method in every
 widget. 

 This default implementation constructs a wrapper qt_window_widget for the 
 widget and returns it. This wrapper will hold a new QTMPlainWindow object
 which will manage close events and take ownership of the original QWidget.
 qt_window_widget owns the QTMPlainWindow and is responsible for its deletion.
 
 The default implementation should suffice in most cases.

 \param name A unique identifier for the window. This is *not* the window title.
 \param quit A command to be executed when the window closes.
 \return The new qt_window_widget.
*/
widget
qt_widget_rep::plain_window_widget (string name, command quit, int border) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "qt_widget_rep::plain_window_widget() around a "
                  << type_as_string() << LF;

  QTMPlainWindow* win = new QTMPlainWindow (0);
  QLayoutItem*     li = as_qlayoutitem();
  if (li) {
    QLayout* l = li->layout();
    if (! l) {
      l = new QVBoxLayout (win);
      l->addItem (li); // Layout owns the QLayoutItem
    }
    win->setLayout (l);// Transfers ownership of QWidgets in QLayoutItems to win
  } else {
    QWidget* qw = as_qwidget();
    if (qw) {
      QLayout* l = new QVBoxLayout (win);
      win->setLayout (l); // And the QLayout to the QTMPlainWindow.
      l->addWidget (qw);  // qw now belongs to the QWidget with the layout (win)
    } else {
      FAILED ("attempt to create a window around a nil QWidget");
    }
  }
  
  int l,t,r,b;
  win->layout()->getContentsMargins (&l, &t, &r, &b);
  win->layout()->setContentsMargins (l+border, t+border, r+border, b+border);
  win->setWindowTitle (to_qstring (name));  // HACK: remove me (see bug#40837)
  
  qt_window_widget_rep* wid = tm_new<qt_window_widget_rep> (win, name, quit);
  wid->add_child (this);

  return widget (wid);
}

/*! Interface for the creation of popups.
 FIXME: the check below should be unnecessary, but current design is ugly.
 */
widget
qt_widget_rep::popup_window_widget (string s) {
  widget wid= tm_new<qt_popup_widget_rep> ((widget_rep*)this, command());
  ASSERT(concrete(wid) != this, "Loop in call to popup_window_widget()");
  return concrete(wid)->popup_window_widget(s);
}

widget
qt_widget_rep::tooltip_window_widget (string s) {
  widget wid= tm_new<qt_popup_widget_rep> ((widget_rep*)this, command());
  ASSERT(concrete(wid) != this, "Loop in call to tooltip_window_widget()");
  return concrete(wid)->tooltip_window_widget(s);
}

tm_ostream& operator << (tm_ostream& out, qt_widget w) {
  return out << "qt_widget of type: " << w.rep->type_as_string();
}


/******************************************************************************
* Global functions we export for the creation of windowed widgets by TeXmacs
******************************************************************************/

/*! Creates a decorated window using the given widget.

 Each widget type may choose how to present itself as a window, by 
 reimplementing qt_widget_rep::plain_window_widget(), although the base class
 qt_widget_rep provides a default implementation which suffices in most cases.
 See its documentation.

 \param w    The contents of the window.
 \param name A unique identifier for the window. This is *not* the window title.
 \param q    A command to be executed when the window closes.
*/
widget
plain_window_widget (widget w, string name, command q) {
  widget win= concrete(w)->plain_window_widget (name, q);
  if (name != "popup") {
    int xx, yy, ww, hh;
    xx = yy = ww = hh = -1;
    get_preferred_position (name, xx, yy);
    get_preferred_size (name, ww, hh);
    if (xx != -1)
      set_position (win, xx, yy);
    if (ww != -1)
      set_size (win, ww, hh);
  }
  return win;
}

/*! Creates an undecorated window with name s and contents w.
 */
widget
popup_window_widget (widget w, string s) {
  return concrete(w)->popup_window_widget (s);
}

widget
tooltip_window_widget (widget w, string s) {
  return concrete(w)->tooltip_window_widget (s);
}

/*! A factory for a popup widget container whose contents are to be unmapped as 
 soon as the mouse exits the widget. 
 
 There are currently two kinds of popup widgets: those whose underlying QWidget
 is a QMenu, and those that hold any sort of QWidget. The former are used in
 edit_mouse.cpp to implement a contextual menu in the canvas and are implemented
 using qt_menu_rep; the latter are used for help-balloons and are implemented
 using qt_popup_widget_rep.
 
 \param w The widget to be placed in the popup. It will be deleted after the
 mouse leaves the popup.
 \return The popup widget.
 */
widget
popup_widget (widget w) {
  // Do nothing. In Qt all the action is taken care of by
  // the enveloping popup window.
  // It is the case that all the popup_widgets are inside popup_window_widgets
  return w;
}

/*! Destroys a window as created via qt_window_widget.

 In the QT implementation explicitly destroying window widgets should
 not be necessary since the widget itself destroys the QWidget as soon as
 its destructor is called. No memory leak should be caused by this trivial
 implementation.
 */
void
destroy_window_widget (widget w) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "destroy_window_widget() on "
                  << static_cast<qt_widget_rep*>(w.rep)->type_as_string() << LF;
}


/******************************************************************************
 * TeXmacs interface for the creation of widgets. 
 * See Graphics/Gui/widget.hpp for comments. 
 ******************************************************************************/

widget horizontal_menu (array<widget> a) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::horizontal_menu, a);
  wid->add_children (a);
  return abstract (wid);
}
widget vertical_menu (array<widget> a)  {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::vertical_menu, a);
  wid->add_children (a);
  return abstract (wid);
}
widget horizontal_list (array<widget> a) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::horizontal_list, a);
  wid->add_children (a);
  return abstract (wid);
}
widget vertical_list (array<widget> a) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::vertical_list, a);
  wid->add_children (a);
  return abstract (wid);
}
widget aligned_widget (array<widget> lhs, array<widget> rhs, SI hsep, SI vsep,
                       SI lpad, SI rpad) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::aligned_widget,
                                    lhs, rhs, coord4 (hsep, vsep, lpad, rpad));
  wid->add_children (lhs);
  wid->add_children (rhs);
  return abstract (wid);
}
widget tabs_widget (array<widget> tabs, array<widget> bodies) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::tabs_widget,
                                             tabs, bodies);
  wid->add_children (tabs);
  wid->add_children (bodies);
  return abstract (wid);
}
widget icon_tabs_widget (array<url> us, array<widget> ts, array<widget> bs) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::icon_tabs_widget,
                                    us, ts, bs);
  wid->add_children (ts);
  wid->add_children (bs);
  return abstract (wid);
}
widget wrapped_widget (widget w, command cmd) {
  return tm_new<qt_wrapped_widget_rep> (w, cmd);
}
widget tile_menu (array<widget> a, int cols) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::tile_menu, a, cols);
  wid->add_children (a);
  return abstract (wid);
}
widget minibar_menu (array<widget> a) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::minibar_menu, a);
  wid->add_children (a);
  return abstract (wid);
}
widget menu_separator (bool vertical) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::menu_separator,
                                             vertical);
  return abstract (wid);
}
widget menu_group (string name, int style) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::menu_group,
                                             name, style);
  return abstract (wid);
}
widget pulldown_button (widget w, promise<widget> pw) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::pulldown_button,
                                             w, pw);
    // FIXME: the promise widget isn't added to the children when it's evaluated
    //  wid->add_child (??);
  return abstract(wid);
}
widget pullright_button (widget w, promise<widget> pw) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::pullright_button,
                                             w, pw);
    // FIXME: the promise widget isn't added to the children when it's evaluated
    //  wid->add_child (??);
  return abstract(wid);
}
widget menu_button (widget w, command cmd, string pre, string ks, int style) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::menu_button,
                                             w, cmd, pre, ks, style);
  wid->add_child (w);
  return abstract (wid);
}
widget balloon_widget (widget w, widget help) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::balloon_widget,
                                             w, help);
  wid->add_child (w);
  return abstract (wid);
}
widget text_widget (string s, int style, color col, bool tsp) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::text_widget,
                                             s, style, col, tsp);
  return abstract (wid);
}
widget xpm_widget (url file_name) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::xpm_widget,
                                             file_name);
  return abstract (wid);
}
widget toggle_widget (command cmd, bool on, int style) { 
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::toggle_widget,
                                             cmd, on, style);
  return abstract (wid);
}
widget enum_widget (command cmd, array<string> vals, string val, int style,
                    string width) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::enum_widget,
                                             cmd, vals, val, style, width);
  return abstract (wid);
}
widget choice_widget (command cmd, array<string> vals, array<string> chosen) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::choice_widget,
                                             cmd, vals, chosen, true);
  return abstract (wid);
}
widget choice_widget (command cmd, array<string> vals, string cur) {
  array<string> chosen (1);
  chosen[0]= cur;
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::choice_widget,
                                             cmd, vals, chosen, false);
  return abstract (wid);
}
widget choice_widget (command cmd, array<string> vals, string cur, string filter) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::filtered_choice_widget,
                                             cmd, vals, cur, filter);
  return abstract (wid);
}
widget user_canvas_widget (widget w, int style) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::scrollable_widget,
                                             w, style);
  wid->add_child (w);
  return abstract (wid);
}
widget resize_widget (widget w, int style, string w1, string h1,
                      string w2, string h2, string w3, string h3,
                      string hpos, string vpos) {
  typedef triple<string, string, string> T1;
  (void) hpos; (void) vpos;
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::resize_widget,
                                             w, style, T1(w1, w2, w3),
                                                       T1(h1, h2, h3));
  wid->add_child (w);
  return abstract (wid);
}
widget hsplit_widget (widget l, widget r) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::hsplit_widget, l, r);
  wid->add_children (array<widget> (l, r));
  return abstract (wid);
}
widget vsplit_widget (widget t, widget b) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::vsplit_widget, t, b);
  wid->add_children (array<widget> (t, b));
  return abstract (wid);
}
widget refresh_widget (string tmwid, string kind) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::refresh_widget,
                                             tmwid, kind);
    // FIXME: decide what to do with children in QTMRefresh::recompute()
  return abstract (wid);
}
widget refreshable_widget (object promise, string kind) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::refreshable_widget,
                                             promise, kind);
    // FIXME: decide what to do with children in QTMRefreshable::recompute()
  return abstract (wid);
}
widget glue_widget (bool hx, bool vx, SI w, SI h) {
  qt_widget wid = qt_ui_element_rep::create (qt_ui_element_rep::glue_widget,
                                             hx, vx, w/PIXEL, h/PIXEL);
  return abstract (wid);
}
widget glue_widget (tree col, bool hx, bool vx, SI w, SI h) {
  return tm_new<qt_glue_widget_rep> (col, hx, vx, w, h);
}
widget inputs_list_widget (command call_back, array<string> prompts) {
  return tm_new<qt_inputs_list_widget_rep> (call_back, prompts);
}
widget input_text_widget (command call_back, string type, array<string> def,
                          int style, string width) {
  return tm_new<qt_input_text_widget_rep> (call_back, type, def, style, width);
}
widget color_picker_widget (command call_back, bool bg, array<tree> proposals) {
  return tm_new<qt_color_picker_widget_rep> (call_back, bg, proposals);
}
widget file_chooser_widget (command cmd, string type, string prompt) {
  return tm_new<qt_chooser_widget_rep> (cmd, type, prompt);
}
widget printer_widget (command cmd, url ps_pdf_file) {
  return tm_new<qt_printer_widget_rep> (cmd, ps_pdf_file);
}
widget texmacs_widget (int mask, command quit) {
  if (mask) return tm_new<qt_tm_widget_rep> (mask, quit);
  else      return tm_new<qt_tm_embedded_widget_rep> (quit);
}
widget ink_widget (command cb) {
  NOT_IMPLEMENTED("Ink widget");
  (void) cb; return widget();
}
widget tree_view_widget (command cmd, tree data, tree actions) {
  qt_widget wid = qt_ui_element_rep::create (qt_widget_rep::tree_view_widget,
                                             cmd, data, actions);
  return abstract (wid);

}
  //// Widgets which are not strictly required by TeXmacs have void implementations

widget empty_widget () { NOT_IMPLEMENTED("empty_widget"); return widget(); }
widget extend (widget w, array<widget> a) { (void) a; return w; }
widget wait_widget (SI width, SI height, string message) {
  (void) width; (void) height; (void) message; return widget();
}
