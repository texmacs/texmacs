
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

#include <QWidget>
#include <QWidgetItem>
#include "QTMMenuHelper.hpp"
#include "QTMWindow.hpp"

widget the_keyboard_focus (NULL);


/******************************************************************************
 * qt_widget_rep: the base widget for the Qt port.
 ******************************************************************************/

static long widget_counter = 0;

qt_widget_rep::qt_widget_rep(types _type, QWidget* _qwid)
  : widget_rep (), id (widget_counter++), qwid (_qwid), type (_type), sequencer(0)
{
  if (DEBUG_QT)
    cout << "qt_widget_rep(), created: " << type_as_string() << LF;
}


qt_widget_rep::~qt_widget_rep() { 
  if (DEBUG_QT)
    cout << "~qt_widget_rep(), deleted: " << type_as_string() << LF;
  
  // DON'T DO THIS! (several qt_widget_rep may have the same underlying QWidget)
  //delete qwid; 
}


/*! Returns the actual QWidget underlying this qt_widget_rep.
 
 This method often constructs the QWidget on the fly each time it's called,
 for example inside qt_ui_element_rep::as_qwidget(), so it is generally NOT safe
 to assume that as_qwidget() simply returns the qwid pointer.
 
 The policy is to give ownership of the object to the caller.
 NOTE: we could set qwid=NULL to ensure that delete qwid did nothing, but this
 might break things: if you nest a widget which needs access to qwid, like 
 qt_tm_embedded_widget_rep, *after* it's been enclosed in a vertical_menu or
 something like that, then the way the latter is built taking all the qwidgets,
 would result in a crash as soon as the nested widget did something with the
 QWidget.
*/
inline QWidget*
qt_widget_rep::as_qwidget () {
  QWidget* ret = qwid;
  return ret; 
}

/*! Returns the QAction associated to this qt_widget_rep.
 
 The default action is of type QTMAction, does nothing and is disabled.
 See the remarks about memory management and menu instantiation and insertion
 in the comments to class qt_menu_rep.
 */
QAction*
qt_widget_rep::as_qaction() {
  QAction* a = new QTMAction (NULL); 
  a->setEnabled(false);
  return a;
}

/*! Wraps the underlying QWidget into a QWidgetItem and returns it.

 The policy is to give ownership of the object to the caller.
*/
inline QLayoutItem*
qt_widget_rep::as_qlayoutitem () {
  return new QWidgetItem (as_qwidget ()); 
}


/*! Returns the QMenu associated if any.
 
 This method must not give ownership of the menu to the caller, thus
 allowing menu caching at the TeXmacs level. See the implementations in
 qt_ui_element_rep and qt_menu_rep.
 Called only by code which attaches root menus in the GUI elements.
 */
QMenu*
qt_widget_rep::get_qmenu () {
  return NULL;
}


/*! Returns the widget as a window.
 
 Each TeXmacs widget can at some point be asked to present itself into a window.
 The scheme-originating function window_create() expects this method in every
 widget. 

 This default implementation constructs a wrapper qt_window_widget for the 
 widget and returns it. This wrapper will hold a new QTMPlainWindow object
 which will manage close events and take ownership of the original QWidget.
 qt_window_widget owns the QTMPlainWindow and is responsible for its deletion.
 
 The default implementation should suffice in most cases.

  \param title (Often?) a title for the window.
  \param quit Scheme closure to be executed upon close. 
  \return The new qt_window_widget.
*/
widget
qt_widget_rep::plain_window_widget (string title, command quit) {
  if (DEBUG_QT)
    cout << "qt_widget_rep::plain_window_widget() around a " << type_as_string() << LF;

  QTMPlainWindow* win = new QTMPlainWindow(0, this);
  QLayoutItem*     li = as_qlayoutitem();
  if (li) {
    QLayout* l = li->layout();
    if (! l) {
      l = new QVBoxLayout();
      l->addItem(li);
    }
    win->setLayout(l);  // Transfers ownership
  } else {
    QWidget* qw = as_qwidget();
    if (qw) {
      QLayout* l = new QVBoxLayout();
      l->addWidget(qw);        // The original QWidget now belongs to the layout
      win->setLayout(l);       // And the QLayout to the QTMPlainWindow.
    } else {
      FAILED("attempt to create a window around a nil QWidget");
    }
  }
  
  win->setWindowTitle (to_qstring (title));
  int l,t,r,b;
  win->layout()->getContentsMargins(&l, &t, &r, &b);
  win->layout()->setContentsMargins(l+3, t+3, r+3, b+3);
  
  return tm_new<qt_window_widget_rep>(win, quit);
}

/*! Instantiates and returns a new widget which will act as a popup widget.

 This is used by popup_window_widget: subclasses reimplement this method and
 return the appropriate widget, and qt_widget_rep::popup_window_widget()
 is the "interface".
 */
widget
qt_widget_rep::make_popup_widget () {
  qwid= as_qwidget();
    //qwid->setStyle(qtmstyle());
  return tm_new<qt_popup_widget_rep>(qwid, command());
}

/*! Interface for the creation of popups.
 FIXME: the check below should be unnecessary, but current design is ugly.
 */
widget
qt_widget_rep::popup_window_widget (string s) {
  widget wid= make_popup_widget();
  ASSERT((qt_widget_rep*)wid.rep != this, "Loop in call to popup_window_widget()");
  return concrete(wid)->popup_window_widget(s);
}

tm_ostream& operator << (tm_ostream& out, qt_widget w) {
  return out << "qt_widget of type: " << w.rep->type_as_string();
}

/*! Stores messages (SLOTS) sent to this widget for later replay.
 
 This is useful for recompilation of the QWidget inside as_qwidget() in some
 cases, where state information of the parsed widget (i.e. the qt_widget) is
 stored by us directly in the QWidget, and thus is lost if we delete it. This
 is used in qt_simple_widget_rep (and might make more sense there).
 
 Each SLOT is stored only once, repeated occurrences of the same one overwriting
 previous ones. Sequence information is also stored, allowing for correct replay. 
 */
void
qt_widget_rep::save_send_slot (slot s, blackbox val) {
  sent_slots[s].seq = sequencer;
  sent_slots[s].val = val;
  sent_slots[s].id  = s.sid;
  sequencer = ++sequencer % slot_id__LAST;
}


void
qt_widget_rep::reapply_sent_slots () {
  if (DEBUG_QT)
    cout << ">>>>>>>> reapply_sent_slots() for widget: " << type_as_string() << LF;
  
  t_slot_entry sorted_slots[slot_id__LAST];
  for (int i = 0; i < slot_id__LAST; ++i)
    sorted_slots[i] = sent_slots[i];
  qSort(&sorted_slots[0], &sorted_slots[slot_id__LAST]);
  
  for (int i = 0; i < slot_id__LAST; ++i)
    if (sorted_slots[i].seq >= 0)
      this->send(sorted_slots[i].id, sorted_slots[i].val);
  
  if (DEBUG_QT)
    cout << "<<<<<<<< reapply_sent_slots() for widget: " << type_as_string() << LF;
  
}


/******************************************************************************
* Global functions we export for the creation of windowed widgets by TeXmacs
******************************************************************************/

/*! Creates a decorated window using the given widget.

 The window will have name s, contents w and perform command q upon closing.
 
 Each widget type may choose how to present itself as a window, by 
 reimplementing qt_widget_rep::plain_window_widget(), although the base class
 qt_widget_rep provides a default implementation which suffices in most cases.
 See its documentation.
*/
widget
plain_window_widget (widget w, string s, command q) {
  return concrete(w)->plain_window_widget (s, q);
}

/*! Creates an undecorated window with name s and contents w.
 */
widget
popup_window_widget (widget w, string s) {
  return concrete(w)->popup_window_widget (s);
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
  return concrete(w)->make_popup_widget();
}

/*! Destroys a window as created via qt_window_widget.

 In the QT implementation explicitly destroying window widgets should
 not be necessary since the widget itself destroys the QWidget as soon as
 its destructor is called. No memory leak should be caused by this trivial
 implementation.
 */
void
destroy_window_widget (widget w) {
  if (DEBUG_QT)
    cout << "destroy_window_widget() on "
         << static_cast<qt_widget_rep*>(w.rep)->type_as_string() << LF;
}


/******************************************************************************
 * TeXmacs interface for the creation of widgets. 
 * See Graphics/Gui/widget.hpp for comments. 
 ******************************************************************************/

widget horizontal_menu (array<widget> arr) {
  return qt_ui_element_rep::create (qt_widget_rep::horizontal_menu, arr); }
widget vertical_menu (array<widget> arr)  {
  return qt_ui_element_rep::create (qt_widget_rep::vertical_menu, arr); }
widget horizontal_list (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_widget_rep::horizontal_list, arr); }
widget vertical_list (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_widget_rep::vertical_list, arr); }
widget aligned_widget (array<widget> lhs, array<widget> rhs, SI hsep, SI vsep, SI lpad, SI rpad) { 
  typedef quartet<SI, SI, SI, SI> T1;
  typedef triple<array<widget>, array<widget>, T1> T;
  return tm_new <qt_ui_element_rep> (qt_widget_rep::aligned_widget, 
                                     close_box (T (lhs,rhs, T1 (hsep, vsep, lpad, rpad)))); }
widget tabs_widget (array<widget> tabs, array<widget> bodies) {
  return qt_ui_element_rep::create (qt_widget_rep::tabs_widget, tabs, bodies); }
widget wrapped_widget (widget w, command cmd) {
  return qt_ui_element_rep::create (qt_widget_rep::wrapped_widget, w, cmd); }
widget tile_menu (array<widget> a, int cols) { 
  return qt_ui_element_rep::create (qt_widget_rep::tile_menu, a, cols); }
widget minibar_menu (array<widget> arr) { 
  return qt_ui_element_rep::create (qt_widget_rep::minibar_menu, arr); }
widget menu_separator (bool vertical) { 
  return qt_ui_element_rep::create (qt_widget_rep::menu_separator, vertical); }
widget menu_group (string name, int style) { 
  return qt_ui_element_rep::create (qt_widget_rep::menu_group , name, style); }
widget pulldown_button (widget w, promise<widget> pw) { 
  return qt_ui_element_rep::create (qt_widget_rep::pulldown_button, w, pw); }
widget pullright_button (widget w, promise<widget> pw) { 
  return qt_ui_element_rep::create (qt_widget_rep::pullright_button, w, pw); }
widget menu_button (widget w, command cmd, string pre, string ks, int style) { 
  return qt_ui_element_rep::create (qt_widget_rep::menu_button, w, cmd, pre, ks, style); }
widget balloon_widget (widget w, widget help) { 
  return qt_ui_element_rep::create (qt_widget_rep::balloon_widget, w, help); }
widget text_widget (string s, int style, color col, bool tsp) { 
  return qt_ui_element_rep::create (qt_widget_rep::text_widget, s, style, col, tsp); }
widget xpm_widget (url file_name) { 
  return qt_ui_element_rep::create (qt_widget_rep::xpm_widget, file_name); }
widget toggle_widget (command cmd, bool on, int style) { 
  return qt_ui_element_rep::create (qt_widget_rep::toggle_widget, cmd, on, style); }
widget enum_widget (command cmd, array<string> vals, string val, int style, string width) { 
  return qt_ui_element_rep::create (qt_widget_rep::enum_widget, cmd, vals, val, style, width); }
widget choice_widget (command cmd, array<string> vals, array<string> chosen) { 
  return qt_ui_element_rep::create(qt_widget_rep::choice_widget, cmd, vals, chosen, true); }
widget choice_widget (command cmd, array<string> vals, string cur) {
  array<string> chosen (1);
  chosen[0]= cur;
  return qt_ui_element_rep::create(qt_widget_rep::choice_widget, cmd, vals, chosen, false); }
widget user_canvas_widget (widget wid, int style) { 
  return qt_ui_element_rep::create(qt_widget_rep::scrollable_widget, wid, style); }
widget resize_widget (widget w, int style, string w1, string h1,
                      string w2, string h2, string w3, string h3) {
  typedef triple<string, string, string> T1;
  return qt_ui_element_rep::create(qt_widget_rep::resize_widget,
                                   w, style, T1(w1,w2,w3), T1(h1,h2,h3)); }
widget hsplit_widget (widget l, widget r) { 
  return qt_ui_element_rep::create(qt_widget_rep::hsplit_widget, l, r); }
widget vsplit_widget (widget t, widget b) { 
  return qt_ui_element_rep::create(qt_widget_rep::vsplit_widget, t, b); }
widget refresh_widget (string tmwid) {
  return qt_ui_element_rep::create(qt_widget_rep::refresh_widget, tmwid); }
widget glue_widget (bool hx, bool vx, SI w, SI h) {
  return qt_ui_element_rep::create (qt_ui_element_rep::glue_widget,
                                    hx, vx, w/PIXEL, h/PIXEL); }
widget glue_widget (tree col, bool hx, bool vx, SI w, SI h) {
  return tm_new<qt_glue_widget_rep> (col, hx, vx, w, h); }
widget inputs_list_widget (command call_back, array<string> prompts) {
  return tm_new<qt_input_widget_rep> (call_back, prompts); }
widget input_text_widget (command call_back, string type, array<string> def,
                          int style, string width) {
  return tm_new<qt_input_text_widget_rep> (call_back, type, def, style, width);}
widget color_picker_widget (command call_back, bool bg, array<tree> proposals) {
  return tm_new<qt_color_picker_widget_rep>(call_back, bg, proposals); }
widget file_chooser_widget (command cmd, string type, bool save) {
  return tm_new<qt_chooser_widget_rep> (cmd, type, save); }
widget printer_widget (command cmd, url ps_pdf_file) {
  return tm_new<qt_printer_widget_rep>(cmd, ps_pdf_file); }
widget texmacs_widget (int mask, command quit) {
  if (mask) return tm_new<qt_tm_widget_rep> (mask, quit);
  else      return tm_new<qt_tm_embedded_widget_rep> (quit); }
widget ink_widget (command cb) {
  if (DEBUG_QT) cout << "Ink widget not yet implemented.\n";
  (void) cb; return widget(); }

  //// Widgets which are not strictly required by TeXmacs have void implementations

widget empty_widget () { NOT_IMPLEMENTED; return widget(); }
widget extend (widget w, array<widget> a) { (void) a; return w; }
widget wait_widget (SI width, SI height, string message) {
  (void) width; (void) height; (void) message; return widget();
}