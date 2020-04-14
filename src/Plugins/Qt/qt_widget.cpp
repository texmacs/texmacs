
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
#include "qt_window_widget.hpp"

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
qt_widget_rep::plain_window_widget (string name, command quit) {
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
  win->layout()->setContentsMargins (l+3, t+3, r+3, b+3);
  win->setWindowTitle (to_qstring (name));  // HACK: remove me (see bug#40837)
  
  qt_window_widget_rep* wid = tm_new<qt_window_widget_rep> (win, name, quit);
  wid->add_child (this);

  return widget (wid);
}

/*! Instantiates and returns a new widget which will act as a popup widget.

 This is used by popup_window_widget: subclasses reimplement this method and
 return the appropriate widget, and qt_widget_rep::popup_window_widget()
 is the "interface".
 */
widget
qt_widget_rep::make_popup_widget () {
  return tm_new<qt_popup_widget_rep> ((widget_rep*)this, command());
}

/*! Interface for the creation of popups.
 FIXME: the check below should be unnecessary, but current design is ugly.
 */
widget
qt_widget_rep::popup_window_widget (string s) {
  widget wid= make_popup_widget();
  ASSERT(concrete(wid) != this, "Loop in call to popup_window_widget()");
  return concrete(wid)->popup_window_widget(s);
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
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "destroy_window_widget() on "
                  << static_cast<qt_widget_rep*>(w.rep)->type_as_string() << LF;
}


/******************************************************************************
 * TeXmacs interface for the creation of widgets. 
 * See Graphics/Gui/widget.hpp for comments. 
 ******************************************************************************/

widget ink_widget (command cb) {
  NOT_IMPLEMENTED("Ink widget");
  (void) cb; return widget();
}

//// Widgets which are not strictly required by TeXmacs have void implementations

widget empty_widget () { NOT_IMPLEMENTED("empty_widget"); return widget(); }

widget extend (widget w, array<widget> a) { (void) a; return w; }

widget wait_widget (SI width, SI height, string message) {
  (void) width; (void) height; (void) message; return widget();
}
