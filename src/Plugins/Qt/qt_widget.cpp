
/******************************************************************************
* MODULE     : qt_widget.cpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_widget.hpp"
#include "qt_simple_widget.hpp"
#include "qt_other_widgets.hpp"
#include "qt_renderer.hpp"
#include "qt_utilities.hpp"
#include "qt_menu.hpp"

#include "gui.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "analyze.hpp"

#include "qt_basic_widgets.hpp"
#include <QScrollArea>
#include <QVariant>
#include <QMainWindow>
#include <QStatusBar>
#include <QMenuBar>
#include <QToolButton>
#include <QHBoxLayout>
#include <QScrollBar>
#include <QTimer>

#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
//#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"

#define TYPE_CHECK(b) ASSERT (b, "type mismatch")
#define NOT_IMPLEMENTED \
  { if (DEBUG_EVENTS) cout << "STILL NOT IMPLEMENTED\n"; }

extern int nr_windows; // the run-loop should exit when the number of windows is zero

widget the_keyboard_focus (NULL);

widget
qt_widget_rep::plain_window_widget (string s) {
  (void) s;
  return widget ();
}

widget
qt_widget_rep::make_popup_widget () {
  return this;
}

widget
qt_widget_rep::popup_window_widget (string s) {
  (void) s;
  return widget ();
}

/******************************************************************************
* some debugging infrastucture
******************************************************************************/

ostream&
operator << (ostream& out, QRect rect) {
  return out << "(" << rect.x() << "," << rect.y() << ","
             << rect.width() << "," << rect.height() << ")";
}

const char *
slot_name (slot s) {
  static const char * slot_names[]= {
    "SLOT_IDENTIFIER",
    "SLOT_WINDOW",
    "SLOT_RENDERER",
    "SLOT_VISIBILITY",
    "SLOT_FULL_SCREEN",
    "SLOT_NAME",
    "SLOT_SIZE",
    "SLOT_POSITION",
    "SLOT_UPDATE",
    "SLOT_KEYBOARD",
    "SLOT_KEYBOARD_FOCUS",
    "SLOT_MOUSE",
    "SLOT_MOUSE_GRAB",
    "SLOT_MOUSE_POINTER",
    "SLOT_INVALIDATE",
    "SLOT_INVALIDATE_ALL",
    "SLOT_REPAINT",
    "SLOT_DELAYED_MESSAGE",
    "SLOT_DESTROY",

    "SLOT_SHRINKING_FACTOR",
    "SLOT_EXTENTS",
    "SLOT_VISIBLE_PART",
    "SLOT_SCROLLBARS_VISIBILITY",
    "SLOT_SCROLL_POSITION",
    "SLOT_CANVAS",

    "SLOT_HEADER_VISIBILITY",
    "SLOT_MAIN_MENU",
    "SLOT_MAIN_ICONS_VISIBILITY",
    "SLOT_MAIN_ICONS",
    "SLOT_CONTEXT_ICONS_VISIBILITY",
    "SLOT_CONTEXT_ICONS",
    "SLOT_USER_ICONS_VISIBILITY",
    "SLOT_USER_ICONS",
    "SLOT_FOOTER_VISIBILITY",
    "SLOT_LEFT_FOOTER",
    "SLOT_RIGHT_FOOTER",
    "SLOT_INTERACTIVE_MODE",
    "SLOT_INTERACTIVE_PROMPT",
    "SLOT_INTERACTIVE_INPUT",

    "SLOT_FORM_FIELD",
    "SLOT_STRING_INPUT",
    "SLOT_INPUT_TYPE",
    "SLOT_INPUT_PROPOSAL",
    "SLOT_FILE",
    "SLOT_DIRECTORY"
  };
  return slot_names[s.sid];
}

/******************************************************************************
* qt_view_widget_rep
******************************************************************************/

// policy: qt_view_widget_rep owns the QWidget

qt_view_widget_rep::qt_view_widget_rep (QWidget* v):
  qt_widget_rep(), view(v), current_renderer(NULL)  {}

qt_view_widget_rep::~qt_view_widget_rep() {
  if (view) delete view;
  //FIXME: I'm (MG) not sure if we should delete manually all the QWidgets we create
  //       or exclusively the top level ones (the windows)
  //       - Qt spectify that widgets with a parent are deleted by the parent.
  //       - Out policy is that qt_view_widget_rep owns the QWidget (so it is responsible to delete it)
  //       are these two requirements compatible ?
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::~qt_view_widget_rep()\n";
}

void
qt_view_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::send " << slot_name (s) << LF;
  switch (s) {
  case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      view->window() -> setWindowTitle (to_qstring (name));
    }
    break;
  case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect(p);
      if (DEBUG_EVENTS) cout << "Invalidating rect " << rect << LF;
      view->update (rect);
    }
    break;
  case SLOT_INVALIDATE_ALL:
    ASSERT (is_nil (val), "type mismatch");
    view->update(); // [view setNeedsDisplay:YES];
    break;

  case SLOT_MOUSE_GRAB:
    NOT_IMPLEMENTED;
    //send_mouse_grab (THIS, val);
    break;

  case SLOT_MOUSE_POINTER:
    NOT_IMPLEMENTED;
    //send_mouse_pointer (THIS, val);
    break;

  case SLOT_KEYBOARD_FOCUS:
    //send_keyboard_focus (THIS, val);
    TYPE_CHECK (type_box (val) == type_helper<bool>::id);
    if (open_box<bool> (val)) the_keyboard_focus = this;
      if (DEBUG_EVENTS) cout << "Ignored!\n";
    break;
                        
  default:
    FAILED ("unhandled slot type");
  }
}

/******************************************************************************
* Querying
******************************************************************************/

blackbox
qt_view_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_EVENTS) && (s != SLOT_RENDERER))
    cout << "qt_view_widget_rep::query " << slot_name(s) << LF;

  switch (s) {
  case SLOT_IDENTIFIER:
    TYPE_CHECK (type_id == type_helper<int>::id);
    // return close_box<int> ((int)view->window());
    // we need only to know if the widget is attached to some gui window
    return close_box<int> (view->window() ? 1 : 0);
  case SLOT_RENDERER:
    {
      TYPE_CHECK (type_id == type_helper<renderer>::id);
      renderer r = get_current_renderer();
      //FIXME: sometimes the renderer is queried outside repaint events (see e.g. edit_interface_rep::idle_time)
      //       TeXmacs current policy is that we should return NULL only if the widget is not attached (in X11 sense)
      if (!r) r = the_qt_renderer();
      return close_box<renderer> (r);
    }
  case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= view->pos();
      if (DEBUG_EVENTS)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }

#if 0
  case SLOT_VISIBLE_PART:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect = view->visibleRegion().boundingRect();
      coord4 c = from_qrect (rect);
      if (DEBUG_EVENTS) cout << "visibleRegion " << rect << LF;
      return close_box<coord4> (c);
    }
#endif

  default:
    FAILED ("cannot handle slot type");
    return blackbox ();
  }
}

/******************************************************************************
* Notification of state changes
******************************************************************************/

// overridden to provid debugging support. Can be removed.

void
qt_view_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::notify " << slot_name(s) << LF;
  qt_widget_rep::notify (s, new_val);
}

/******************************************************************************
* Read and write access of subwidgets
******************************************************************************/

widget
qt_view_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::read " << slot_name(s) << LF;

  switch (s) {
    case SLOT_WINDOW:
      {
        check_type_void (index, "SLOT_WINDOW");
        QWidget* qwin = view->window();
        QVariant v= qwin->property ("texmacs_window_widget");
        if (v.canConvert<void*> ())
          return (widget_rep*) (v.value<void*> ());
        else FAILED ("QWidget property not set");
      }
      break; // not reached     
    default:
      FAILED ("cannot handle slot type");
      return widget();
  }
}

void
qt_view_widget_rep::write (slot s, blackbox index, widget w) {
  (void) index; (void) w;
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::write " << slot_name (s) << LF;
  switch (s) {
  default:
    FAILED ("cannot handle slot type");
  }
}


widget
qt_view_widget_rep::plain_window_widget (string s) {
  // creates a decorated window with name s and contents w
  view->setWindowTitle (to_qstring (s));
  //return this;
  widget wid= tm_new<qt_window_widget_rep> (view);
  //FIXME: is this the right thing to do?
  return wid;
}


qt_tm_widget_rep::qt_tm_widget_rep(int mask, command _quit):
  qt_view_widget_rep (new QTMWindow (this)), helper (this), quit(_quit)
{
  // decode mask
  visibility[0] = (mask & 1)  == 1;  // header
  visibility[1] = (mask & 2)  == 2;  // main
  visibility[2] = (mask & 4)  == 4;  // context
  visibility[3] = (mask & 8)  == 8;  // user
  visibility[4] = (mask & 16) == 16; // footer


  QMainWindow* mw= tm_mainwindow ();
  mw->setStyle (qtmstyle ());
  mw->menuBar()->setStyle (qtmstyle ());

  QScrollArea* sa= new QTMScrollArea (this);
  sa->setParent (mw);
  sa->setBackgroundRole (QPalette::Dark);
  sa->setAlignment (Qt::AlignCenter);

  mw->setCentralWidget (sa);

  QStatusBar* bar= new QStatusBar(mw);
  leftLabel= new QLabel ("Welcome to TeXmacs", mw);
  rightLabel= new QLabel ("Booting", mw);
  leftLabel->setFrameStyle (QFrame::NoFrame);
  rightLabel->setFrameStyle (QFrame::NoFrame);
  bar->addWidget (leftLabel);
  bar->addPermanentWidget (rightLabel);
  bar->setStyle (qtmstyle ());
  mw->setStatusBar (bar);

  mainToolBar= mw->addToolBar ("main toolbar");
  mw->addToolBarBreak ();
  contextToolBar= mw->addToolBar ("context toolbar");
  mw->addToolBarBreak ();
  userToolBar= mw->addToolBar ("user toolbar");
  mw->addToolBarBreak ();

  mainToolBar->setStyle (qtmstyle ());
  contextToolBar->setStyle (qtmstyle ());
  userToolBar->setStyle (qtmstyle ());

  updateVisibility();

  mw->setIconSize (QSize (17, 17));

  mw->setFocusPolicy (Qt::NoFocus);
  sa->setFocusPolicy (Qt::NoFocus);


}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::~qt_tm_widget_rep\n";
}

void qt_tm_widget_rep::updateVisibility()
{
  mainToolBar->setVisible (visibility[1] && visibility[0]);
  contextToolBar->setVisible (visibility[2] && visibility[0]);
  userToolBar->setVisible (visibility[3] && visibility[0]);
  tm_mainwindow()->statusBar()->setVisible (visibility[4]);
#ifndef Q_WS_MAC
  tm_mainwindow()->menuBar()->setVisible (visibility[0]);
#endif
}


void
qt_tm_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::send " << slot_name (s) << LF;

  switch (s) {
  case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      //cout << "p= " << p << "\n";
      QSize sz= to_qrect (p).size ();
      QSize ws= tm_scrollarea () -> size ();
      sz.setHeight (max (sz.height (), ws.height () - 4));
      //FIXME: the above adjustment is not very nice and useful only in papyrus mode.
      //       When setting the size we should ask the GUI of some preferred max size
      //       and set that without post-processing.
      tm_canvas () -> setFixedSize (sz);
    }
    break;
  case SLOT_HEADER_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[0] = f;
      updateVisibility();
    }
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[1] = f;
      updateVisibility();
    }
    break;
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[2] = f;
      updateVisibility();
    }
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[3] = f;
      updateVisibility();
    }
    break;
  case SLOT_FOOTER_VISIBILITY:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      bool f= open_box<bool> (val);
      visibility[4] = f;
      updateVisibility();
    }
    break;

  case SLOT_LEFT_FOOTER:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string msg= open_box<string> (val);
      leftLabel->setText (to_qstring_utf8 (qt_translate (msg)));
      leftLabel->update ();
    }
    break;
  case SLOT_RIGHT_FOOTER:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string msg= open_box<string> (val);
      rightLabel->setText (to_qstring_utf8 (qt_translate (msg)));
      rightLabel->update ();
    }
    break;

  case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt= to_qpoint (p);
      // convert from main widget to canvas coordinates
      // QPoint pt2= tm_mainwindow()->mapToGlobal (pt);
      // pt= tm_scrollarea()->widget()->mapFromGlobal (pt2);
      if (DEBUG_EVENTS)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n ";
#if 0
      cout << "scrollarea ("
           << tm_scrollarea()->x() << ","
           << tm_scrollarea()->y() << ","
           << tm_scrollarea()->width() << ","
           << tm_scrollarea()->height() << ")\n";
      cout << "widget     ("
           << tm_scrollarea()->widget()->x() << ","
           << tm_scrollarea()->widget()->y() << ","
           << tm_scrollarea()->widget()->width() << ","
           << tm_scrollarea()->widget()->height() << ")\n";
      cout << "GOING TO ("
           << pt.x()+tm_scrollarea()->width()/2 << ","
           << pt.y()+tm_scrollarea()->height()/2 << ")\n";
#endif
      // It is still not very clear to me because this shift of half h/w size works...
      int sx= pt.x () + tm_scrollarea () -> width  () / 2;
      int sy= pt.y () + tm_scrollarea () -> height () / 2;
      tm_scrollarea () -> ensureVisible (sx - 50, sy - 50);
      tm_scrollarea () -> ensureVisible (sx + 50, sy + 50);
      tm_scrollarea () -> ensureVisible (sx, sy);
      tm_scrollarea () -> update();
    }
    break;

  case SLOT_SCROLLBARS_VISIBILITY:
    // ignore this: qt handles scrollbars independently
    //                send_int (THIS, "scrollbars", val);
    break;

  case SLOT_INTERACTIVE_MODE:
    {
      TYPE_CHECK (type_box (val) == type_helper<bool>::id);
      if (open_box<bool> (val) == true) {
        QTimer::singleShot (0, &helper, SLOT (doit ()));
        // do_interactive_prompt ();
      }
    }
    break;

  case SLOT_SHRINKING_FACTOR:
    TYPE_CHECK (type_box (val) == type_helper<int>::id);
    if (QTMWidget* tmw= qobject_cast<QTMWidget*> (tm_canvas())) {
      int new_sf = open_box<int> (val);
      if (DEBUG_EVENTS) cout << "New shrinking factor :" << new_sf << LF;
      tmw->tm_widget()->handle_set_shrinking_factor (new_sf);
    }
    break;

  default:
    qt_view_widget_rep::send (s, val);
  }
}

void
QTMInteractiveInputHelper::doit () {
  wid->do_interactive_prompt();
}

blackbox
qt_tm_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::query " << slot_name (s) << LF;

  switch (s) {
  case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= tm_canvas()->pos();
      if (DEBUG_EVENTS)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }

  case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect= tm_canvas()->geometry();
      coord4 c= from_qrect (rect);
      if (DEBUG_EVENTS) cout << "Canvas geometry " << rect << LF;
      return close_box<coord4> (c);
    }

  case SLOT_VISIBLE_PART:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect= tm_canvas()->visibleRegion().boundingRect();
      coord4 c= from_qrect (rect);
      if (DEBUG_EVENTS) cout << "Visible Region " << rect << LF;
      return close_box<coord4> (c);
    }
                        
  case SLOT_USER_ICONS_VISIBILITY:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (visibility[3]);

  case SLOT_CONTEXT_ICONS_VISIBILITY:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (visibility[2]);

  case SLOT_MAIN_ICONS_VISIBILITY:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (visibility[1]);

  case SLOT_HEADER_VISIBILITY:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (visibility[0]);

  case SLOT_FOOTER_VISIBILITY:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (visibility[4]);

  case SLOT_INTERACTIVE_INPUT:
    TYPE_CHECK (type_id == type_helper<string>::id);
    return close_box<string>
      (((qt_input_text_widget_rep*) int_input.rep) -> text);
    // return close_box<string> ("FIXME");

  case SLOT_INTERACTIVE_MODE:
    TYPE_CHECK (type_id == type_helper<bool>::id);
    return close_box<bool> (false); // FIXME: who needs this info?

  default:
    return qt_view_widget_rep::query (s, type_id);
  }
}

widget
qt_tm_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS) cout << "[qt_tm_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}

void
replaceActions (QWidget* dest, QWidget* src) {
  QList<QAction *> list = dest->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->removeAction (a);
    delete a;
//    a->deleteLater();
  }
  // cout << "replaceActions n:" << src->actions().count() << LF;
  list = src->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->addAction (a);
    a->setParent (dest);
  }

  // dest->addActions(src->actions());
#if 0
  for(int i=0; i<list.count(); i++)
  {
    list[i]->setMenuRole (QAction::ApplicationSpecificRole);
  }
#endif
}

extern void
replaceButtons(QToolBar* dest, QWidget* src) {
  replaceActions (dest, src);
  QList<QObject*> list= dest->children();
  for (int i=0; i<list.count(); i++) {
    QToolButton* button= qobject_cast<QToolButton*> (list[i]);
    if (button)
      button->setPopupMode (QToolButton::InstantPopup);
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::write " << slot_name (s) << LF;

  switch (s) {
  case SLOT_CANVAS:
    {
      check_type_void (index, "SLOT_CANVAS");
      QWidget* qw= ((qt_view_widget_rep*) w.rep)->view;
      QWidget* old_canvas= tm_scrollarea()->takeWidget();
      tm_scrollarea()->setWidget (qw);                  
      (void) old_canvas;
      // old_canvas will be deleted when the corresponding qt_view_widget_rep is destroyed
      qw->setFocusPolicy (Qt::StrongFocus);
      qw->setFocus ();
    }
    break;

  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    {
      QMenu* m= to_qmenu (w);
      if (m) {
        replaceActions (tm_mainwindow()->menuBar (), m);
        delete m;
      }
    }
    break;

  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    {
      //cout << "widget :" << (void*)w.rep << LF;
      QMenu* m= to_qmenu (w);
      //mainToolBar->setUpdatesEnabled (false);
      replaceButtons (mainToolBar, m);
      delete m;
    }
    break;

  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    {   
      QMenu* m= to_qmenu (w);
      replaceButtons (contextToolBar, m);
      //mainToolBar->setUpdatesEnabled (true);
      delete m;
    }
    break;

  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    {   
      QMenu* m= to_qmenu (w);
      replaceButtons (userToolBar, m);
      delete m;
    }
    break;

  case SLOT_INTERACTIVE_PROMPT:
    check_type_void (index, "SLOT_INTERACTIVE_PROMPT");
    int_prompt= concrete (w);
    break;

  case SLOT_INTERACTIVE_INPUT:
    check_type_void (index, "SLOT_INTERACTIVE_INPUT");
    int_input= concrete (w);
    break;

  default:
    qt_view_widget_rep::write (s, index, w);
  }
}

widget
qt_tm_widget_rep::plain_window_widget (string s) {
  // creates a decorated window with name s and contents w
  widget w= qt_view_widget_rep::plain_window_widget (s);
  // to manage correctly retain counts
  qt_window_widget_rep* wid= (qt_window_widget_rep*) (w.rep);
  return wid;
}

qt_window_widget_rep::qt_window_widget_rep (QWidget* _wid):
  widget_rep(), wid(_wid)
{
  wid->setProperty ("texmacs_window_widget",
                    QVariant::fromValue ((void*) this));
  nr_windows++;
}

qt_window_widget_rep::~qt_window_widget_rep ()
{
  nr_windows--;
}

void
qt_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::send " << slot_name (s) << LF;

  switch (s) {
  case SLOT_SIZE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      if (wid) {
              QSize size= to_qsize (p);
              wid->resize (size);
      }
    }
    break;

  case SLOT_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      if (wid) {
              QPoint pt = to_qpoint (p);
              pt.ry() += 40;
        // to avoid window under menu bar on MAC when moving at (0,0)
              if (DEBUG_EVENTS) cout << "Moving to (" << pt.x() << "," << pt.y() << ")" << LF;
        wid->move (pt);
      }
    }
    break;

  case SLOT_VISIBILITY:
    {
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (wid) {
              if (flag) wid->show();
              else wid->hide();
      }
    }
    break;

  case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      if (wid) wid->setWindowTitle (to_qstring (name));
    }
    break;

  case SLOT_FULL_SCREEN:
    {
      check_type<bool> (val, "SLOT_FULL_SCREEN");
      NOT_IMPLEMENTED ;
      bool flag = open_box<bool> (val);
      if (flag) {
        if (wid) wid->showFullScreen();
      } else {
        if (wid) wid->showNormal();
      }
    //win->set_full_screen (open_box<bool> (val));
    }
    break;

  case SLOT_UPDATE:
    NOT_IMPLEMENTED ;
    //send_update (THIS, val);
    break;

  default:
    FAILED ("cannot handle slot type");
  }
}

blackbox
qt_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_IDENTIFIER:
    TYPE_CHECK (type_id == type_helper<int>::id);
    // we need only to know if the widget is attached to some gui window
    return close_box<int> (wid? 1: 0);
    // return close_box<int> ((int)wid);
  case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= wid->pos();
      if (DEBUG_EVENTS)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QSize s= wid->size();
      return close_box<coord2> (from_qsize (s));
    }
  default:
    FAILED ("cannot handle slot type");
    return blackbox ();
  }
}

/******************************************************************************
* Notification of state changes
******************************************************************************/

void
qt_window_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::notify " << slot_name(s) << LF;
  widget_rep::notify (s, new_val);
}

widget
qt_window_widget_rep::read (slot s, blackbox index) {
  (void) index;
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  default:
    FAILED ("cannot handle slot type");
    return widget();
  }
}

void
qt_window_widget_rep::write (slot s, blackbox index, widget w) {
  (void) w; (void) index;
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::write " << slot_name(s) << LF;

  switch (s) {
  default:
    FAILED ("cannot handle slot type");
  }
}

/******************************************************************************
* simple_widget_rep
******************************************************************************/

simple_widget_rep::simple_widget_rep ():
  qt_view_widget_rep (new QTMWidget (this))
{
  // view->setProperty ("texmacs_widget", QVariant::fromValue ((void*) this));
}


/******************************************************************************
* Empty handlers for redefinition later on
******************************************************************************/

void
simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t;
}

void
simple_widget_rep::handle_set_shrinking_factor (int sf) {
  (void) sf;
}

void
simple_widget_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::send (slot s, blackbox val) {
  // if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::send " << slot_name(s) << LF;
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::send (s, val);
}


blackbox
simple_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_EVENTS) && (s != SLOT_RENDERER))
    cout << "[qt_simple_widget_rep] ";
  return qt_view_widget_rep::query (s, type_id);
}

void
simple_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::notify (s, new_val);
}

/******************************************************************************
* Read and write access of subwidgets
******************************************************************************/

widget
simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}

void
simple_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::write (s, index, w);
}

/******************************************************************************
* Window widgets
******************************************************************************/

widget
plain_window_widget (widget w, string s) {
  // creates a decorated window with name s and contents w
  return concrete(w)->plain_window_widget (s);
}

widget
popup_window_widget (widget w, string s) {
  // creates an undecorated window with name s and contents w
  return concrete(w)->popup_window_widget (s);
}


void
destroy_window_widget (widget w) {
  // FIXME: Handle correcly
  // destroys a window as created by the above routines
  (void) w;

  // In the QT implementation explicitly destroying window widgets should not be necessary
  // since the widget itself destroy the Qt widget as soon as its destructor is called.
  // No memory leak should be caused by this trivial implementation.
}

/******************************************************************************
* Top-level widgets, typically given as an argument to plain_window_widget
* See also message.hpp for specific messages for these widgets
******************************************************************************/

widget
texmacs_widget (int mask, command quit) {
  // the main TeXmacs widget and a command which is called on exit
  // the mask variable indicates whether the menu, icon bars, status bar, etc.
  // are visible or not
  (void) mask; (void) quit; // FIXME: handle correctly mask and quit
  widget w= tm_new<qt_tm_widget_rep> (mask, quit);
  return w;
}

widget
popup_widget (widget w) {
  // a widget container which results w to be unmapped as soon as
  // the pointer quits the widget
  // used in edit_mouse.cpp to implement a contextual menu in the canvas
  return concrete(w)->make_popup_widget();
}

/******************************************************************************
*  Widgets which are not strictly required by TeXmacs
*  their implementation is void
******************************************************************************/

widget
empty_widget () {
  // an empty widget of size zero
  NOT_IMPLEMENTED;
  return widget();
}

widget
glue_widget (bool hx, bool vx, SI w, SI h) {
  //{ return widget(); }
  // an empty widget of minimal width w and height h and which is horizontally
  // resp. vertically extensible if hx resp. vx is true
  NOT_IMPLEMENTED;
  (void) hx; (void) vx; (void) w; (void) h;
  return tm_new<qt_view_widget_rep> (new QWidget ());
}

widget
wait_widget (SI width, SI height, string message) {
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
  (void) width; (void) height; (void) message;
  return widget();
}
