
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
#include "qt_gui.hpp"

#include "gui.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "analyze.hpp"
#include "list.hpp"

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
#include <QStackedWidget>

#include "QTMWidget.hpp"
#include "QTMWindow.hpp"
//#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"
#include "QTMMenuHelper.hpp"
#include "QTMGuiHelper.hpp"

#define TYPE_CHECK(b) ASSERT (b, "type mismatch")
#define NOT_IMPLEMENTED \
  { if (DEBUG_QT) cout << "STILL NOT IMPLEMENTED\n"; }

extern int nr_windows; 
// the run-loop should exit when the number of windows is zero


#ifdef Q_WS_MAC
static QMenuBar *app_menubar = NULL;
#endif

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
 * infrastructure for delayed menu installation 
 ******************************************************************************/


int  menu_count = 0; // positive is main menu is busy
list <qt_tm_widget_rep*> waiting_widgets;
// this widget is the last wanting to install his menu bar


void
QTMGuiHelper::aboutToShowMainMenu() {
  //  cout << "Show" << LF;
  menu_count++;
}

void 
QTMGuiHelper::aboutToHideMainMenu() {
  menu_count--;
  cout << "Hide :" << menu_count << " " << N(waiting_widgets) <<  LF;
  if (menu_count <= 0) {
    menu_count = 0;
    QTimer::singleShot (0, the_gui->gui_helper, SLOT (doPopWaitingWidgets ()));
  }
}

void 
QTMGuiHelper::doPopWaitingWidgets() {
  if (!is_nil(waiting_widgets)) {
    //if (DEBUG_QT)
    cout << "Installing postponed menu" << LF;
    qApp->sendPostedEvents();
    waiting_widgets->item->install_main_menu();
    waiting_widgets = waiting_widgets->next;
  }
}


/******************************************************************************
* some debugging infrastucture
******************************************************************************/

tm_ostream&
operator << (tm_ostream& out, QRect rect) {
  return out << "(" << rect.x() << "," << rect.y() << ","
             << rect.width() << "," << rect.height() << ")";
}


/******************************************************************************
* qt_view_widget_rep
******************************************************************************/

// policy: qt_view_widget_rep owns the QWidget

qt_view_widget_rep::qt_view_widget_rep (QWidget* v):
  qt_widget_rep(), view(v), current_renderer(NULL)  {}

qt_view_widget_rep::~qt_view_widget_rep() {
  if (view) delete view;
  //FIXME: I'm (MG) not sure if we should delete manually all the QWidgets we 
  //       create or exclusively the top level ones (the windows)
  //       - Qt spectify that widgets with a parent are deleted by the parent.
  //       - Out policy is that qt_view_widget_rep owns the QWidget (so it is 
  //         responsible to delete it)
  //       Are these two requirements compatible ?
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::~qt_view_widget_rep()\n";
}

void
qt_view_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::send " << slot_name (s) << LF;
  switch (s) {
  case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      view->window() -> setWindowTitle (to_qstring_utf8 (name));
    }
    break;
#if 1
  case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (ren && canvas) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        canvas->invalidate_rect (x1,y2,x2,y1);
      }
    }
    break;
  case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (canvas) canvas->invalidate_all ();
    }
    break;
#endif
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
      if (DEBUG_QT) cout << "Ignored!\n";
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
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_view_widget_rep::query " << slot_name(s) << LF;

  switch (s) {
  case SLOT_IDENTIFIER:
    TYPE_CHECK (type_id == type_helper<int>::id);
    // return close_box<int> ((int)view->window());
    // we need only to know if the widget is attached to some gui window
    return close_box<int> (view->window() ? 1 : 0);
#if 0
    case SLOT_RENDERER:
    {
      TYPE_CHECK (type_id == type_helper<renderer>::id);
      renderer r = get_current_renderer();
      //FIXME: sometimes the renderer is queried outside repaint events 
      //       (see e.g. edit_interface_rep::idle_time)
      //       TeXmacs current policy is that we should return NULL only 
      //       if the widget is not attached (in X11 sense)
      if (!r) 
        r = the_qt_renderer();
      
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (r && canvas) {
        SI ox = -canvas->backing_pos.x()*PIXEL;
        SI oy = canvas->backing_pos.y()*PIXEL;
        r->set_origin(ox,oy);
      }
      
      return close_box<renderer> (r);
    }      
#endif
  case SLOT_POSITION:
    {
      typedef pair<SI,SI> coord2;
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= view->pos();
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }
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
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::notify " << slot_name(s) << LF;
  qt_widget_rep::notify (s, new_val);
}

/******************************************************************************
* Read and write access of subwidgets
******************************************************************************/

widget
qt_view_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT)
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
  if (DEBUG_QT)
    cout << "qt_view_widget_rep::write " << slot_name (s) << LF;
  switch (s) {
  default:
    FAILED ("cannot handle slot type");
  }
}


widget
qt_view_widget_rep::plain_window_widget (string s) {
  // creates a decorated window with name s and contents w
  view->setWindowTitle (to_qstring_utf8 (s));
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

#ifdef Q_WS_MAC
  // on the Mac there is only the system menu bar which is globally allocated.
  // do not call QMainWindow::menuBar()
  if (!app_menubar) {
   // app_menubar = new QMenuBar (0);
  //  app_menubar = mw->menuBar();
  }
//  app_menubar->setStyle (qtmstyle ());
  mw->menuBar()->setStyle (qtmstyle ());
#else
  mw->menuBar()->setStyle (qtmstyle ());
#endif
  
  QStackedWidget* tw = new QStackedWidget ();
  mw->setCentralWidget(tw);
  
#if 0
  QScrollArea* sa= new QTMScrollArea (this);
  sa->setParent (mw);
  sa->setBackgroundRole (QPalette::Dark);
  sa->setAlignment (Qt::AlignCenter);
  sa->setFocusPolicy (Qt::NoFocus);

  mw->setCentralWidget (sa);
#endif
  
  QStatusBar* bar= new QStatusBar(mw);
  leftLabel= new QLabel ("Welcome to TeXmacs", mw);
  rightLabel= new QLabel ("Booting", mw);
  leftLabel->setFrameStyle (QFrame::NoFrame);
  rightLabel->setFrameStyle (QFrame::NoFrame);
  bar->addWidget (leftLabel);
  bar->addPermanentWidget (rightLabel);
  bar->setStyle (qtmstyle ());

  // NOTE (mg): the following setMinimumWidth command disable automatic 
  // enlarging of the status bar and consequently of the main window due to 
  // long messages in the left label. I found this strange solution here
  // http://www.archivum.info/qt-interest@trolltech.com/2007-05/01453/Re:-QStatusBar-size.html
  // The solution if due to Martin Petricek. He adds:
  //    The docs says: If minimumSize() is set, the minimum size hint will be ignored.
  //    Probably the minimum size hint was size of the lengthy message and
  //    internal layout was enlarging the satusbar and the main window
  //    Maybe the notice about QLayout that is at minimumSizeHint should be
  //    also at minimumSize, didn't notice it first time and spend lot of time
  //    trying to figure this out :)

  bar->setMinimumWidth(2);
  
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
	
  // there is a bug in the early implementation of toolbars in Qt 4.6
  // which has been fixed in 4.6.2 (at least)
  // this is why we change dimension of icons
	
#if (defined(Q_WS_MAC)&&(QT_VERSION>=QT_VERSION_CHECK(4,6,0))&&(QT_VERSION<QT_VERSION_CHECK(4,6,2)))
  mw->setIconSize (QSize (22, 30));  
#else
  mw->setIconSize (QSize (17, 17));
#endif
  mw->setFocusPolicy (Qt::NoFocus);


  // handles visibility
  // at this point all the toolbars are empty so we avoid showing them
  // same for the menu bar if we are not on the Mac (where we do not have
  // other options)
  
  mainToolBar->setVisible (false);
  contextToolBar->setVisible (false);
  userToolBar->setVisible (false);
  tm_mainwindow()->statusBar()->setVisible (true);
#ifndef Q_WS_MAC
  tm_mainwindow()->menuBar()->setVisible (false);
#endif  
}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::~qt_tm_widget_rep\n";
  
  
  // clear any residual waiting menu installation
  waiting_widgets = remove(waiting_widgets, this);

  // we must detach the QTMWidget canvas from the Qt widget hierarchy otherwise
  // it will be destroyed when the view member of this object is deallocated
  // this is another problem related to our choice of letting qt_widget own its
  // underlying QWidget.

  QTMWidget *canvas = tm_canvas();
  QStackedWidget* tw= tm_centralwidget();
  if (canvas) {
    tw->removeWidget(canvas);
    canvas->setParent(NULL);
    QTMWidget::all_widgets.remove(canvas);
  }
}

void qt_tm_widget_rep::updateVisibility()
{
  mainToolBar->setVisible (visibility[1] && visibility[0]);
  contextToolBar->setVisible (visibility[2] && visibility[0]);
  userToolBar->setVisible (visibility[3] && visibility[0]);
  tm_mainwindow()->statusBar()->setVisible (visibility[4]);
#ifdef Q_WS_MAC
  //app_menubar->setVisible(true);
#else
  tm_mainwindow()->menuBar()->setVisible (visibility[0]);
#endif
}


void
qt_tm_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::send " << slot_name (s) << LF;

  switch (s) {
    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (ren && canvas) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        canvas->invalidate_rect (x1,y2,x2,y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      QTMWidget *canvas = qobject_cast <QTMWidget*>(view);
      if (canvas) canvas->invalidate_all ();
    }
      break;
      
  case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect (p);
      //NOTE: rect.topLeft is ignored since it is always (0,0)
      tm_canvas() -> setExtents(rect);
#if 0
      //cout << "p= " << p << "\n";
      QSize sz= to_qrect (p).size ();
      QSize ws= tm_scrollarea () -> size ();
      sz.setHeight (max (sz.height (), ws.height () - 4));
      //FIXME: the above adjustment is not very nice and useful only in papyrus 
      //       mode. When setting the size we should ask the GUI of some 
      //       preferred max size and set that without post-processing.
//      tm_canvas () -> setFixedSize (sz);
      tm_canvas() -> setExtentsSize(sz);
#endif
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
      leftLabel->setText (to_qstring_utf8 (tm_var_encode (msg)));
      leftLabel->update ();
    }
    break;
  case SLOT_RIGHT_FOOTER:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string msg= open_box<string> (val);
      rightLabel->setText (to_qstring_utf8 (tm_var_encode (msg)));
      rightLabel->update ();
    }
    break;

  case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt= to_qpoint (p);
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n ";
      tm_scrollarea()->setOrigin(pt);
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
      if (DEBUG_QT) cout << "New shrinking factor :" << new_sf << LF;
      tmw->tm_widget()->handle_set_shrinking_factor (new_sf);
    }
    break;

  case SLOT_FILE:
    {
      TYPE_CHECK (type_box (val) == type_helper<string>::id);
      string file = open_box<string> (val);
      if (DEBUG_QT) cout << "File: " << file << LF;
#if (QT_VERSION >= 0x040400)
      view->window()->setWindowFilePath(to_qstring_utf8(file));
#endif
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
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::query " << slot_name (s) << LF;

  switch (s) {
  case SLOT_SCROLL_POSITION:
    {
      TYPE_CHECK (type_id == type_helper<coord2>::id);
      QPoint pt= tm_canvas()->origin;
      if (DEBUG_QT)
        cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint (pt));
    }

  case SLOT_EXTENTS:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QRect rect= tm_canvas()->extents;
      coord4 c= from_qrect (rect);
      if (DEBUG_QT) cout << "Canvas geometry " << rect << LF;
      return close_box<coord4> (c);
    }

  case SLOT_VISIBLE_PART:
    {
      TYPE_CHECK (type_id == type_helper<coord4>::id);
      QSize sz = tm_canvas()->QAbstractScrollArea::viewport()->size();
      //sz.setWidth(sz.width()-2);
      QPoint pos = tm_canvas()->backing_pos;
      coord4 c = from_qrect(QRect(pos,sz));
      if (DEBUG_QT) 
        cout << "Visible Region " << c << LF;
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
  if (DEBUG_QT) cout << "[qt_tm_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}

void
replaceActions (QWidget* dest, QWidget* src) {
  //NOTE: the parent hierarchy of the actions is not modified while installing
  //      the menu in the GUI (see qt_menu.cpp for this memory management 
  //      policy)
  dest->setUpdatesEnabled(false);
  QList<QAction *> list = dest->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->removeAction (a);
  }
  list = src->actions();
  while (!list.isEmpty()) {
    QAction* a= list.takeFirst();
    dest->addAction (a);
  }
  dest->setUpdatesEnabled(true);
}

extern void
replaceButtons(QToolBar* dest, QWidget* src) {
  replaceActions (dest, src);
  dest->setUpdatesEnabled(false);
  QList<QObject*> list= dest->children();
  for (int i=0; i<list.count(); i++) {
    QToolButton* button= qobject_cast<QToolButton*> (list[i]);
    if (button)
      button->setPopupMode (QToolButton::InstantPopup);
  }
  dest->setUpdatesEnabled(true);
}



void
qt_tm_widget_rep::install_main_menu () {
  QMenu* m= to_qmenu (main_menu_widget);
  if (m) {
    {
      QMenuBar *dest = tm_mainwindow()->menuBar();
      QWidget *src = m;
      dest->setUpdatesEnabled(false);
      dest->clear();
      QList<QAction*> list = src->actions();
      while (!list.isEmpty()) {
        QAction* a= list.takeFirst();
        dest->addAction (a);
        if (a->menu()) {
          QObject::connect(a->menu(), SIGNAL(aboutToShow()),
                           the_gui->gui_helper, SLOT(aboutToShowMainMenu()));
          QObject::connect(a->menu(), SIGNAL(aboutToHide()),
                           the_gui->gui_helper, SLOT(aboutToHideMainMenu()));
        }
        
      }
      dest->setUpdatesEnabled(true);            
    }
    updateVisibility();
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT)
    cout << "qt_tm_widget_rep::write " << slot_name (s) << LF;

  switch (s) {
  case SLOT_CANVAS:
    {
      check_type_void (index, "SLOT_CANVAS");
      QTMWidget* new_canvas= qobject_cast<QTMWidget*>(((qt_view_widget_rep*) w.rep)->view);
      QTMWidget* old_canvas= tm_canvas();
      QStackedWidget* tw= tm_centralwidget();
      if (new_canvas && (new_canvas != old_canvas) ) {
        tw->addWidget(new_canvas);
        tw->removeWidget(old_canvas);
        QTMWidget::all_widgets.insert(new_canvas);
        if (old_canvas) {
          old_canvas->setParent(NULL);
          QTMWidget::all_widgets.remove(old_canvas);
        }
        new_canvas->setFocusPolicy (Qt::StrongFocus);
        new_canvas->setFocus ();
        
      }
    }
    break;

  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    {
      widget tmp = main_menu_widget;
      main_menu_widget = w;
      if (menu_count <=0) {
        install_main_menu();
      } else { 
        // menu interaction ongoing.
        // postpone menu installation when the menu interaction is done
        if (DEBUG_QT)
          cout << "Main menu is busy: postponing menu installation" << LF;

        waiting_widgets << this;
      }
    }
    break;

  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    {
      //cout << "widget :" << (void*)w.rep << LF;
      main_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (mainToolBar, m);
      updateVisibility();
    }
    break;

  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    {   
      context_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (contextToolBar, m);
      updateVisibility();
    }
    break;

  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    {   
      user_icons_widget = w;
      QMenu* m= to_qmenu (w);
      replaceButtons (userToolBar, m);
      updateVisibility();
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
  if (DEBUG_QT)
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
              if (DEBUG_QT) 
                cout << "Moving to (" << pt.x() << "," 
                    << pt.y() << ")" << LF;
        wid->move (pt);
      }
    }
    break;

  case SLOT_VISIBILITY:
    {
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      if (wid) {
        if (flag) {
          wid->show();
          wid->activateWindow();
          wid->raise();
        }
        else wid->hide();
      }
    }
    break;

  case SLOT_NAME:
    {   
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      if (wid) wid->setWindowTitle (to_qstring_utf8 (name));
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
  if (DEBUG_QT)
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
      if (DEBUG_QT)
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
  if (DEBUG_QT)
    cout << "qt_window_widget_rep::notify " << slot_name(s) << LF;
  widget_rep::notify (s, new_val);
}

widget
qt_window_widget_rep::read (slot s, blackbox index) {
  (void) index;
  if (DEBUG_QT)
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
  if (DEBUG_QT)
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
  if (DEBUG_QT) cout << "qt_simple_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
    case SLOT_INVALIDATE:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord4>::id);
      coord4 p= open_box<coord4> (val);
      if (DEBUG_QT)
        cout << "Invalidating rect " << rectangle(p.x1,p.x2,p.x3,p.x4) << LF;
      qt_renderer_rep* ren = (qt_renderer_rep*)get_renderer (this);
      if (ren) {
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;    
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        tm_canvas()->invalidate_rect (x1,y2,x2,y1);
      }
    }
      break;
    case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (DEBUG_QT)
        cout << "Invalidating all"<<  LF;
      tm_canvas()->invalidate_all ();
    }
      break;
    case SLOT_CURSOR:
    {
      TYPE_CHECK (type_box (val) == type_helper<coord2>::id);
      coord2 p= open_box<coord2> (val);
      QPoint pt = to_qpoint(p);
      tm_canvas() -> setCursorPos(pt);
    }
      break;
      
    default:
      if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
      qt_view_widget_rep::send (s, val);
//      FAILED ("unhandled slot type");
  }
  
}


blackbox
simple_widget_rep::query (slot s, int type_id) {
  if ((DEBUG_QT) && (s != SLOT_RENDERER))
    cout << "qt_simple_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_RENDERER:
    {
      TYPE_CHECK (type_id == type_helper<renderer>::id);
      renderer r = get_current_renderer();
      //FIXME: sometimes the renderer is queried outside repaint events 
      //       (see e.g. edit_interface_rep::idle_time)
      //       TeXmacs current policy is that we should return NULL only 
      //       if the widget is not attached (in X11 sense)
      if (!r) 
        r = the_qt_renderer();
 
#if 1
      QTMWidget *canvas = tm_canvas();
      if (r && canvas) {
        SI ox = -canvas->backing_pos.x()*PIXEL;
        SI oy = canvas->backing_pos.y()*PIXEL;
        r->set_origin(ox,oy);
      }
#endif 
      return close_box<renderer> (r);
    }

    default:
      return qt_view_widget_rep::query (s, type_id);      
  }
}

void
simple_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
  qt_view_widget_rep::notify (s, new_val);
}

/******************************************************************************
* Read and write access of subwidgets
******************************************************************************/

widget
simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
  return qt_view_widget_rep::read (s, index);
}

void
simple_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT) cout << "[qt_simple_widget_rep] ";
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
  
  // glue_widget is used when detaching a canvas from the texmacs window
  // in view of attaching another one, e.g. when changing buffer.
  
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
