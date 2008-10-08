
/****************************************************************************** * MODULE     : qt_widget.cpp
* DESCRIPTION: QT widget class
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

#include "QTMWidget.hpp"
#include "QTMGuiHelper.hpp"
#include "QTMStyle.hpp"

widget the_keyboard_focus(NULL);

/******************************************************************************
* main renderer
******************************************************************************/

qt_renderer_rep *the_renderer = NULL;
qt_renderer_rep *the_qt_renderer() 
{
  if (!the_renderer) the_renderer = new qt_renderer_rep (the_gui);
  return the_renderer;
}


widget qt_widget_rep::plain_window_widget (string s)
{
  (void) s;
  return widget ();
}

widget qt_widget_rep::make_popup_widget ()
{
  return this;
}

widget qt_widget_rep::popup_window_widget (string s)
{
  (void) s;
  return widget();
}

/******************************************************************************
 * some debugging infrastucture
 ******************************************************************************/


ostream& operator << (ostream& out, QRect rect)
{
	out << "(" << rect.x() << "," << rect.y() << "," << rect.width() << "," << rect.height() << ")";
	return out;
}

char  *slot_name(slot s)
{
  static char * slot_names[] =  {
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
 
qt_view_widget_rep::qt_view_widget_rep(QWidget *v) : qt_widget_rep(), view(v) {  }
qt_view_widget_rep::~qt_view_widget_rep()  
{ 
  if (view) delete view;
  if (DEBUG_EVENTS) 
    cout << "qt_view_widget_rep::~qt_view_widget_rep()\n";
}



void
qt_view_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_NAME:
    {	
      check_type<string> (val, "SLOT_NAME");
      string name = open_box<string> (val);
      view->window()->setWindowTitle(to_qstring(name));
    }
    break;
  case SLOT_SCROLLBARS_VISIBILITY:
    // ignore this: cocoa handles scrollbars independently
    //			send_int (THIS, "scrollbars", val);
    break;
  case SLOT_INVALIDATE:
    {
      if (type_box (val) != type_helper<coord4>::id)
        fatal_error ("type mismatch", "SLOT_INVALIDATE");
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect(p);
      if (DEBUG_EVENTS) cout << "Invalidating rect " << rect << LF;
      view->update(rect);
    }
    break;
  case SLOT_EXTENTS:
    {
      if (type_box (val) != type_helper<coord4>::id)
        fatal_error ("type mismatch", "SLOT_EXTENTS");
      coord4 p= open_box<coord4> (val);
      QRect rect = to_qrect(p);
      qobject_cast< QScrollArea * >(view)->widget()->resize(rect.size());
      // [[(NSScrollView*)view documentView] setFrameSize: rect.size];
    }
    break;
  case SLOT_INVALIDATE_ALL:
    {
      if (!is_nil (val))
        fatal_error ("type mismatch", "SLOT_INVALIDATE_ALL");
      view->update(); // [view setNeedsDisplay:YES];
    }
    break;
    
  case SLOT_HEADER_VISIBILITY:
    //			send_bool (THIS, "header", val);
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    //			send_bool (THIS, "main icons", val);
    break;
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    //			send_bool (THIS, "context icons", val);
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    //			send_bool (THIS, "user icons", val);
    break;
  case SLOT_FOOTER_VISIBILITY:
    //			send_bool (THIS, "footer flag", val);
    break;
    
  case SLOT_MOUSE_GRAB:
    //			send_mouse_grab (THIS, val);
    break;
  case SLOT_MOUSE_POINTER:
    //			send_mouse_pointer (THIS, val);
    break;
  case SLOT_SCROLL_POSITION:
    {
      if (type_box (val) != type_helper<coord2>::id)
        fatal_error ("type mismatch", "SLOT_SCROLL_POSITION");
      coord2 p= open_box<coord2> (val);
      QPoint pt = to_qpoint(p);
      //[[(NSScrollView*)view documentView] scrollPoint:pt];
      if (DEBUG_EVENTS) cout << "Position (" << pt.x() << "," << pt.y() << ")" << LF;
      //qobject_cast<QScrollArea *>(view)->widget()->move(pt);
     // 	qobject_cast<QScrollArea *>(view)->ensureVisible(pt.x(),pt.y());
      //			[[(NSScrollView*)view documentView] scrollRectToVisible:NSMakeRect(pt.x,pt.y,1.0,1.0)];
    }
    break;
  case SLOT_SHRINKING_FACTOR:
    {
      if (type_box (val) != type_helper<int>::id)
        fatal_error ("type mismatch", "SLOT_SHRINKING_FACTOR");
      if (DEBUG_EVENTS) cout << "Ignored!\n";
    }
    break;
  case SLOT_KEYBOARD_FOCUS:
    //			send_keyboard_focus (THIS, val);
    {
      if (type_box (val) != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_KEYBOARD_FOCUS");
      if (open_box<bool>(val)) the_keyboard_focus = this;
    }
    break;
    
#if 0
  case SLOT_SIZE:
    {
      if (type_box (val) != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_SIZE");
      coord2 p= open_box<coord2> (val);
      NSWindow *win = [view window];
      if (win) {
	NSRect frame = [win frame];
	NSSize s = to_nssize(p);
	frame.size = s;
	[win setFrame:frame display:YES];
      }
    }
    break;
  case SLOT_POSITION:
    {
      if (type_box (val) != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_POSITION");
      coord2 p= open_box<coord2> (val);
      NSWindow *win = [view window];
      if (win) {
	[win setFrameTopLeftPoint:to_nspoint(p)];
      }
    }
    break;
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
      NSWindow *win = [view window];
      if (win) {
	if (flag)
	  [win makeKeyAndOrderFront:nil] ;
	else 
	  [win orderOut:nil]  ;
      }
    }	
    break;
  case SLOT_IDENTIFIER:
    check_type<int> (val, "SLOT_IDENTIFIER");
    THIS << emit_attach_window (get_window (open_box<int> (val)));
    break;
  case SLOT_FULL_SCREEN:
    check_type<bool> (val, "SLOT_FULL_SCREEN");
    win->set_full_screen (open_box<bool> (val));
    break;
  case SLOT_UPDATE:
    send_update (THIS, val);
    break;
  case SLOT_KEYBOARD:
    send_keyboard (THIS, val);
    break;
  case SLOT_MOUSE:
    send_mouse (THIS, val);
    break;
  case SLOT_REPAINT:
    send_repaint (THIS, val);
    break;
  case SLOT_DELAYED_MESSAGE:
    send_delayed_message (THIS, val);
    break;
  case SLOT_DESTROY:
    send_destroy (THIS, val);
    break;
			
  case SLOT_EXTENTS:
    send_coord4 (THIS, "extents", val);
    break;
  case SLOT_SCROLLBARS_VISIBILITY:
    send_int (THIS, "scrollbars", val);
    break;
			
  case SLOT_INTERACTIVE_MODE:
    send_bool (THIS, "interactive mode", val);
    break;
			
  case SLOT_STRING_INPUT:
    send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    send_string (THIS, "type", val);
    break;
  case SLOT_INPUT_PROPOSAL:
    send_string (THIS, "default", val);
    break;
  case SLOT_FILE:
    send_string (THIS, "file", val);
    break;
  case SLOT_DIRECTORY:
    send_string (THIS, "directory", val);
    break;
#endif			
  default:
    fatal_error ("cannot handle slot type", "qt_view_widget_rep::send");
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
    if (type_id != type_helper<int>::id)
      fatal_error ("int expected (SLOT_IDENTIFIER)", "qt_view_widget_rep::query");
//    return close_box<int> ((int)view->window());
	// we need only to know if the widget is attached to some gui window	  
		  return close_box<int> (view->window() ? 1 : 0);
			
  case SLOT_RENDERER:
    if (type_id != type_helper<renderer>::id)
      fatal_error ("renderer expected (SLOT_RENDERER)", "qt_view_widget_rep::query");
    return close_box<renderer> ((renderer) the_qt_renderer());

  case SLOT_VISIBLE_PART:
    {
      if (type_id != type_helper<coord4>::id)
	fatal_error ("type mismatch", "SLOT_VISIBLE_PART");
      QRect rect = view->visibleRegion().boundingRect();
      coord4 c = from_qrect(rect);
			if (DEBUG_EVENTS) cout << "visibleRegion " << rect << LF;
      return close_box<coord4> (c);
    }

			
		case SLOT_SCROLL_POSITION:
    {
      if (type_id != type_helper<coord2>::id)
				fatal_error ("type mismatch", "SLOT_SCROLL_POSITION");
      QPoint pt = qobject_cast<QScrollArea *>(view)->widget()->pos(); // FIXME

			if (DEBUG_EVENTS) {
				cout << "Position (" << pt.x() << "," << pt.y() << ")\n"; 
			}

      //	NSPoint pt = [[(NSScrollView *)view contentView] bounds].origin;
      return close_box<coord2> (from_qpoint(pt));
    }
			
#if 0

			
  case SLOT_POSITION:  
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
        fatal_error ("type mismatch (SLOT_POSITION)", "qt_view_widget_rep::query");
      return close_box<coord2> (coord2(0,0)); //FIXME: fake position (who need this information?)
    }
			
			
  case SLOT_SIZE:
    return query_size (THIS, type_id);
  case SLOT_POSITION:
    return query_position (THIS, type_id);
  case SLOT_KEYBOARD_FOCUS:
    return query_keyboard_focus (THIS, type_id);
  case SLOT_MOUSE_GRAB:
    return query_mouse_grab (THIS, type_id);
			
  case SLOT_EXTENTS:
    return query_coord4 (THIS, "extents", type_id);
  case SLOT_SCROLLBARS_VISIBILITY:
    return query_int (THIS, "scrollbars", type_id);
			
  case SLOT_INTERACTIVE_MODE:
    return query_bool (THIS, "interactive mode", type_id);
  case SLOT_INTERACTIVE_INPUT:
    return query_string (THIS, "interactive input", type_id);
			
  case SLOT_STRING_INPUT:
    return query_string (THIS, "input", type_id);
#endif
  default:
    fatal_error ("cannot handle slot type", "qt_view_widget_rep::query");
    return blackbox ();
  }
}

/******************************************************************************
* Notification of state changes
******************************************************************************/

void
qt_view_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::notify " << slot_name(s) << LF;
  switch (s) {
#if 0
  case SLOT_SIZE:
    check_type<SI,SI> (new_val, "SLOT_SIZE");
    THIS << emit_resize ();
    if (is_window_widget ())
      send_size (THIS [0], new_val);
    break;
  case SLOT_POSITION:
    check_type<SI,SI> (new_val, "SLOT_POSITION");
    THIS << emit_move ();
    break;
  case SLOT_KEYBOARD_FOCUS:
    notify_keyboard_focus (THIS, new_val);
    break;
  case SLOT_MOUSE_GRAB:
    notify_mouse_grab (THIS, new_val);
    break;
#endif
  default: ;
  }
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
  case SLOT_WINDOW: {
    check_type_void (index, "SLOT_WINDOW");
    QWidget *qwin = view->window();
    QVariant v= qwin->property("texmacs_window_widget");
    if (v.canConvert<void *>()) 
      return (widget_rep *) (v.value<void *>());
    else 
      fatal_error ("QWidget property not set", "qt_view_widget_rep::read");
  }
			
    //			return qobject_cast <QTMWindow *>(view->window())->wid;
    //			[(TMWindowController*)[[view window] windowController] widget];
#if 0
  case SLOT_WINDOW:
    check_type_void (index, "SLOT_WINDOW");
    return win -> get_widget ();
  case SLOT_FORM_FIELD:
    check_type<int> (index, "SLOT_FORM_FIELD");
    return abstract (THIS [0] ["inputs"] [open_box<int> (index)] ["input"]);
  case SLOT_FILE:
    check_type_void (index, "SLOT_FILE");
    return abstract (THIS [0] ["file"] ["input"]);
  case SLOT_DIRECTORY:
    check_type_void (index, "SLOT_DIRECTORY");
    return abstract (THIS [0] ["directory"] ["input"]);
#endif
  default:
    fatal_error ("cannot handle slot type", "qt_view_widget_rep::read");
    return widget();
  }
}

void
qt_view_widget_rep::write (slot s, blackbox index, widget w) {
  (void) index; (void) w;
  if (DEBUG_EVENTS)
    cout << "qt_view_widget_rep::write " << slot_name(s) << LF;
  switch (s) {
#if 0
  case SLOT_CANVAS: 
    {
      check_type_void (index, "SLOT_CANVAS");
      NSView *v = concrete (w)->get_nsview();
      NSScrollView *sv = (NSScrollView*) view;
      [sv setDocumentView: v];
      [[sv window] makeFirstResponder:v];
    }
    break;
  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    [NSApp setMainMenu: to_nsmenu(w)];
    //			THIS << set_widget ("menu bar", concrete (w));
    break;
  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    //			THIS << set_widget ("main icons bar", concrete (w));
    break;
  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    //			THIS << set_widget ("context icons bar", concrete (w));
    break;
  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    //			THIS << set_widget ("user icons bar", concrete (w));
    break;
#endif
  default:
    fatal_error ("cannot handle slot type", "qt_view_widget_rep::write");
  }
}


widget qt_view_widget_rep::plain_window_widget (string s)
// creates a decorated window with name s and contents w
{
  view->setWindowTitle(to_qstring(s));
  //	return this;
  widget wid =  new qt_window_widget_rep(view); //FIXME: is this the right thing to do?
  return wid; 
}


qt_tm_widget_rep::qt_tm_widget_rep():
  qt_view_widget_rep (new QMainWindow ()), helper (this)
{
  QMainWindow *mw = tm_mainwindow ();
 
  QScrollArea *sa = new QScrollArea (mw);
  sa->setBackgroundRole (QPalette::Dark);
  mw->setCentralWidget (sa);

  QStatusBar *bar = new QStatusBar(mw);
  leftLabel = new QLabel ("", mw);
  rightLabel = new QLabel ("", mw);
  leftLabel->setFrameStyle (QFrame::NoFrame);
  rightLabel->setFrameStyle (QFrame::NoFrame);
  bar->addWidget (leftLabel);
  bar->addPermanentWidget (rightLabel);
  bar->setStyle (qtmstyle ());
  mw->setStatusBar (bar);

  mainToolBar = new QToolBar("main toolbar", mw);
  contextToolBar = new QToolBar("context toolbar", mw);
  userToolBar = new QToolBar("user toolbar", mw);
  mainToolBar->setStyle(qtmstyle());
  contextToolBar->setStyle(qtmstyle());
  userToolBar->setStyle(qtmstyle());  
  mw->addToolBar(mainToolBar);
  mw->addToolBarBreak();
  mw->addToolBar(contextToolBar);
  mw->addToolBarBreak();
  mw->addToolBar(userToolBar);
  mw->addToolBarBreak();
  mw->setIconSize(QSize(17,17));
	
  mw->setFocusPolicy(Qt::StrongFocus);
}

qt_tm_widget_rep::~qt_tm_widget_rep () {
  if (DEBUG_EVENTS) 
    cout << "qt_tm_widget_rep::~qt_tm_widget_rep\n";
}

void
qt_tm_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_EXTENTS:
    {
      if (type_box (val) != type_helper<coord4>::id)
	fatal_error ("type mismatch", "SLOT_EXTENTS");
      coord4 p= open_box<coord4> (val);
      //cout << "p= " << p << "\n";
      QSize sz= to_qrect (p).size ();
      // [view window] setContentSize:rect.size];
      QSize ws= tm_scrollarea () -> size ();
      sz.setHeight (max (sz.height (), 7 * ws.height () / 8));
      tm_scrollarea () -> setAlignment (Qt::AlignCenter);
      tm_canvas () -> setFixedSize (sz);
    }
    break;
  case SLOT_INVALIDATE_ALL:
    {
      if (!is_nil (val))
	fatal_error ("type mismatch", "SLOT_INVALIDATE_ALL");
      view->update();
    }
    break;
			
  case SLOT_HEADER_VISIBILITY:
    // send_bool (THIS, "header", val);
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    {
    if (type_box (val) != type_helper<bool>::id)
      fatal_error ("type mismatch", "SLOT_MAIN_ICONS_VISIBILITY");
    bool f= open_box<bool> (val);
    mainToolBar->setVisible(f);
    }
    break;
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    {
    if (type_box (val) != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_CONTEXT_ICONS_VISIBILITY");
    bool f= open_box<bool> (val);
    contextToolBar->setVisible(f);
    }
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    {
    if (type_box (val) != type_helper<bool>::id)
      fatal_error ("type mismatch", "SLOT_USER_ICONS_VISIBILITY");
    bool f= open_box<bool> (val);
    userToolBar->setVisible(f);
    }
    break;
  case SLOT_FOOTER_VISIBILITY:
    {
      if (type_box (val) != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_FOOTER_VISIBILITY");
      bool f= open_box<bool> (val);
      tm_mainwindow()->statusBar()->setVisible(f);
    }
      break;

    case SLOT_LEFT_FOOTER:
    {
      if (type_box (val) != type_helper<string>::id)
        fatal_error ("type mismatch", "SLOT_LEFT_FOOTER");
      string msg = open_box<string> (val);
      leftLabel->setText (to_qstring_utf8 (qt_translate (msg)));
      leftLabel->update();
    }
			
  //  if (DEBUG_EVENTS) cout << "left footer\n";
    //			send_string (THIS, "left footer", val);
    break;
  case SLOT_RIGHT_FOOTER:
    {
      if (type_box (val) != type_helper<string>::id)
        fatal_error ("type mismatch", "SLOT_RIGHT_FOOTER");
      string msg = open_box<string> (val);
      rightLabel->setText (to_qstring_utf8 (qt_translate (msg)));
      rightLabel->update ();
    }
			
 //   if (DEBUG_EVENTS) cout << "right footer\n";
    //			send_string (THIS, "right footer", val);
    break;
			
  case SLOT_SCROLL_POSITION:
    {
      if (type_box (val) != type_helper<coord2>::id)
        fatal_error ("type mismatch", "SLOT_SCROLL_POSITION");
      coord2 p= open_box<coord2> (val);
      QPoint pt = to_qpoint(p);
      // conver from main widget to canvas coordinates
      //QPoint pt2 = tm_mainwindow()->mapToGlobal(pt);
      //pt = tm_scrollarea()->widget()->mapFromGlobal(pt2);
      if (DEBUG_EVENTS) cout << "Position (" << pt.x() << "," << pt.y() << ")\n ";
#if 0
			cout << "scrollarea (" << tm_scrollarea()->x() << "," <<  tm_scrollarea()->y() << "," 
			     <<  tm_scrollarea()->width() << "," <<  tm_scrollarea()->height() << ")\n";
			cout << "widget     (" << tm_scrollarea()->widget()->x() << "," <<  tm_scrollarea()->widget()->y() << "," 
			<<  tm_scrollarea()->widget()->width() << "," <<  tm_scrollarea()->widget()->height() << ")\n";
			
			cout << "GOING TO (" << pt.x()+tm_scrollarea()->width()/2 << "," << pt.y()+tm_scrollarea()->height()/2 << ")\n";
#endif
			// It is still not very clear to me because this shift of half h/w size works...
			tm_scrollarea()->ensureVisible(pt.x()+tm_scrollarea()->width()/2,pt.y()+tm_scrollarea()->height()/2);
      //			tm_scrollarea()->widget()->move(pt);
      //[[sv documentView] scrollPoint:pt];
      //			[[(NSScrollView*)view documentView] scrollRectToVisible:NSMakeRect(pt.x,pt.y,1.0,1.0)];
      tm_scrollarea()->update();
    }
    break;
  case SLOT_INTERACTIVE_MODE:
    {
      if (type_box (val) != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_INTERACTIVE_MODE");
      if (open_box<bool>(val) == true) {
        QTimer::singleShot(0,&helper, SLOT(doit()));
        //			 do_interactive_prompt();
      }
    }
    break;

  case SLOT_SHRINKING_FACTOR:
    {
      if (type_box (val) != type_helper<int>::id)
        fatal_error ("type mismatch", "SLOT_SHRINKING_FACTOR");
      if (QTMWidget *tmw = qobject_cast<QTMWidget *> (tm_canvas())) 
      {
        int new_sf = open_box<int> (val);
        if (DEBUG_EVENTS) cout << "New shrinking factor :" << new_sf << LF;
        tmw->tm_widget()->handle_set_shrinking_factor(new_sf);
      }
    }
    break;
      
			
  default:
    qt_view_widget_rep::send(s,val);
  }
}

#include "QTMInteractiveInputHelper.moc"

void QTMInteractiveInputHelper::doit()
{
  wid->do_interactive_prompt();
}


blackbox
qt_tm_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::query " << slot_name(s) << LF;

  switch (s) {
  case SLOT_SCROLL_POSITION:
    {
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_SCROLL_POSITION");
      QPoint pt = tm_canvas()->pos();
      if (DEBUG_EVENTS)
	cout << "Position (" << pt.x() << "," << pt.y() << ")\n"; 
      return close_box<coord2> (from_qpoint(pt));
    }

  case SLOT_EXTENTS:
    {
      if (type_id != type_helper<coord4>::id)
	fatal_error ("type mismatch", "SLOT_EXTENTS");
      QRect rect = tm_canvas()->geometry();
      coord4 c = from_qrect(rect);
      if (DEBUG_EVENTS)
	cout << "Canvas geometry " << rect << LF;
      return close_box<coord4> (c);
    }
			
	
  case SLOT_VISIBLE_PART:
    {
      if (type_id != type_helper<coord4>::id)
	fatal_error ("type mismatch", "SLOT_VISIBLE_PART");
      QRect rect = tm_canvas()->visibleRegion().boundingRect();
      coord4 c = from_qrect(rect);
      if (DEBUG_EVENTS)
	cout << "Visible Region " << rect << LF;
      return close_box<coord4> (c);
    }
			
  case SLOT_USER_ICONS_VISIBILITY:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_USER_ICONS_VISIBILITY");
      return close_box<bool> (userToolBar->isVisible());
    }
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_CONTEXT_ICONS_VISIBILITY");
      return close_box<bool> (contextToolBar->isVisible());
    }
      
  case SLOT_MAIN_ICONS_VISIBILITY:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_MAIN_ICONS_VISIBILITY");
      return close_box<bool> (mainToolBar->isVisible());
    }
      
  case SLOT_HEADER_VISIBILITY:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_HEADER_VISIBILITY");
      return close_box<bool> (true);
    }
  case SLOT_FOOTER_VISIBILITY:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_FOOTER_VISIBILITY");
      return close_box<bool> (tm_mainwindow()->statusBar()->isVisible());
    }
  case SLOT_INTERACTIVE_INPUT:
    {
      if (type_id != type_helper<string>::id)
        fatal_error ("type mismatch", "SLOT_INTERACTIVE_INPUT");
      return close_box<string> (((qt_input_text_widget_rep*) int_input.rep)->text );
      // return close_box<string> ("FIXME");
    }
  case SLOT_INTERACTIVE_MODE:
    {
      if (type_id != type_helper<bool>::id)
        fatal_error ("type mismatch", "SLOT_INTERACTIVE_MODE");
      return close_box<bool> (false); // FIXME: who needs this info?
    }

  default:
    return qt_view_widget_rep::query(s,type_id);
  }
}

#if 0
void
qt_tm_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::notify " << slot_name(s) << LF;
  switch (s) {
  default: ;
  }
  qt_view_widget_rep::notify (s, new_val);
}
#endif

widget
qt_tm_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::read " << slot_name(s) << LF;

  switch (s) {
  default:
    return qt_view_widget_rep::read(s,index);
  }
}

void
replaceActions (QWidget* dest, QWidget* src) {
  QList<QAction *> list = dest->actions();
  while (!list.isEmpty()) {
    QAction *a = list.takeFirst();
    dest->removeAction(a);
    a->deleteLater();
  }
//  cout << "replaceActions n:" << src->actions().count() << LF;
  list = src->actions();
  while (!list.isEmpty()) {
    QAction *a = list.takeFirst();
    dest->addAction(a);
    a->setParent(dest);
  }
  
  //dest->addActions(src->actions());
#if 0
  for(int i=0; i< list.count(); i++)
  {
    list[i]->setMenuRole(QAction::ApplicationSpecificRole);
  }
#endif
}

extern void
replaceButtons(QToolBar* dest, QWidget* src) {
  replaceActions(dest,src);
  QList<QObject *> list = dest->children();
  for (int i=0; i<list.count(); i++) {
    QToolButton *button = qobject_cast<QToolButton*>(list[i]);
    if (button)
      button->setPopupMode(QToolButton::InstantPopup);
  }
}

void
qt_tm_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_EVENTS)
    cout << "qt_tm_widget_rep::write " << slot_name(s) << LF;

  switch (s) {
  case SLOT_CANVAS: 
    {
      check_type_void (index, "SLOT_CANVAS");
      QWidget *qw =((qt_view_widget_rep*) w.rep)->view;
      QWidget *old_canvas = tm_scrollarea()->takeWidget(); 
      tm_scrollarea()->setWidget(qw);			
      (void) old_canvas;
      // old_canvas will be deleted when the corresponding qt_view_widget_rep is destroyed
      qw->setFocus();
      //FIXME: [[sv window] makeFirstResponder:v];
    }
    break;
  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    {
      QMenu *m = to_qmenu(w);
      QMenuBar *b =  new QMenuBar();
      replaceActions(b,m);
      tm_mainwindow()->setMenuBar(b);
      delete m;
    }
    break;
  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    {
      QMenu *m = to_qmenu(w);
      replaceButtons(mainToolBar,m);
      delete m;
    }
    break;
  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    {	
      QMenu *m = to_qmenu(w);
      replaceButtons(contextToolBar,m);
      delete m;
    }	
    break;
  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    {	
      QMenu *m = to_qmenu(w);
      replaceButtons(userToolBar,m);
      delete m;
    }
    break;
  case SLOT_INTERACTIVE_PROMPT:
    check_type_void (index, "SLOT_INTERACTIVE_PROMPT");
    int_prompt = concrete(w); 
    // THIS << set_widget ("interactive prompt", concrete (w));
    break;
  case SLOT_INTERACTIVE_INPUT:
    check_type_void (index, "SLOT_INTERACTIVE_INPUT");
    int_input = concrete(w);
    // THIS << set_widget ("interactive input", concrete (w));
    break;
  default:
    qt_view_widget_rep::write(s,index,w);
  }
}

widget
qt_tm_widget_rep::plain_window_widget (string s) {
  // creates a decorated window with name s and contents w
  widget w = qt_view_widget_rep::plain_window_widget(s);
  // to manage correctly retain counts
  qt_window_widget_rep * wid = (qt_window_widget_rep *)(w.rep);
  return wid;
}

qt_window_widget_rep::qt_window_widget_rep(QWidget *_wid):
  widget_rep(), wid(_wid)
{
  wid->setProperty("texmacs_window_widget",QVariant::fromValue((void*)this)); 
}

qt_window_widget_rep::~qt_window_widget_rep () {}

void
qt_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_SIZE:
    {
      if (type_box (val) != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_SIZE");
      coord2 p= open_box<coord2> (val);
      if (wid) {
	QSize size = to_qsize(p);
	wid->resize(size);
      }
    }
    break;
  case SLOT_POSITION:
    {
      if (type_box (val) != type_helper<coord2>::id)
	fatal_error ("type mismatch", "SLOT_POSITION");
      coord2 p= open_box<coord2> (val);
      if (wid) { 
	QPoint pt = to_qpoint(p); 
	pt.ry() += 40;
	// to avoid window under menu bar on MAC when moving at (0,0)
	if (DEBUG_EVENTS)
	  cout << "Moving to (" << pt.x() << "," << pt.y() << ")" << LF; 
	wid->move(pt);
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
      if (wid)
	wid->setWindowTitle(to_qstring(name));
    }
    break;
#if 0
  case SLOT_IDENTIFIER:
    check_type<int> (val, "SLOT_IDENTIFIER");
    THIS << emit_attach_window (get_window (open_box<int> (val)));
    break;
  case SLOT_FULL_SCREEN:
    check_type<bool> (val, "SLOT_FULL_SCREEN");
    win->set_full_screen (open_box<bool> (val));
    break;
  case SLOT_UPDATE:
    send_update (THIS, val);
    break;
  case SLOT_KEYBOARD:
    send_keyboard (THIS, val);
    break;
  case SLOT_KEYBOARD_FOCUS:
    send_keyboard_focus (THIS, val);
    break;
  case SLOT_MOUSE:
    send_mouse (THIS, val);
    break;
  case SLOT_MOUSE_GRAB:
    send_mouse_grab (THIS, val);
    break;
  case SLOT_MOUSE_POINTER:
    send_mouse_pointer (THIS, val);
    break;
  case SLOT_INVALIDATE:
    send_invalidate (THIS, val);
    break;
  case SLOT_INVALIDATE_ALL:
    send_invalidate_all (THIS, val);
    break;
  case SLOT_REPAINT:
    send_repaint (THIS, val);
    break;
  case SLOT_DELAYED_MESSAGE:
    send_delayed_message (THIS, val);
    break;
  case SLOT_DESTROY:
    send_destroy (THIS, val);
    break;
			
  case SLOT_SHRINKING_FACTOR:
    send_int (THIS, "shrinking factor", val);
    break;
  case SLOT_EXTENTS:
    send_coord4 (THIS, "extents", val);
    break;
  case SLOT_SCROLLBARS_VISIBILITY:
    send_int (THIS, "scrollbars", val);
    break;
  case SLOT_SCROLL_POSITION:
    send_coord2 (THIS, "scroll position", val);
    break;
			
  case SLOT_HEADER_VISIBILITY:
    send_bool (THIS, "header", val);
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    send_bool (THIS, "main icons", val);
    break;
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    send_bool (THIS, "context icons", val);
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    send_bool (THIS, "user icons", val);
    break;
  case SLOT_FOOTER_VISIBILITY:
    send_bool (THIS, "footer flag", val);
    break;
  case SLOT_LEFT_FOOTER:
    send_string (THIS, "left footer", val);
    break;
  case SLOT_RIGHT_FOOTER:
    send_string (THIS, "right footer", val);
    break;
  case SLOT_INTERACTIVE_MODE:
    send_bool (THIS, "interactive mode", val);
    break;
			
  case SLOT_STRING_INPUT:
    send_string (THIS, "input", val);
    break;
  case SLOT_INPUT_TYPE:
    send_string (THIS, "type", val);
    break;
  case SLOT_INPUT_PROPOSAL:
    send_string (THIS, "default", val);
    break;
  case SLOT_FILE:
    send_string (THIS, "file", val);
    break;
  case SLOT_DIRECTORY:
    send_string (THIS, "directory", val);
    break;
#endif			
  default:
    fatal_error ("cannot handle slot type", "qt_window_widget_rep::send");
  }
}


blackbox
qt_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_IDENTIFIER:
    if (type_id != type_helper<int>::id)
      fatal_error ("int expected (SLOT_IDENTIFIER)",
		   "qt_window_widget_rep::query");
    // we need only to know if the widget is attached to some gui window
    return close_box<int> (wid ? 1 : 0);
    // return close_box<int> ((int)wid);
  case SLOT_POSITION:  
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_POSITION)",
		     "qt_window_widget_rep::query");
      QPoint pt = wid->pos();
      if (DEBUG_EVENTS)
	cout << "Position (" << pt.x() << "," << pt.y() << ")\n";
      return close_box<coord2> (from_qpoint(pt));
    }
  case SLOT_SIZE:
    {
      typedef pair<SI,SI> coord2;
      if (type_id != type_helper<coord2>::id)
	fatal_error ("type mismatch (SLOT_SIZE)",
		     "qt_window_widget_rep::query");
      QSize s = wid->size();
      return close_box<coord2> (from_qsize(s));
    }
#if 0
  case SLOT_RENDERER:
    if (type_id != type_helper<renderer>::id)
      fatal_error ("renderer expected (SLOT_RENDERER)",
		   "qt_window_widget_rep::query");
    return close_box<renderer> ((renderer) win);
			
  case SLOT_KEYBOARD_FOCUS:
    return query_keyboard_focus (THIS, type_id);
  case SLOT_MOUSE_GRAB:
    return query_mouse_grab (THIS, type_id);
			
  case SLOT_EXTENTS:
    return query_coord4 (THIS, "extents", type_id);
  case SLOT_VISIBLE_PART:
    return query_coord4 (THIS, "visible", type_id);
  case SLOT_SCROLLBARS_VISIBILITY:
    return query_int (THIS, "scrollbars", type_id);
  case SLOT_SCROLL_POSITION:
    return query_coord2 (THIS, "scroll position", type_id);
			
  case SLOT_HEADER_VISIBILITY:
    return query_bool (THIS, "header", type_id);
  case SLOT_MAIN_ICONS_VISIBILITY:
    return query_bool (THIS, "main icons", type_id);
  case SLOT_CONTEXT_ICONS_VISIBILITY:
    return query_bool (THIS, "context icons", type_id);
  case SLOT_USER_ICONS_VISIBILITY:
    return query_bool (THIS, "user icons", type_id);
  case SLOT_FOOTER_VISIBILITY:
    return query_bool (THIS, "footer flag", type_id);
  case SLOT_INTERACTIVE_MODE:
    return query_bool (THIS, "interactive mode", type_id);
  case SLOT_INTERACTIVE_INPUT:
    return query_string (THIS, "interactive input", type_id);
			
  case SLOT_STRING_INPUT:
    return query_string (THIS, "input", type_id);
#endif
  default:
    fatal_error ("cannot handle slot type", "qt_window_widget_rep::query");
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

  switch (s) {
#if 0
  case SLOT_SIZE:
    check_type<SI,SI> (new_val, "SLOT_SIZE");
    THIS << emit_resize ();
    if (is_window_widget ())
      send_size (THIS [0], new_val);
    break;
  case SLOT_POSITION:
    check_type<SI,SI> (new_val, "SLOT_POSITION");
    THIS << emit_move ();
    break;
  case SLOT_KEYBOARD_FOCUS:
    notify_keyboard_focus (THIS, new_val);
    break;
  case SLOT_MOUSE_GRAB:
    notify_mouse_grab (THIS, new_val);
    break;
#endif
  default: ;
  }
  widget_rep::notify (s, new_val);
}

widget
qt_window_widget_rep::read (slot s, blackbox index) {
  (void) index; 
  if (DEBUG_EVENTS)
    cout << "qt_window_widget_rep::read " << slot_name(s) << LF;

  switch (s) {
#if 0
  case SLOT_WINDOW:
    check_type_void (index, "SLOT_WINDOW");
    return win -> get_widget ();
  case SLOT_FORM_FIELD:
    check_type<int> (index, "SLOT_FORM_FIELD");
    return abstract (THIS [0] ["inputs"] [open_box<int> (index)] ["input"]);
  case SLOT_FILE:
    check_type_void (index, "SLOT_FILE");
    return abstract (THIS [0] ["file"] ["input"]);
  case SLOT_DIRECTORY:
    check_type_void (index, "SLOT_DIRECTORY");
    return abstract (THIS [0] ["directory"] ["input"]);
#endif
  default:
    fatal_error ("cannot handle slot type", "qt_window_widget_rep::read");
    return widget();
  }
}

void
qt_window_widget_rep::write (slot s, blackbox index, widget w) {
  (void) w;
  if (DEBUG_EVENTS)    
    cout << "qt_window_widget_rep::write " << slot_name(s) << LF;

  switch (s) {
  case SLOT_CANVAS:
    check_type_void (index, "SLOT_CANVAS");
    //			[(NSScrollView*)view setDocumentView: concrete (w)->get_nsview()];
    break;
  case SLOT_MAIN_MENU:
    check_type_void (index, "SLOT_MAIN_MENU");
    //			THIS << set_widget ("menu bar", concrete (w));
    break;
  case SLOT_MAIN_ICONS:
    check_type_void (index, "SLOT_MAIN_ICONS");
    //			THIS << set_widget ("main icons bar", concrete (w));
    break;
  case SLOT_CONTEXT_ICONS:
    check_type_void (index, "SLOT_CONTEXT_ICONS");
    //			THIS << set_widget ("context icons bar", concrete (w));
    break;
  case SLOT_USER_ICONS:
    check_type_void (index, "SLOT_USER_ICONS");
    //			THIS << set_widget ("user icons bar", concrete (w));
    break;
  case SLOT_INTERACTIVE_PROMPT:
    check_type_void (index, "SLOT_INTERACTIVE_PROMPT");
    //			THIS << set_widget ("interactive prompt", concrete (w));
    break;
  case SLOT_INTERACTIVE_INPUT:
    check_type_void (index, "SLOT_INTERACTIVE_INPUT");
    //			THIS << set_widget ("interactive input", concrete (w));
    break;
  default:
    fatal_error ("cannot handle slot type", "qt_window_widget_rep::write");
  }
}


/******************************************************************************
* simple_widget_rep
******************************************************************************/


/******************************************************************************
* Constructor
******************************************************************************/

simple_widget_rep::simple_widget_rep ()
  : qt_view_widget_rep (new QTMWidget(this)) 
{ 
  //	view->setProperty("texmacs_widget",QVariant::fromValue((void*)this));
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
//  if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::send " << slot_name(s) << LF;
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";

  qt_view_widget_rep::send(s, val);
}


blackbox
simple_widget_rep::query (slot s, int type_id) {
//  if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::query " << slot_name(s) << LF;
  if ((DEBUG_EVENTS) && (s != SLOT_RENDERER))
    cout << "[qt_simple_widget_rep] ";

  return qt_view_widget_rep::query(s,type_id);
}

void
simple_widget_rep::notify (slot s, blackbox new_val) 
{ 
//  if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::notify " << slot_name(s) << LF;
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";

  qt_view_widget_rep::notify (s, new_val);
}

/******************************************************************************
* Read and write access of subwidgets
******************************************************************************/

widget
simple_widget_rep::read (slot s, blackbox index) 
{
// if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::read " << slot_name(s) << LF;
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";
  return qt_view_widget_rep::read(s,index);
}

void
simple_widget_rep::write (slot s, blackbox index, widget w) 
{
//  if (DEBUG_EVENTS) cout << "qt_simple_widget_rep::write " << slot_name(s) << LF;
  if (DEBUG_EVENTS) cout << "[qt_simple_widget_rep] ";

  qt_view_widget_rep::write(s,index,w);
}




/******************************************************************************
* Window widgets
******************************************************************************/


widget plain_window_widget (widget w, string s) 
// creates a decorated window with name s and contents w
{
  return concrete(w)->plain_window_widget(s);
}

widget popup_window_widget (widget w, string s) 
// creates an undecorated window with name s and contents w
{
  return concrete(w)->popup_window_widget(s);
}

void   destroy_window_widget (widget w) { (void) w;  } //FIXME: Handle correcly
// destroys a window as created by the above routines

/******************************************************************************
* Top-level widgets, typically given as an argument to plain_window_widget
* See also message.hpp for specific messages for these widgets
******************************************************************************/

widget texmacs_widget (int mask, command quit) 
// the main TeXmacs widget and a command which is called on exit
// the mask variable indicates whether the menu, icon bars, status bar, etc.
// are visible or not
{
  (void) mask; (void) quit; // FIXME: handle correctly mask and quit
  widget w = new qt_tm_widget_rep();
  return w; 
}





widget popup_widget (widget w) 
// a widget container which results w to be unmapped as soon as
// the pointer quits the widget
// used in edit_mouse.cpp to implement a contextual menu in the canvas
{
  return concrete(w)->make_popup_widget();
}


/******************************************************************************
*  Widgets which are not strictly required by TeXmacs
*  their implementation is void
******************************************************************************/

widget empty_widget () { return widget(); }
// an empty widget of size zero
widget glue_widget (bool hx, bool vx, SI w, SI h) 
//{ return widget(); }
// an empty widget of minimal width w and height h and which is horizontally
// resp. vertically extensible if hx resp. vx is true
{
  (void)hx; (void)vx; (void)w; (void)h;
  //FIXME: Implement
  return new qt_view_widget_rep (new QWidget());
}
widget wait_widget (SI width, SI height, string message) 
// a widget of a specified width and height, displaying a wait message
// this widget is only needed when using the X11 plugin
{ 
  (void) width; (void) height; (void) message;
  if (DEBUG_EVENTS) cout << "wait_widget IS STILL NOT IMPLEMENTED\n";
  return widget(); 
}




