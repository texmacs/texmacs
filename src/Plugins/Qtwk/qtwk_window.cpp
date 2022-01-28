
/******************************************************************************
* MODULE     : qtwk_window.cpp
* DESCRIPTION: QT/Widkit window class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qtwk_window.hpp"
#include "message.hpp"
#include "Qt/qt_picture.hpp"
#include "Qt/qt_utilities.hpp"

#include "qtwk_gui.hpp"
#include "QTWKWindow.hpp"


#include "analyze.hpp" // starts

extern int nr_windows;

int window_unique_id= 1;

hashmap<int,pointer> Window_to_window (NULL);

/******************************************************************************
* Creation and deletion of a qtwk_window
******************************************************************************/

void
qtwk_window_rep::initialize () {
  SI min_w= Min_w, min_h= Min_h;
  SI def_w= Def_w, def_h= Def_h;
  SI max_w= Max_w, max_h= Max_h;

  QSize min = to_qsize (Min_w, Min_h);
  QSize max = to_qsize (Max_w, Max_h);
  QSize def = to_qsize (Def_w, Def_h);
  
  def_w= def.width(); def_h= def.height();
  min_w= min.width(); min_h= min.height();
  max_w= max.width(); max_h= max.height();
  
  //SI screen_width, screen_height;
  //gui->get_extents (screen_width, screen_height);

  win = new QTWKWindow (this);

  if (name == NULL) {
    win->setFlags(Qt::Popup);
    name= const_cast<char*> ("popup");
  } else {
    if (!starts(name, "TeXmacs")) {
      win->setFlags(Qt::WindowTitleHint | Qt::WindowSystemMenuHint
                      | Qt::WindowMinMaxButtonsHint | Qt::WindowCloseButtonHint);
    }
  }
  
  if (the_name == "") {
    the_name= name;
    mod_name= name;
  }

  win->setObjectName(to_qstring(the_name));
  win->setGeometry(QRect(win->geometry().topLeft(), QSize(def_w, def_h)));
  win->setMinimumSize(QSize(min_w, min_h));
  win->setMaximumSize(QSize(max_w, max_h));
  win->setBaseSize(QSize(def_w, def_h));

  cout << the_name << " init size " << def_w << " " << def_h << "\n";
  
  nr_windows++;
  Window_to_window (win_id)= (void*) this;

  set_identifier (w, win_id);
  notify_position (w, 0, 0);
  notify_size (w, Def_w, Def_h);
}

qtwk_window_rep::qtwk_window_rep (widget w2, qtwk_gui gui2, char* n2,
			    SI min_w, SI min_h, SI def_w, SI def_h,
			    SI max_w, SI max_h):
  window_rep (), w (w2), gui (gui2),
  orig_name (n2 == ((char*) NULL) ? string ("popup") : n2),
  name (n2),
  win_x (0), win_y (0),
  win_w (def_w), win_h (def_h),
  Min_w (min_w), Min_h (min_h),
  Def_w (def_w), Def_h (def_h),
  Max_w (max_w), Max_h (max_h),
  kbd_focus (w.rep),
  has_focus (false),
  full_screen_flag (false),
  win_id (window_unique_id++),
  backingPixmap (1,1) // avoid problems at startup with translate
{
  initialize ();
  gui->created_window (this);
}

qtwk_window_rep::~qtwk_window_rep () {
  set_identifier (w, 0);
  Window_to_window->reset (win_id);
  nr_windows--;
  gui->deleted_window (this);
  delete win;
}

widget
qtwk_window_rep::get_widget () {
  return w;
}

qtwk_window
get_qtwk_window (widget w) {
  int id= get_identifier (w);
  if (id == 0) return NULL;
  else return (qtwk_window) Window_to_window [id];
}

int
get_identifier (window w) {
  if (w == NULL) return 0;
  else return (((qtwk_window) w) -> win_id);
}

window
get_window (int id) {
  if (id == 0) return NULL;
  else return (window) ((qtwk_window) Window_to_window [id]);
}

/******************************************************************************
* Window mapping and appearance
******************************************************************************/

void
qtwk_window_rep::get_position (SI& x, SI& y) {
  // we need to use the top left corner of the content, without margins
  QPoint pos= win->geometry ().topLeft ();
  coord2 Pos= from_qpoint (pos);
  x= Pos.x1;
  y= Pos.x2;
}

void
qtwk_window_rep::get_size (SI& ww, SI& hh) {
  coord2 sz= from_qsize (win->size ());
  ww= sz.x1; hh= sz.x2;
}

void
qtwk_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

void
qtwk_window_rep::set_position (SI x, SI y) {
  x= x/PIXEL;
  y= -y/PIXEL;
//  cout << "move to " << x << "," << y << "\n";
  win->setPosition (x,y);

#if 0
  if ((x+ win_w) > gui->screen_width) x= gui->screen_width- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > gui->screen_height) y= gui->screen_height- win_h;
  if (y<0) y=0;
#endif
//  win->move (to_qpoint (coord2(x, y)));
}

void
qtwk_window_rep::set_size (SI w, SI h) {
  QSize sz=to_qsize (w, h);
  cout << "resize " <<  name << " to " << sz.width() << " " << sz.height() << "\n";
  win->resize (sz);
}

void
qtwk_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  //min_w= min_w/PIXEL; min_h= min_h/PIXEL;
  //max_w= max_w/PIXEL; max_h= max_h/PIXEL;
  cout << "set minimum size " <<  name << " to " << min_w << " " << min_h << "\n";
  cout << "set maximum size " <<  name << " to " << max_w << " " << max_h << "\n";

  win->setMinimumSize (to_qsize  (min_w, min_h));
  win->setMaximumSize (to_qsize (max_w, max_h));
}

void
qtwk_window_rep::set_name (string name) {
  if (the_name != name) {
    c_string s (name);
    win->setTitle (QString (s));
    the_name= name;
    mod_name= name;
  }
}

string
qtwk_window_rep::get_name () {
  return the_name;
}

void
qtwk_window_rep::set_modified (bool flag) {
  string name= (flag? (the_name * " *"): the_name);
  if (mod_name != name) {
    c_string s (name);
    win->setTitle (QString (s));
    mod_name= name;
  }
}

void
qtwk_window_rep::set_visibility (bool flag) {
  if (flag)  {
    if (starts(the_name, "TeXmacs")) win->show();
    else win->showNormal();
  } else win->hide();
}

void
qtwk_window_rep::set_full_screen (bool flag) {
#if 0
  if (full_screen_flag == flag) return;
  string old_name= get_name ();
  if (old_name == "")
    old_name= as_string (name);
  if (flag) {
    save_win= win;
    name= NULL;
    save_x= win_x; save_y= win_y;
    save_w= win_w; save_h= win_h;
    initialize ();
    XMoveResizeWindow (dpy, win, 0, 0,
		       gui->screen_width, gui->screen_height);
    move_event   (0, 0);
    resize_event (gui->screen_width, gui->screen_height);
    set_visibility (true);
    XSetInputFocus (dpy, win, PointerRoot, CurrentTime);
  }
  else {
    set_visibility (false);
    Window_to_window->reset (win_id);
    nr_windows--;
    XDestroyWindow (dpy, win);
    win= save_win;
    set_visibility (false);
    Window_to_window->reset (win_id);
    nr_windows--;
    XDestroyWindow (dpy, win);
    //FIXME: is this 'as_charp' a possible memory leak?
    name= as_charp (old_name);
    win_x= save_x; win_y= save_y;
    win_w= save_w; win_h= save_h;
    initialize ();
    set_visibility (true);
    XMoveResizeWindow (dpy, win, save_x, save_y, save_w, save_h);
    resize_event (save_w, save_h);
    move_event   (save_x, save_y);
  }
  set_name (old_name);
  full_screen_flag= flag;
#endif
}

void
qtwk_window_rep::move_event (int x, int y) {
  bool flag= 1;//(win_x!=x) || (win_y!=y);
  //win_x= x; win_y= y;
  if (flag) {
//    XWindowAttributes attrs;
//    XGetWindowAttributes (dpy, win, &attrs);
//    int border_x= attrs.x, border_y= attrs.y;
    int border_x = 0;
    int border_y = 0;

    notify_position (w, x*PIXEL, y*PIXEL);
    if (border_x >= 0 && border_x <= 5 && border_y >= 0 && border_y <= 30) {
      //cout << "Move to " << x-border_x << ", " << y-border_y << "\n";
      notify_window_move (orig_name, (x-border_x)*PIXEL, (border_y-y)*PIXEL);
    }
  }
}

void
qtwk_window_rep::resize_event (int ww, int hh) {
  bool flag= (win_w!=ww) || (win_h!=hh);
  win_w= ww; win_h= hh;
  if (flag) {
    notify_size (w, ww, hh);
    notify_window_resize (orig_name, ww, hh);
  }
}

void
qtwk_window_rep::destroy_event () {
  notify_window_destroy (orig_name);
  send_destroy (w);
}

/******************************************************************************
* Event handling
******************************************************************************/

void
qtwk_window_rep::invalidate_event (int x1, int y1, int x2, int y2) {
  invalid_regions= invalid_regions | rectangles (rectangle (x1, y1, x2, y2));
}

void
qtwk_window_rep::key_event (string key) {
  send_keyboard (kbd_focus, key);
}

void
qtwk_window_rep::focus_in_event () {
  //if (ic_ok) XSetICFocus (ic);
  win->setKeyboardGrabEnabled (true);
  has_focus= true;
  notify_keyboard_focus (kbd_focus, true);
  gui->focussed_window (this);
}

void
qtwk_window_rep::focus_out_event () {
 // if (ic_ok) XUnsetICFocus (ic);
  win->setKeyboardGrabEnabled (false);
  has_focus= false;
  notify_keyboard_focus (kbd_focus, false);
}

void
qtwk_window_rep::mouse_event (string ev, int x, int y, int flags, time_t t) {
  //cout << "mouse event " << (long int)((void*)this) << " "
  //     << ev << " " << x << " " << y << "\n";
  if (is_nil (gui->grab_ptr) || (get_qtwk_window (gui->grab_ptr->item) == NULL)) {
    send_mouse (w, ev, x, y, flags, t);
  }
  else {
    qtwk_window grab_win= get_qtwk_window (gui->grab_ptr->item);
    if (this != grab_win) {
      int win_x= win->x (), win_y= win->y ();
      int win2_x= grab_win->win->x (), win2_y= grab_win->win->y ();
      int dx=  win_x - win2_x, dy= win_y - win2_y;
      coord2 p= from_qpoint (QPoint (dx, dy));
      x += p.x1;
      y += p.x2;
      //cout << "-----\n";
      // return;
    }
    send_mouse (gui->grab_ptr->item, ev, x, y, flags, t);
  }
}

basic_renderer
qtwk_window_rep::get_renderer () {
  qt_renderer_rep *ren = the_qt_renderer ();
  ren->begin (&backingPixmap);
  return ren;
}

void
qtwk_window_rep::repaint_invalid_regions () {
  //if (!is_nil (invalid_regions)) cout << invalid_regions << "\n";
  //else { cout << "."; cout.flush (); }
  QRegion qrgn;

  {
    QSize _oldSize = backingPixmap.size();
    QSize _new_logical_Size = win->size();
    QSize _newSize = _new_logical_Size;
    _newSize *= retina_factor;
    
    //cout << "      surface size of " << _newSize.width() << " x "
    // << _newSize.height() << LF;
    
    if (_newSize != _oldSize) {
      // cout << "RESIZING BITMAP"<< LF;
      QPixmap newBackingPixmap (_newSize);
      QPainter p (&newBackingPixmap);
      p.drawPixmap (0,0,backingPixmap);
      //p.fillRect (0, 0, _newSize.width(), _newSize.height(), Qt::red);
      if (_newSize.width() >= _oldSize.width()) {
        invalidate_event (_oldSize.width(), 0, _newSize.width(), _newSize.height());
        p.fillRect (QRect (_oldSize.width(), 0, _newSize.width()-_oldSize.width(), _newSize.height()), Qt::gray);
      }
      if (_newSize.height() >= _oldSize.height()) {
        invalidate_event (0,_oldSize.height(), _newSize.width(), _newSize.height());
        p.fillRect (QRect (0,_oldSize.height(), _newSize.width(), _newSize.height()-_oldSize.height()), Qt::gray);
      }
      p.end();
      backingPixmap = newBackingPixmap;
    }
  }
  
  basic_renderer_rep* ren = get_renderer();

  rectangles new_regions;
  if (!is_nil (invalid_regions)) {
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);
  }
  while (!is_nil (invalid_regions)) {
    ren->set_origin (0, 0);
    rectangle r= copy (invalid_regions->item);
    
    rectangle r0 = invalid_regions->item;
    QRect qr = QRect (r0->x1 / retina_factor, r0->y1 / retina_factor,
                      (r0->x2 - r0->x1) / retina_factor,
                      (r0->y2 - r0->y1) / retina_factor);

    r= thicken (r, 1, 1);
    ren->encode (r->x1, r->y1);
    ren->encode (r->x2, r->y2);
    ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
    send_repaint (w, ren, r->x1, r->y2, r->x2, r->y1);
    if (gui_interrupted ())
      new_regions= rectangles (invalid_regions->item, new_regions);
    invalid_regions= invalid_regions->next;
    qrgn += qr;
  }
  invalid_regions= new_regions;
  
  ren->end();
  // propagate immediately the changes to the screen
  win->repaint (qrgn);
}

void
qtwk_window_rep::set_keyboard_focus (widget wid, bool get_focus) {
  //FIXME:
  ASSERT (get_focus, "explicit loss of keyboard focus not yet implemented");
  if (has_focus && (kbd_focus != wid.rep)) {
    notify_keyboard_focus (kbd_focus, false);
    notify_keyboard_focus (wid, true);
  }
  kbd_focus= wid.rep;
}

bool
qtwk_window_rep::get_keyboard_focus (widget wid) {
  return has_focus && kbd_focus == wid.rep;
}

void
qtwk_window_rep::set_mouse_grab (widget wid, bool get_grab) {
  if (get_grab) gui->obtain_mouse_grab (wid);
  else gui->release_mouse_grab ();
}

bool
qtwk_window_rep::get_mouse_grab (widget w) {
  return gui->has_mouse_grab (w);
}

void
qtwk_window_rep::set_mouse_pointer (widget wid, string name, string mask) {
  //FIXME: implement
#if 0
  if (mask == "") gui->set_mouse_pointer (wid, name);
  else gui->set_mouse_pointer (wid, name, mask);
#endif
}

/******************************************************************************
* Delayed messages
******************************************************************************/

message_rep::message_rep (widget wid2, string s2, time_t t2):
  wid (wid2), s (s2), t (t2) {}
message::message (widget wid, string s, time_t t):
  rep (tm_new<message_rep> (wid, s, t)) {}

tm_ostream&
operator << (tm_ostream& out, message m) {
  return out << "message " << m->s << " to " << m->wid
	     << "at time " << m->t << "\n";
}

static list<message>
insert_message (list<message> l, widget wid, string s, time_t cur, time_t t) {
  if (is_nil (l)) return list<message> (message (wid, s, t));
  time_t ref= l->item->t;
  if ((t-cur) <= (ref-cur)) return list<message> (message (wid, s, t), l);
  return list<message> (l->item, insert_message (l->next, wid, s, cur, t));
}

void
qtwk_window_rep::delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  the_gui->messages= insert_message (the_gui->messages, wid, s, ct, ct+ delay);
}

/******************************************************************************
* Routines concerning regions in a window
******************************************************************************/

void
qtwk_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  basic_renderer ren = get_renderer ();
  ren->set_origin (0,0);
  ren->clip (x1, y1, x2, y2);

  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  ren->decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

//  XEvent report;
//  while (XCheckWindowEvent (dpy, win, ExposureMask, &report))
//    gui->process_event (this, &report);

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  rectangles extra= thicken (region - ::translate (region, dx, dy), 1, 1);
  invalid_regions= invalid_regions | extra;

  if (x1<x2 && y2<y1)
    backingPixmap.scroll (dx, dy, x1, y2, x2-x1, y1-y2);

  ren->unclip ();
  ren->end ();
  
  // we need to propagate the changes from the backingPixmap to the window
  win->repaint();  
}

void
qtwk_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  qt_renderer_rep* ren = the_qt_renderer();
  ren->set_origin(0, 0);
  ren->outer_round (x1, y1, x2, y2);
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  invalidate_event (x1, y2, x2, y1);
}

bool
qtwk_window_rep::is_invalid () {
  return ! is_nil (invalid_regions);
}

/******************************************************************************
* Interface
******************************************************************************/

window
popup_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  window win= tm_new<qtwk_window_rep> (w, the_gui, (char*) NULL,
				    min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}

window
plain_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  c_string _name (name);
  window win= tm_new<qtwk_window_rep> (w, the_gui, _name,
				    min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}
