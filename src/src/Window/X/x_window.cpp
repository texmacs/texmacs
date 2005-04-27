
/******************************************************************************
* MODULE     : x_window.cpp
* DESCRIPTION: Windows under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "X/x_window.hpp"
#include "widget.hpp"

extern int    nr_windows;

hashmap<Window,pointer> Window_to_window (NULL);

/******************************************************************************
* Creation and deletion of an x_window
******************************************************************************/

void
x_window_rep::compute_size (SI& def_w, SI& def_h,
			    SI& min_w, SI& min_h, SI& max_w, SI& max_h)
{
  dis->get_extents (def_w, def_h);
  def_w >>= 1;
  def_h >>= 1;
  w << ::get_size (def_w, def_h, 0);

  min_w= def_w;
  min_h= def_h;
  w << ::get_size (min_w, min_h, -1);

  max_w= def_w;
  max_h= def_h;
  w << ::get_size (max_w, max_h, 1);
}

void
x_window_rep::set_hints (SI min_w, SI min_h, SI max_w, SI max_h) {
  XSizeHints* size_hints;
  XWMHints*   wm_hints;
  XClassHint* class_hints;
  if (!(size_hints= XAllocSizeHints ()))
    fatal_error ("out of memory (X server)", "set_attributes");
  if (!(wm_hints= XAllocWMHints ()))
    fatal_error ("out of memory (X server)", "set_attributes");
  if (!(class_hints= XAllocClassHint ()))
    fatal_error ("out of memory (X server)", "set_attributes");

  XTextProperty Window_Name;
  XTextProperty Icon_Name;
  if (XStringListToTextProperty (&name, 1, &Window_Name) == 0)
    fatal_error ("out of memory (X server)", "set_attributes");
  if (XStringListToTextProperty (&name, 1, &Icon_Name) == 0)
    fatal_error ("out of memory (X server)", "set_attributes");

  // int start_1= texmacs_time ();
  if (!dis->xpm_pixmap->contains ("TeXmacs.xpm"))
    xpm_initialize ("TeXmacs.xpm");
  Pixmap pm= (Pixmap) dis->xpm_pixmap ["TeXmacs.xpm"];
  // cout << "Getting pixmap required " << (texmacs_time ()-start_1) << " ms\n";

  // int start_2= texmacs_time ();
  size_hints->flags       = PPosition | PSize | PMinSize | PMaxSize;
  size_hints->min_width   = min_w;
  size_hints->min_height  = min_h;
  size_hints->max_width   = max_w;
  size_hints->max_height  = max_h;
  wm_hints->initial_state = NormalState;
  wm_hints->input         = true;
  wm_hints->icon_pixmap   = pm;
  wm_hints->flags         = StateHint | IconPixmapHint | InputHint;
  class_hints->res_name   = name;
  class_hints->res_class  = name;

  XSetWMProperties (
    dpy,
    win,
    &Window_Name,
    &Icon_Name,
    dis->argv,
    dis->argc,
    size_hints,
    wm_hints,
    class_hints
  );
  // cout << "Setting hints required " << (texmacs_time ()-start_2) << " ms\n";
}

void
x_window_rep::initialize () {
  dpy= dis->dpy;
  gc = dis->gc;
  full_screen_flag= false;
  
  // int start_1= texmacs_time ();
  SI def_w, def_h, min_w, min_h, max_w, max_h;
  compute_size (def_w, def_h, min_w, min_h, max_w, max_h);
  w << emit_attach_window (this);
  w << emit_position (0, 0, def_w, def_h);
  set_origin (0, 0);  
  decode (def_w, def_h); def_h= -def_h;
  decode (min_w, min_h); min_h= -min_h;
  decode (max_w, max_h); max_h= -max_h;
  // cout << "Size computation required " << (texmacs_time ()-start_1) << " ms\n";

  // int start_2= texmacs_time ();
  unsigned long valuemask= CWOverrideRedirect | CWSaveUnder;
  //unsigned long valuemask= CWOverrideRedirect | CWSaveUnder | CWBackingStore;
  XSetWindowAttributes setattr;
  setattr.override_redirect= (name==NULL);
  setattr.save_under       = True; // (name==NULL);
  // setattr.backing_store    = Always;
  // FIXME: backing store does not seem to work correctly
  if (win_w == 0) win_w= def_w;
  if (win_h == 0) win_h= def_h;
  if ((win_x+ win_w) > dis->display_width) win_x= dis->display_width- win_w;
  if (win_x < 0) win_x= 0;
  if ((win_y+ win_h) > dis->display_height) win_y= dis->display_height- win_h;
  if (win_y < 0) win_y=0;
  win= XCreateWindow (dpy, dis->root, win_x, win_y, win_w, win_h, 0,
		      dis->depth, InputOutput, CopyFromParent,
		      valuemask, &setattr);
  x_drawable_rep::win= (Drawable) win;
  // cout << "XWindow creation required " << (texmacs_time ()-start_2) << " ms\n";

  // cout << "Hints: " << min_w << ", " << min_h << " --- "
  // << max_w << ", " << max_h << "\n";
  if (name == NULL) name= "popup";
  if (the_name == "") the_name= name;
  set_hints (min_w, min_h, max_w, max_h);

  XSelectInput (dpy, win,
		ExposureMask | StructureNotifyMask |
		SubstructureNotifyMask | FocusChangeMask |
		PointerMotionMask | EnterWindowMask | LeaveWindowMask |
		ButtonPressMask | ButtonReleaseMask | KeyPressMask);

  Atom wm_protocols     = XInternAtom(dpy, "WM_PROTOCOLS", 1);
  Atom wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", 1);
  XSetWMProtocols (dpy, win, &wm_protocols, 1);
  XSetWMProtocols (dpy, win, &wm_delete_window, 1);

  nr_windows++;
  Window_to_window (win)= (void*) this;
}

x_window_rep::x_window_rep (widget w2, x_display dis2, char* n2):
  x_drawable_rep (dis2), window_rep (dis2), w (w2), dis (dis2),
  name (n2), win_x (0), win_y (0), win_w (0), win_h (0),
  kbd_focus (w.rep), has_focus (false)
{
  initialize ();
}

x_window_rep::x_window_rep (widget w2, x_display dis2, char* n2, SI x, SI y):
  x_drawable_rep (dis2), window_rep (dis2), w (w2), dis (dis2),
  name (n2), win_x (x/PIXEL), win_y (-y/PIXEL), win_w (0), win_h (0),
  kbd_focus (w.rep), has_focus (false)
{
  initialize ();
}

x_window_rep::x_window_rep (widget w2, x_display dis2, char* n2,
			    SI ww, SI hh, SI x, SI y):
  x_drawable_rep (dis2), window_rep (dis2), w (w2), dis (dis2), name (n2),
  win_x (x/PIXEL), win_y (-y/PIXEL), win_w (ww/PIXEL), win_h (hh/PIXEL),
  kbd_focus (w.rep), has_focus (false)
{
  initialize ();
}

x_window_rep::~x_window_rep () {
  w << emit_attach_window (NULL);

  XEvent report;
  while (XCheckWindowEvent (dpy, win, 0xffffffff, &report));

  Window_to_window->reset (win);
  nr_windows--;
  XDestroyWindow (dpy, win);
}

int
x_window_rep::get_type () {
  return PS_DEVICE_SCREEN;
}

/******************************************************************************
* Window apping and appearance
******************************************************************************/

void
x_window_rep::get_position (SI& x, SI& y) {
#ifdef OS_WIN32
  XGetWindowPos (dpy, win, &win_x, &win_y);
#endif
  x=  win_x*PIXEL;
  y= -win_y*PIXEL;
}

void
x_window_rep::get_size (SI& ww, SI& hh) {
  ww= win_w*PIXEL;
  hh= win_h*PIXEL;
}

void
x_window_rep::move (SI x, SI y) {
  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > dis->display_width) x= dis->display_width- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > dis->display_height) y= dis->display_height- win_h;
  if (y<0) y=0;
  XMoveWindow (dpy, win, x, y);
}

void
x_window_rep::resize (SI w, SI h) {
  h=-h; decode (w, h);
  XResizeWindow (dpy, win, w/PIXEL, h/PIXEL);
}

void
x_window_rep::set_name (string name) {
  char* s= as_charp (name);
  XStoreName (dpy, win, s);
  XSetIconName (dpy, win, s);
  delete[] s;
  the_name= name;
}

string
x_window_rep::get_name () {
  return the_name;
}

void
x_window_rep::map () {
  XMapRaised (dpy, win);
}

void
x_window_rep::unmap () {
  XUnmapWindow (dpy, win);  
}

void
x_window_rep::full_screen (bool flag) {
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
		       dis->display_width, dis->display_height);
    move_event   (0, 0);
    resize_event (dis->display_width, dis->display_height);
    map ();
    XSetInputFocus (dpy, win, PointerRoot, CurrentTime);
  }
  else {
    unmap ();
    Window_to_window->reset (win);
    nr_windows--;
    XDestroyWindow (dpy, win);
    win= save_win;
    unmap ();
    Window_to_window->reset (win);
    nr_windows--;
    XDestroyWindow (dpy, win);
    name= as_charp (old_name);
    win_x= save_x; win_y= save_y;
    win_w= save_w; win_h= save_h;
    initialize ();
    map ();
    XMoveResizeWindow (dpy, win, save_x, save_y, save_w, save_h);
    resize_event (save_w, save_h);
    move_event   (save_x, save_y);
  }
  set_name (old_name);
  full_screen_flag= flag;
}

void
x_window_rep::move_event (int x, int y) {
  bool flag= (win_x!=x) || (win_y!=y);
  win_x= x; win_y= y;
  if (flag) w << emit_move ();
}

void
x_window_rep::resize_event (int ww, int hh) {
  bool flag= (win_w!=ww) || (win_h!=hh);
  win_w= ww; win_h= hh;
  if (flag) w << emit_resize ();
  w << emit_position (0, 0, win_w*PIXEL, win_h*PIXEL);
}

void
x_window_rep::destroy_event () {
  w << emit_destroy ();
}

/******************************************************************************
* Event handling
******************************************************************************/

void
x_window_rep::invalidate_event (int x1, int y1, int x2, int y2) {
  invalid_regions= invalid_regions | rectangles (rectangle (x1, y1, x2, y2));
}

void
x_window_rep::key_event (string key) {
  kbd_focus << emit_keypress (key, 0);
}

void
x_window_rep::focus_in_event () {
  has_focus= true;
  kbd_focus << emit_keyboard_focus (true);
}

void
x_window_rep::focus_out_event () {
  has_focus= false;
  kbd_focus << emit_keyboard_focus (false);
}

void
x_window_rep::mouse_event (string ev, int x, int y, time_t t) {
  if (nil (dis->grab_ptr)) {
    set_origin (0, 0);
    encode (x, y);
    w << emit_mouse (ev, x, y, t, dis->state);
  }
  else {
    x_window grab_win= (x_window) dis->grab_ptr->item->win;
    if (((window) this) != dis->grab_ptr->item->win) {
      x += win_x- grab_win->win_x;
      y += win_y- grab_win->win_y;
      // return;
    }
    set_origin (0, 0);
    encode (x, y);
    dis->grab_ptr->item << emit_mouse (ev, x, y, t, dis->state);
  }
}

void
x_window_rep::repaint_invalid_regions () {
  rectangles new_regions;
  event_status=false;
  while (!nil (invalid_regions)) {
    set_origin (0, 0);
    rectangle r= copy (invalid_regions->item);
    encode (r->x1, r->y1);
    encode (r->x2, r->y2);
    x_drawable_rep::set_clipping (r->x1, r->y2, r->x2, r->y1);
    bool stop_flag= false;
    w << emit_repaint (r->x1, r->y2, r->x2, r->y1, stop_flag);
    switch (stop_flag) {
    case true : new_regions= rectangles (invalid_regions->item, new_regions);
    case false: invalid_regions= invalid_regions->next; break;
    default   : invalid_regions << new_regions; return;
    }
  }
  invalid_regions= new_regions;
}

void
x_window_rep::set_keyboard_focus (widget wid) {
  if (has_focus && (kbd_focus!=wid.rep)) {
    kbd_focus << emit_keyboard_focus (false);
    wid << emit_keyboard_focus (true);
  }
  kbd_focus= wid.rep;
}

/******************************************************************************
* Routines concerning regions in a window
******************************************************************************/

void
x_window_rep::clip (SI x1, SI y1, SI x2, SI y2) {
  rectangle r (cx1, cy1, cx2, cy2);
  clipping= rectangles (r, clipping);
  x_drawable_rep::set_clipping (x1, y1, x2, y2);
}

void
x_window_rep::unclip () {
  rectangle r (clipping->item);
  x_drawable_rep::set_clipping (r->x1- ox, r->y1- oy, r->x2- ox, r->y2- oy);
  clipping= clipping->next;
}

void
x_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  decode (x1, y1);
  decode (x2, y2);
  decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

  XEvent report;
  while (XCheckWindowEvent (dpy, win, ExposureMask, &report))
    dis->process_event (this, &report);

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  XCopyArea (dpy, win, win, gc, x1, y2, x2-x1, y1-y2, X1, Y2);
}

void
x_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  invalidate_event (x1, y2, x2, y1);
}

bool
x_window_rep::repainted () {
  return nil (invalid_regions);
}

ps_device
x_window_rep::window_to_shadow (SI x1, SI y1, SI x2, SI y2) {
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  SI scx1= x1+ ox;
  SI scy1= y1+ oy;
  SI scx2= x2+ ox;
  SI scy2= y2+ oy;
  decode (x1, y1);
  decode (x2, y2);
  if ((dis->shadow==NULL) || (x2 > dis->shadow->w) || (y1 > dis->shadow->h)) {
    SI w= max (x2, dis->display_width);
    SI h= max (y1, dis->display_height);
    if (dis->shadow != NULL) delete dis->shadow;
    dis->shadow= new x_drawable_rep (dis, w, h);
  } 
  XCopyArea (dpy, win, dis->shadow->win, gc, x1, y2, x2-x1, y1-y2, x1, y2);

  dis->shadow->ox = ox;
  dis->shadow->oy = oy;
  dis->shadow->cx1= scx1;
  dis->shadow->cy1= scy1;
  dis->shadow->cx2= scx2;
  dis->shadow->cy2= scy2;
  dis->shadow->event_status= event_status;
  dis->shadow_src= this;

  return dis->shadow;
}

void
x_window_rep::shadow_to_window (SI x1, SI y1, SI x2, SI y2) {
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  XCopyArea (dpy, dis->shadow->win, win, gc, x1, y2, x2-x1, y1-y2, x1, y2);
  event_status= event_status || dis->shadow->event_status;
}

/******************************************************************************
* Interface
******************************************************************************/

window
popup_window (widget w, SI x, SI y) {
  return new x_window_rep (w, (x_display) w->dis, NULL, x, y);
}

window
plain_window (widget w, char* name, SI width, SI height, SI x, SI y) {
  return new x_window_rep (w, (x_display) w->dis, name, width, height, x, y);
}
