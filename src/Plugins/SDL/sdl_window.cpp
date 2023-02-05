
/******************************************************************************
* MODULE     : sdl_window.cpp
* DESCRIPTION: Windows under SDL
* COPYRIGHT  : (C) 2022  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// FIXME: rename?
// REASON: this code is abstracted from SDL and depends only of a particular interface
// for the GUI object.

#include "sdl_window.hpp"

#include "message.hpp"
#include "boot.hpp"

#ifdef MUPDF_RENDERER
#include "mupdf_picture.hpp"
#endif

hashmap<int, window> id_to_window (0);

/******************************************************************************
* Creation and deletion of an sdl_window
******************************************************************************/

static int serial= 1; // serial identifier for windows

void
sdl_window_rep::initialize () {

  id= serial++;
  id_to_window (id)= this;
  

  SI min_w= Min_w / PIXEL, min_h= Min_h / PIXEL;
  SI def_w= Def_w / PIXEL, def_h= Def_h / PIXEL;
  SI max_w= Max_w / PIXEL, max_h= Max_h / PIXEL;

  full_screen_flag= false;

  if (win_w == 0) win_w= def_w;
  if (win_h == 0) win_h= def_h;
  if ((win_x+ win_w) > gui->screen_width) win_x= gui->screen_width- win_w;
  if (win_x < 0) win_x= 0;
  if ((win_y+ win_h) > gui->screen_height) win_y= gui->screen_height- win_h;
  if (win_y < 0) win_y=0;
  
  if (N(name) == 0) {
    name= "popup";
    win= gui->create_window (id, name, win_x, win_y, win_w, win_h, true);

  } else {
    win= gui->create_window (id, name, win_x, win_y, win_w, win_h, false);
  }
  
  if (the_name == "") {
    the_name= name;
    mod_name= name;
  }

  gui->set_window_limits (win, min_w, min_h, max_w, max_h);

  backing_store= native_picture (win_w * retina_factor, win_h  * retina_factor, 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  
  // update widget state
  set_identifier (w, id);
  notify_position (w, 0, 0);
  notify_size (w, Def_w,  Def_h);

  gui->created_window (win);
  cout << "create window " << id << LF;
}

sdl_window_rep::sdl_window_rep (widget w2, sdl_gui gui2, string n2,
			    SI min_w, SI min_h, SI def_w, SI def_h,
			    SI max_w, SI max_h):
  window_rep (), w (w2), gui (gui2),
  orig_name (N(n2) == 0? string ("popup"): n2), name (n2),
  Min_w (min_w), Min_h (min_h), Def_w (def_w), Def_h (def_h),
  Max_w (max_w), Max_h (max_h),
  win_x (0), win_y (0), win_w (Def_w/PIXEL), win_h (Def_h/PIXEL),
  kbd_focus (w.rep), has_focus (false)
{
  //cout << "Min " << (min_w >> 8) << ", " << (min_h >> 8) << "\n";
  //cout << "Def " << (def_w >> 8) << ", " << (def_h >> 8) << "\n";
  //cout << "Max " << (max_w >> 8) << ", " << (max_h >> 8) << "\n";

  initialize ();
}

sdl_window_rep::~sdl_window_rep () {
  cout << "destroy window " << id << LF;
  gui->deleted_window (win);
  gui->destroy_window (win);
  delete_renderer (ren);
  
  id_to_window->reset (id);
  id= 0;
  set_identifier (w, 0); // FIXME: is this ok?
}

widget
sdl_window_rep::get_widget () {
  return w;
}

sdl_window
get_sdl_window (widget w) {
  int id= get_identifier (w);
  if (id == 0) return NULL;
  return (sdl_window)id_to_window[id];
}

int
get_identifier (window w) {
  if (w == NULL) return 0;
  else return (((sdl_window) w) -> id);
}

window
get_window (int id) {
  if (id == 0) return NULL;
  else return id_to_window [id];
}

/******************************************************************************
* Window appearance
******************************************************************************/

void
sdl_window_rep::get_position (SI& x, SI& y) {
  int xx, yy;
  gui->get_window_position (win, xx, yy);
  x=  xx*PIXEL;
  y= -yy*PIXEL;
}

void
sdl_window_rep::get_size (SI& ww, SI& hh) {
  ww= win_w*PIXEL;
  hh= win_h*PIXEL;
}

void
sdl_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

void
sdl_window_rep::set_position (SI x, SI y) {
  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > gui->screen_width) x= gui->screen_width- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > gui->screen_height) y= gui->screen_height- win_h;
  if (y<0) y=0;
  win_x= x;
  win_y= y;
  gui->set_window_position (win, win_x, win_y);
}

void
sdl_window_rep::set_size (SI w, SI h) {
  w= w/PIXEL; h= h/PIXEL;
  //h=-h; ren->decode (w, h);
  gui->set_window_size (win, w, h);
}

void
sdl_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  min_w= min_w/PIXEL; min_h= min_h/PIXEL;
  max_w= max_w/PIXEL; max_h= max_h/PIXEL;
  gui->set_window_limits (win, min_w, min_h, max_w, max_h);
}

void
sdl_window_rep::set_name (string name) {
  if (the_name != name) {
    gui->set_window_title (win, name);
    the_name= name;
    mod_name= name;
  }
}

string
sdl_window_rep::get_name () {
  return the_name;
}

void
sdl_window_rep::set_modified (bool flag) {
  string name= (flag? (the_name * " *"): the_name);
  if (mod_name != name) {
    gui->set_window_title (win, name);
    mod_name= name;
  }
}

void
sdl_window_rep::set_visibility (bool flag) {
  gui->set_window_visibility (win, flag);
}

void
sdl_window_rep::set_full_screen (bool flag) {
  if (full_screen_flag == flag) return;
  string old_name= get_name ();
  if (old_name == "")
    old_name=  name;
  if (flag) {
    save_win= win;
    name= string ();
    save_x= win_x; save_y= win_y;
    save_w= win_w; save_h= win_h;
//    initialize ();
    gui->set_window_fullscreen (win, true);
    move_event   (0, 0);
    resize_event (gui->screen_width, gui->screen_height);
    set_visibility (true);
//    XSetInputFocus (dpy, win, PointerRoot, CurrentTime);
  }
  else {
    gui->set_window_fullscreen (win, false);
    win= save_win;
    //FIXME: is this 'as_charp' a possible memory leak?
    name= old_name;
    win_x= save_x; win_y= save_y;
    win_w= save_w; win_h= save_h;
    set_visibility (true);
    gui->set_window_position (win, save_x, save_y);
    gui->set_window_size (win, save_w, save_h);
    resize_event (save_w, save_h);
    move_event   (save_x, save_y);
  }
  set_name (old_name);
  full_screen_flag= flag;
}

void
sdl_window_rep::move_event (int x, int y) {
  bool flag= (win_x!=x) || (win_y!=y);
  win_x= x; win_y= y;
  if (flag) {
 //   XWindowAttributes attrs;
 //   XGetWindowAttributes (dpy, win, &attrs);
 //   int border_x= attrs.x, border_y= attrs.y;
    int border_x=0, border_y=0;
    notify_position (w, win_x*PIXEL, win_y*PIXEL); //FIXME: not used???
    if (border_x >= 0 && border_x <= 5 && border_y >= 0 && border_y <= 30) {
      //cout << "Move to " << x-border_x << ", " << y-border_y << "\n";
      notify_window_move (orig_name, (x-border_x)*PIXEL, (border_y-y)*PIXEL);
    }
  }
}

void
sdl_window_rep::resize_event (int ww, int hh) {
  bool flag= (win_w!=ww) || (win_h!=hh);
  win_w= ww; win_h= hh;
  if (flag) {
    notify_size (w, win_w*PIXEL, win_h*PIXEL);
    notify_window_resize (orig_name, ww*PIXEL, hh*PIXEL);
  }
}

void
sdl_window_rep::destroy_event () {
  notify_window_destroy (orig_name);
  send_destroy (w);
}

/******************************************************************************
* Event handling
******************************************************************************/

void
sdl_window_rep::invalidate_event (int x1, int y1, int x2, int y2) {
//  cout << "invalidate " << x1 << ", " << y1 << ", "  << x2 << ", " << y2 << LF;
  invalid_regions= invalid_regions | rectangles (rectangle (x1, y1, x2, y2));
}

void
sdl_window_rep::key_event (string key) {
  send_keyboard (kbd_focus, key);
}

void
sdl_window_rep::focus_in_event () {
//  SDL_SetWindowKeyboardGrab (win, SDL_TRUE);
  has_focus= true;
  notify_keyboard_focus (kbd_focus, true);
  gui->focussed_window (win);
}

void
sdl_window_rep::focus_out_event () {
 // SDL_SetWindowKeyboardGrab (win, SDL_FALSE);
  has_focus= false;
  notify_keyboard_focus (kbd_focus, false);
}

void
sdl_window_rep::mouse_event (string ev, int x, int y, time_t t) {
  x *= retina_factor;
  y *= retina_factor;
  if (is_nil (gui->grab_ptr) ||
      get_sdl_window (gui->grab_ptr->item) == NULL) {
    ren->set_origin (0, 0);
    ren->encode (x, y);
    send_mouse (w, ev, x, y, gui->mouse_state, t);
  }
  else {
    sdl_window grab_win= get_sdl_window (gui->grab_ptr->item);
    int gw_x, gw_y;
    gui->get_window_position (grab_win->win, gw_x, gw_y);
    int w_x, w_y;
    gui->get_window_position (win, w_x, w_y);
    if (this != grab_win) {
//      x += win_x - grab_win->win_x;
//      y += win_y - grab_win->win_y;
      x += (w_x - gw_x)*retina_factor;
      y += (w_y - gw_y)*retina_factor;
      // return;
    }
    ren->set_origin (0, 0);
    ren->encode (x, y);
    send_mouse (gui->grab_ptr->item, ev, x, y, gui->mouse_state, t);
  }
}

void
sdl_window_rep::repaint_invalid_regions () {

  int bs_w= backing_store->get_width();
  int bs_h= backing_store->get_height();

  int new_bs_w, new_bs_h;
  gui->get_window_size (win, new_bs_w, new_bs_h);
  new_bs_w *= retina_factor;
  new_bs_h *= retina_factor;
  
  if ((new_bs_w != bs_w)   || (new_bs_h != bs_h)) {
    // the viewport size changed, reset the backing store
    
    // create a new backing store with updated viewport and the renderer
    picture new_backing_store= native_picture (new_bs_w, new_bs_h, 0, 0);
    renderer ren2= picture_renderer (new_backing_store, std_shrinkf * retina_factor);
    
    // copy the old backingstore
    SI x1=0, y1=0, x2=bs_w, y2=bs_h;
    ren->encode (x1, y1);
    ren->encode (x2, y2);
    ren2->fetch (x1, y2, x2, y1, ren, x1, y2);
    
    // compute new invalid regions
    // add new exposed regions due to resize
    if (new_bs_w > bs_w) {
      rectangle r = rectangle (bs_w, 0, new_bs_w, new_bs_h);
      invalid_regions = invalid_regions | rectangles (r);
    }
    if (new_bs_h > bs_h) {
      rectangle r = rectangle (0, bs_h, new_bs_w, new_bs_h);
      invalid_regions = invalid_regions | rectangles (r);
    }
    
    // update the state
    bs_w = new_bs_w;
    bs_h = new_bs_h;
    backing_store= new_backing_store;
    delete_renderer (ren);
    ren= ren2;
  }
  
  //invalid_regions= rectangles (rectangle (0,0, bs_w, bs_h));
  
  // repaint invalid rectangles if needed
  if (!is_nil (invalid_regions)) {
    rectangles new_regions;
    
    // simplify
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);
    
    while (!is_nil (invalid_regions)) {
      ren->set_origin (0, 0);
      rectangle r= copy (invalid_regions->item);
//      cout << "repaint " << r->x1 << ", " << r->y1 << ", "
//           << r->x2 << ", " << r->y2 << LF;
      r= thicken (r, 1, 1);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      send_repaint (w, ren, r->x1, r->y2, r->x2, r->y1);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1, true);
      if (gui_interrupted ())
        new_regions= rectangles (invalid_regions->item, new_regions);
      invalid_regions= invalid_regions->next;
    }
    invalid_regions= new_regions;
  
    // propagate immediately the changes to the screen
    gui->sync_window (win, backing_store);
  } // if (!is_nil (invalid_regions))
}

extern "C" {
// Additional Fitz API
void fz_copy_pixmap_rect(fz_context *ctx, fz_pixmap *dest, fz_pixmap *src, fz_irect b, const fz_default_colorspaces *default_cs);
}

void
sdl_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  ren->set_origin (0,0);
  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  ren->decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  rectangles extra= thicken (region - ::translate (region, dx, dy), 1, 1);
  invalid_regions= invalid_regions | extra;

  if (x1<x2 && y2<y1) {
//    cout << "translate " << x1 << ", " << y1 << ", " << x2 << ", " << y2 << ", " << X1 << ", " << Y2  << LF;
    //    XCopyArea (dpy, win, win, gc, x1, y2, x2-x1, y1-y2, X1, Y2);
    //FIXME: this should really be in mupdf_picture (copy_area)...
    fz_pixmap *pix= ((mupdf_picture_rep*)backing_store->get_handle())->pix;
    int w= fz_pixmap_width (mupdf_context (), pix);
    int h= fz_pixmap_height (mupdf_context (), pix);
    fz_pixmap *area= fz_new_pixmap (mupdf_context (),
                                   fz_device_rgb (mupdf_context ()),
                                   w, h, NULL, 1);
    fz_irect r= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context(), area, pix, r, NULL);
    area->x= dx;
    area->y= dy;
    fz_copy_pixmap_rect (mupdf_context(), pix, area, r, NULL);
    fz_drop_pixmap (mupdf_context(), area);
  }
}

void
sdl_window_rep::set_keyboard_focus (widget wid, bool get_focus) {
  ASSERT (get_focus, "explicit loss of keyboard focus not yet implemented");
  if (has_focus && (kbd_focus != wid.rep)) {
    notify_keyboard_focus (kbd_focus, false);
    notify_keyboard_focus (wid, true);
  }
  kbd_focus= wid.rep;
}

bool
sdl_window_rep::get_keyboard_focus (widget wid) {
  return has_focus && kbd_focus == wid.rep;
}

void
sdl_window_rep::set_mouse_grab (widget wid, bool get_grab) {
  if (get_grab) gui->obtain_mouse_grab (wid);
  else gui->release_mouse_grab ();
}

bool
sdl_window_rep::get_mouse_grab (widget w) {
  return gui->has_mouse_grab (w);
}

void
sdl_window_rep::set_mouse_pointer (widget wid, string name, string mask) {
  if (mask == "") gui->set_mouse_pointer (wid, name);
  else gui->set_mouse_pointer (wid, name, mask);
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
sdl_window_rep::delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  the_gui->messages= insert_message (the_gui->messages, wid, s, ct, ct+ delay);
}

/******************************************************************************
* Routines concerning regions in a window
******************************************************************************/

void
sdl_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  ren->set_origin(0, 0);
  ren->outer_round (x1, y1, x2, y2);
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  invalidate_event (x1, y2, x2, y1);
}

bool
sdl_window_rep::is_invalid () {
  return ! is_nil (invalid_regions);
}

void
sdl_window_rep::invalidate_all () {
  invalidate_event (0, 0, backing_store->get_width(), backing_store->get_height());
}

/******************************************************************************
* Interface
******************************************************************************/

window
popup_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  window win= tm_new<sdl_window_rep> (w, the_gui, string (),
				    min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}

window
plain_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  window win= tm_new<sdl_window_rep> (w, the_gui, name,
				    min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}
