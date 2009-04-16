
/******************************************************************************
* MODULE     : x_gui.cpp
* DESCRIPTION: Graphical user interface for X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "timer.hpp"
#include "dictionary.hpp"
#include "X11/x_gui.hpp"
#include "X11/x_drawable.hpp"
#include "X11/x_window.hpp"
#include "image_files.hpp"
#include <X11/cursorfont.h>
#include "message.hpp"

extern hashmap<Window,pointer> Window_to_window;

/******************************************************************************
* Making color scales for anti alised fonts
******************************************************************************/

x_character_rep::x_character_rep (
  int c2, font_glyphs fng2, int sf2, color fg2, color bg2):
    c (c2), fng (fng2), sf (sf2), fg (fg2), bg (bg2) {}

x_character::x_character (int c, font_glyphs fng, int sf, color fg, color bg):
  rep (tm_new<x_character_rep> (c, fng, sf, fg, bg)) {}

x_character::operator tree () {
  tree t (TUPLE,  as_string (rep->c), rep->fng->res_name);
  t << as_string (rep->sf) << as_string (rep->fg) << as_string (rep->bg);
  return t; }

bool operator == (x_character xc1, x_character xc2) {
  return
    (xc1->c==xc2->c) && (xc1->fng.rep==xc2->fng.rep) &&
    (xc1->sf==xc2->sf) && (xc1->fg==xc2->fg) && (xc1->bg==xc2->bg); }

bool operator != (x_character xc1, x_character xc2) {
  return
    (xc1->c!=xc2->c) || (xc1->fng.rep!=xc2->fng.rep) ||
    (xc1->sf!=xc2->sf) || (xc1->fg!=xc2->fg) || (xc1->bg!=xc2->bg); }

int hash (x_character xc) {
  return xc->c ^ ((intptr_t) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf; }

void
x_gui_rep::prepare_color (int sf, color fg, color bg) {
  int nr_cols= sf*sf;
  if (nr_cols >= 64) nr_cols= 64;
  x_character col_entry (0, font_glyphs (), sf, fg, bg);
  color* cols= (color*) color_scale [col_entry];
  if (cols == NULL) {
    int fR, fG, fB, bR, bG, bB, j;
    get_rgb_color (fg, fR, fG, fB);
    get_rgb_color (bg, bR, bG, bB);
    cols= tm_new_array<color> (nr_cols+1);
    for (j=0; j<=nr_cols; j++)
      cols [nr_cols-j]= rgb_color ((bR*j + fR*(nr_cols-j)) / nr_cols,
				   (bG*j + fG*(nr_cols-j)) / nr_cols,
				   (bB*j + fB*(nr_cols-j)) / nr_cols);
    color_scale (col_entry)= (void*) cols;
  }
}

/******************************************************************************
* Subroutines
******************************************************************************/

void
x_gui_rep::set_button_state (unsigned int state) {
  int i= 0;
  if ((state & Button1Mask) != 0) i += 1;
  if ((state & Button2Mask) != 0) i += 2;
  if ((state & Button3Mask) != 0) i += 4;
  if ((state & Button4Mask) != 0) i += 8;
  if ((state & Button5Mask) != 0) i += 16;
  if ((state & ShiftMask)   != 0) i += 256;
  if ((state & ControlMask) != 0) i += 512;
  if ((state & LockMask)    != 0) i += 1024;
  if ((state & Mod1Mask)    != 0) i += 2048;
  if ((state & Mod2Mask)    != 0) i += 4096;
  if ((state & Mod3Mask)    != 0) i += 8192;
  if ((state & Mod4Mask)    != 0) i += 16384;
  if ((state & Mod5Mask)    != 0) i += 32768;
  x_gui_rep::state= i;
}

void
x_gui_rep::emulate_leave_enter (widget old_widget, widget new_widget) {
  Window root, child;
  SI root_x, root_y, x, y;
  unsigned int mask;

  XQueryPointer (dpy, get_Window (old_widget),
		 &root, &child, &root_x, &root_y, &x, &y, &mask);
  set_button_state (mask);
  x= (x * PIXEL);
  y= ((-y) * PIXEL);
  // cout << "\nLeave " << old_widget << "\n";
  send_mouse (old_widget, "leave", x, y, state, 0);
  // cout << "Leave OK\n";

  XQueryPointer (dpy, get_Window (new_widget),
		 &root, &child, &root_x, &root_y, &x, &y, &mask);
  set_button_state (mask);
  x= (x * PIXEL);
  y= ((-y) * PIXEL);
  // cout << "Enter " << new_widget << "\n";
  send_mouse (new_widget, "enter", x, y, state, 0);
  // cout << "Enter OK\n\n";
}

/******************************************************************************
* Grabbing
******************************************************************************/

/*
string
pritty (tree t) {
  if (is_atomic (t)) return copy (as_string (t));
  else if (N(t) == 2) return pritty (t[1]);
  else {
    int i;
    string s ("(");
    for (i=1; i<N(t); i++) {
      if (i>1) s << " ";
      s << pritty (t[i]);
    }
    s << ")";
    return s;
  }
}
*/

void
x_gui_rep::obtain_mouse_grab (widget wid) {
  Window win= get_Window (wid);
  if ((!is_nil (grab_ptr)) && (wid==grab_ptr->item)) return;
  widget old_widget; if (!is_nil (grab_ptr)) old_widget= grab_ptr->item;
  grab_ptr= list<widget> (wid, grab_ptr);
  widget new_widget= grab_ptr->item;
  notify_mouse_grab (new_widget, true);
  XGrabPointer (dpy, win, false,
		PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
  // cout << "\n---> In grab " << pritty ((tree) wid) << "\n\n";
  if (!is_nil (old_widget)) {
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

void
x_gui_rep::release_mouse_grab () {
  if (is_nil (grab_ptr)) return;
  widget old_widget= grab_ptr->item;
  grab_ptr= grab_ptr->next;
  widget new_widget; if (!is_nil (grab_ptr)) new_widget= grab_ptr->item;
  if (is_nil (grab_ptr)) {
    XUngrabPointer (dpy, CurrentTime);
    // cout << "\n---> No grab\n\n";
  }
  else {
    x_window grab_win= get_x_window (new_widget);
    notify_mouse_grab (new_widget, true);    
    XGrabPointer (dpy, grab_win->win, false,
		  PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
		  GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
    // cout << "\n---> In grab " << new_widget << "\n";
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

bool
x_gui_rep::has_mouse_grab (widget w) {
  return (!is_nil (grab_ptr)) && (grab_ptr->item == w);
}

/******************************************************************************
* Selections
******************************************************************************/

Bool
my_predicate (Display* dpy, XEvent* ev, XPointer arg) { (void) dpy;
  x_window win= (x_window) arg;
  return (win->win==ev->xany.window) && (ev->type==SelectionNotify);
}

void
x_gui_rep::created_window (Window win) {
  windows_l << win;
}

void
x_gui_rep::deleted_window (Window win) {
  windows_l= remove (windows_l, win);
}

void
x_gui_rep::focussed_window (Window win) {
  windows_l= list<Window> (win, remove (windows_l, win));
}

bool
x_gui_rep::get_selection (string key, tree& t, string& s) {
  t= "none";
  s= "";
  if (selection_t->contains (key)) {
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
  if (key != "primary") return false;
  if (XGetSelectionOwner (dpy, XA_PRIMARY) == None) return false;
  
  if (is_nil (windows_l)) return false;
  Window win= windows_l->item;
  x_window x_win= (x_window) Window_to_window[win];
  Atom data= XInternAtom (dpy, "MY_STRING_SELECTION", false);
  XConvertSelection (dpy, XA_PRIMARY, XA_STRING, data, win, CurrentTime);

  int i;
  XEvent ev;
  for (i=0; i<1000000; i++)
    if (XCheckIfEvent (dpy, &ev, my_predicate, (XPointer) x_win))
      break;
  if (i==1000000) return false;
  XSelectionEvent& sel= ev.xselection;

  if (sel.property!=None) {
    long offset=0;
    Atom type;
    int fm;
    unsigned long n, remains;
    unsigned char* ret;
    
    do {
      XGetWindowProperty (dpy, win, sel.property,
			  offset, 1024, true, AnyPropertyType,
			  &type, &fm, &n, &remains, &ret);
      s << string ((char*) ret, n);
      offset += (n >> 2);
      XFree (ret);
    } while (remains>0);
  }
  t= tuple ("extern", s);
  return true;
}

bool
x_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  if (key == "primary") {
    if (is_nil (windows_l)) return false;
    Window win= windows_l->item;
    if (selection!=NULL) tm_delete_array (selection);
    XSetSelectionOwner (dpy, XA_PRIMARY, win, CurrentTime);
    if (XGetSelectionOwner(dpy, XA_PRIMARY)==None) return false;
    selection= as_charp (s);
  }
  return true;
}

void
x_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  if ((key == "primary") && (selection != NULL)) {
    tm_delete_array (selection);
    selection= NULL;
  }
}

bool
set_selection (string key, tree t, string s) {
  return the_gui->set_selection (key, t, s);
}

bool
get_selection (string key, tree& t, string& s) {
  return the_gui->get_selection (key, t, s);
}

void
clear_selection (string key) {
  the_gui->clear_selection (key);
}

/******************************************************************************
* X Pointers
******************************************************************************/

// Definitions from X11/cursorfont.h
static int
fetch_X11_cursor_no (string name) {
  string pref= name(0,3);
  if (pref!="XC_") return -1;
  name= name (3,N(name));
  switch (name[0]) {
    case 'X':
      if (name=="X_cursor") return XC_X_cursor;
    break;
    case 'a':
      if (name=="arrow") return XC_arrow;
    break;
    case 'b':
      if (name=="based_arrow_down") return XC_based_arrow_down;
      if (name=="based_arrow_up") return XC_based_arrow_up;
      if (name=="boat") return XC_boat;
      if (name=="bogosity") return XC_bogosity;
      if (name=="bottom_left_corner") return XC_bottom_left_corner;
      if (name=="bottom_right_corner") return XC_bottom_right_corner;
      if (name=="bottom_side") return XC_bottom_side;
      if (name=="bottom_tee") return XC_bottom_tee;
      if (name=="box_spiral") return XC_box_spiral;
    break;
    case 'c':
      if (name=="center_ptr") return XC_center_ptr;
      if (name=="circle") return XC_circle;
      if (name=="clock") return XC_clock;
      if (name=="coffee_mug") return XC_coffee_mug;
      if (name=="cross") return XC_cross;
      if (name=="cross_reverse") return XC_cross_reverse;
      if (name=="crosshair") return XC_crosshair;
    break;
    case 'd':
      if (name=="diamond_cross") return XC_diamond_cross;
      if (name=="dot") return XC_dot;
      if (name=="dotbox") return XC_dotbox;
      if (name=="double_arrow") return XC_double_arrow;
      if (name=="draft_large") return XC_draft_large;
      if (name=="draft_small") return XC_draft_small;
      if (name=="draped_box") return XC_draped_box;
    break;
    case 'e':
      if (name=="exchange") return XC_exchange;
    break;
    case 'f':
      if (name=="fleur") return XC_fleur;
    break;
    case 'g':
      if (name=="gobbler") return XC_gobbler;
      if (name=="gumby") return XC_gumby;
    break;
    case 'h':
      if (name=="hand1") return XC_hand1;
      if (name=="hand2") return XC_hand2;
      if (name=="heart") return XC_heart;
    break;
    case 'i':
      if (name=="icon") return XC_icon;
      if (name=="iron_cross") return XC_iron_cross;
    break;
    case 'l':
      if (name=="left_ptr") return XC_left_ptr;
      if (name=="left_side") return XC_left_side;
      if (name=="left_tee") return XC_left_tee;
      if (name=="leftbutton") return XC_leftbutton;
      if (name=="ll_angle") return XC_ll_angle;
      if (name=="lr_angle") return XC_lr_angle;
    break;
    case 'm':
      if (name=="man") return XC_man;
      if (name=="middlebutton") return XC_middlebutton;
      if (name=="mouse") return XC_mouse;
    break;
    case 'p':
      if (name=="pencil") return XC_pencil;
      if (name=="pirate") return XC_pirate;
      if (name=="plus") return XC_plus;
    break;
    case 'q':
      if (name=="question_arrow") return XC_question_arrow;
    break;
    case 'r':
      if (name=="right_ptr") return XC_right_ptr;
      if (name=="right_side") return XC_right_side;
      if (name=="right_tee") return XC_right_tee;
      if (name=="rightbutton") return XC_rightbutton;
      if (name=="rtl_logo") return XC_rtl_logo;
    break;
    case 's':
      if (name=="sailboat") return XC_sailboat;
      if (name=="sb_down_arrow") return XC_sb_down_arrow;
      if (name=="sb_h_double_arrow") return XC_sb_h_double_arrow;
      if (name=="sb_left_arrow") return XC_sb_left_arrow;
      if (name=="sb_right_arrow") return XC_sb_right_arrow;
      if (name=="sb_up_arrow") return XC_sb_up_arrow;
      if (name=="sb_v_double_arrow") return XC_sb_v_double_arrow;
      if (name=="shuttle") return XC_shuttle;
      if (name=="sizing") return XC_sizing;
      if (name=="spider") return XC_spider;
      if (name=="spraycan") return XC_spraycan;
      if (name=="star") return XC_star;
    break;
    case 't':
      if (name=="target") return XC_target;
      if (name=="tcross") return XC_tcross;
      if (name=="top_left_arrow") return XC_top_left_arrow;
      if (name=="top_left_corner") return XC_top_left_corner;
      if (name=="top_right_corner") return XC_top_right_corner;
      if (name=="top_side") return XC_top_side;
      if (name=="top_tee") return XC_top_tee;
      if (name=="trek") return XC_trek;
    break;
    case 'u':
      if (name=="ul_angle") return XC_ul_angle;
      if (name=="umbrella") return XC_umbrella;
      if (name=="ur_angle") return XC_ur_angle;
    break;
    case 'w':
      if (name=="watch") return XC_watch;
    break;
    case 'x':
      if (name=="xterm") return XC_xterm;
    break;
  }
  return -1;
}

void
x_gui_rep::set_mouse_pointer (widget w, string name) {
  int no= fetch_X11_cursor_no (name);
  if (no==-1) return;
  Cursor cursor=XCreateFontCursor(dpy, no);
  x_window win= get_x_window (w);
  if (win != NULL) XDefineCursor(dpy, win->win, cursor);
}

void
x_gui_rep::set_mouse_pointer (widget w, string name, string mask_name) {
  static hashmap<string,tree> xpm_cache ("");
  if (mask_name=="") mask_name= name;
  x_drawable_rep* dra= tm_new<x_drawable_rep> (this, 1, 1);
  dra->xpm_initialize (name);
  if (mask_name!=name) dra->xpm_initialize (mask_name);
  tm_delete (dra);
  Pixmap curs= (Pixmap) xpm_bitmap [name];
  Pixmap mask= (Pixmap) xpm_bitmap [mask_name];

  if (!xpm_cache->contains (name))
    xpm_cache (name)= xpm_load (name);

  if (!xpm_cache->contains (mask_name))
    xpm_cache (mask_name)= xpm_load (mask_name);

  array<string> cnames_curs= xpm_colors (xpm_cache[name]);
  array<SI> hotspot= xpm_hotspot (xpm_cache[name]);
  ASSERT (N(hotspot) != 0, "missing hotspot");
  array<string> cnames_mask= xpm_colors (xpm_cache[mask_name]);
  char* bgcolor= as_charp (N(cnames_mask)>1 ? cnames_mask[1] :
					      string ("white"));
  char* fgcolor= as_charp (N(cnames_curs)>1 ? cnames_curs[1] :
					      string ("black"));
  if (!strcmp (bgcolor, "none")) bgcolor= as_charp (string ("white"));
  if (!strcmp (fgcolor, "none")) fgcolor= as_charp (string ("white"));

  XColor *bg= NULL, *fg= NULL;
  XColor exact1, closest1;
  XLookupColor(dpy, cols, fgcolor, &exact1, &closest1);
  if (XAllocColor (dpy, cols, &exact1)) fg= &exact1;
  else if (XAllocColor (dpy, cols, &closest1)) fg= &closest1;
  else FAILED ("unable to allocate fgcolor");

  XColor exact2, closest2;
  XLookupColor(dpy, cols, bgcolor, &exact2, &closest2);
  if (XAllocColor (dpy, cols, &exact2)) bg= &exact2;
  else if (XAllocColor (dpy, cols, &closest2)) bg= &closest2;
  else FAILED ("unable to allocate bgcolor");

  tm_delete_array (bgcolor);
  tm_delete_array (fgcolor);

  SI x= hotspot[0], y= hotspot[1];
  Cursor cursor=XCreatePixmapCursor (dpy, curs, mask, fg, bg, x, y);
  x_window win= get_x_window (w);
  if (win != NULL) XDefineCursor(dpy, win->win, cursor);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
x_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  unmap_balloon ();
  balloon_wid = wid;
  balloon_win = NULL;
  balloon_x   = x;
  balloon_y   = y;
  balloon_time= texmacs_time ();
}

void
x_gui_rep::map_balloon () {
  widget win_wid= popup_window_widget (balloon_wid, "Balloon");
  set_position (win_wid, balloon_x, balloon_y);
  balloon_win= (window) get_x_window (win_wid);
  balloon_win->set_visibility (true);
}

void
x_gui_rep::unmap_balloon () {
  if (!is_nil (balloon_wid)) {
    if (balloon_win != NULL) {
      balloon_win->set_visibility (false);
      tm_delete (balloon_win);
      balloon_win= NULL;
    }
    balloon_wid= widget ();
  }
}

void
x_gui_rep::show_wait_indicator (widget w, string message, string arg) {
  // NOTE: the wait indicator is directly displayed inside the window
  // corresponding to w. We explicitly shortcut the main event loop
  // by invalidating the wait widget and requesting a redraw.
  // Using a popup window does not work, because it would be necessary
  // to return to the main loop to map and redraw it.
  x_window ww= get_x_window (w);
  if (ww == NULL || message == "") return;
  if (arg != "") message= message * "#" * arg * "...";
  SI width= 400*PIXEL, height= 160*PIXEL;
  widget wait_wid= wait_widget (width, height, message);
  SI mid_x= (ww->win_w>>1)*PIXEL, mid_y= -(ww->win_h>>1)*PIXEL + height;
  SI x= mid_x- width/2, y= mid_y- height/2;
  widget old_wid= ww->w;
  ww->w= wait_wid;
  set_position (wait_wid, x, y);
  set_identifier (wait_wid, (int) ww->win);
  send_invalidate_all (wait_wid);
  ww->repaint_invalid_regions ();
  ww->w= old_wid;
  XFlush (dpy);
  send_invalidate_all (old_wid);
}

bool
x_gui_rep::check_event (int type) {
  bool status;
  XEvent ev;
  switch (type) {
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      time_t now= texmacs_time ();
      if (now - interrupt_time < 0) return false;
      else interrupt_time= now + (100 / (XPending (dpy) + 1));
      interrupted= XCheckMaskEvent (dpy, KeyPressMask|ButtonPressMask, &ev);
      if (interrupted) XPutBackEvent (dpy, &ev);
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  case ANY_EVENT:
    return (XPending (dpy)>0);
  case MOTION_EVENT:
    status= XCheckMaskEvent (dpy, PointerMotionMask, &ev);
    if (status) XPutBackEvent (dpy, &ev);
    return status;
  case DRAG_EVENT:
    status= XCheckMaskEvent (dpy, ButtonMotionMask, &ev);
    if (status) XPutBackEvent (dpy, &ev);
    return status;
  case MENU_EVENT:
    status= XCheckMaskEvent (dpy, ButtonMotionMask|ButtonReleaseMask, &ev);
    if (status) XPutBackEvent (dpy, &ev);
    return status;
  }
  return interrupted;
}

void
beep () {
#ifdef OS_WIN32
  XBeep ();
#else
  cerr << '\a';
#endif
}

void
show_help_balloon (widget wid, SI x, SI y) {
  the_gui->show_help_balloon (wid, x, y);
}

void
show_wait_indicator (widget w, string message, string arg) {
  the_gui->show_wait_indicator (w, message, arg);
}

void
needs_update () {
}

bool
check_event (int type) {
  return the_gui->check_event (type);
}
