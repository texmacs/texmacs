
/******************************************************************************
* MODULE     : x_display.cpp
* DESCRIPTION: X displays
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "timer.hpp"
#include "dictionary.hpp"
#include "X/x_display.hpp"
#include "X/x_drawable.hpp"
#include "X/x_window.hpp"
#include "iterator.hpp"
#include "image_files.hpp"
#include <X11/cursorfont.h>
extern hashmap<Window,pointer> Window_to_window;

/******************************************************************************
* Making color scales for anti alised fonts
******************************************************************************/

x_character_rep::x_character_rep (
  int c2, font_glyphs fng2, int sf2, color fg2, color bg2):
    c (c2), fng (fng2), sf (sf2), fg (fg2), bg (bg2) {}

x_character::x_character (int c, font_glyphs fng, int sf, color fg, color bg):
  rep (new x_character_rep (c, fng, sf, fg, bg)) {}

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
  return xc->c ^ ((long) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf; }

void
x_display_rep::prepare_color (int sf, color fg, color bg) {
  int nr_cols= sf*sf;
  if (nr_cols >= 64) nr_cols= 64;
  x_character col_entry (0, font_glyphs (), sf, fg, bg);
  color* cols= (color*) color_scale [col_entry];
  if (cols == NULL) {
    int fR, fG, fB, bR, bG, bB, j;
    get_rgb (fg, fR, fG, fB);
    get_rgb (bg, bR, bG, bB);
    cols= new color [nr_cols+1];
    for (j=0; j<=nr_cols; j++)
      cols [nr_cols-j]= rgb ((bR*j + fR*(nr_cols-j)) / nr_cols,
			     (bG*j + fG*(nr_cols-j)) / nr_cols,
			     (bB*j + fB*(nr_cols-j)) / nr_cols);
    color_scale (col_entry)= (void*) cols;
  }
}

/******************************************************************************
* Subroutines
******************************************************************************/

void
x_display_rep::set_button_state (unsigned int state) {
  int i= 0;
  if ((state & Button1Mask) != 0) i += 1;
  if ((state & Button2Mask) != 0) i += 2;
  if ((state & Button3Mask) != 0) i += 4;
  if ((state & Button4Mask) != 0) i += 8;
  if ((state & Button5Mask) != 0) i += 16;
  x_display_rep::state= i;
}

void
x_display_rep::emulate_leave_enter (widget old_widget, widget new_widget) {
  Window root, child;
  SI root_x, root_y, x, y;
  unsigned int mask;

  XQueryPointer (dpy, ((x_window) old_widget->win)->win, &root, &child,
		 &root_x, &root_y, &x, &y, &mask);
  set_button_state (mask);
  x= (x * PIXEL);
  y= ((-y) * PIXEL);
  // cout << "\nLeave " << ((tree) old_widget) << " {\n";
  old_widget << emit_mouse ("leave", x, y, 0, state);
  // cout << "}\n";

  XQueryPointer (dpy, ((x_window) new_widget->win)->win, &root, &child,
		 &root_x, &root_y, &x, &y, &mask);
  set_button_state (mask);
  x= (x * PIXEL);
  y= ((-y) * PIXEL);
  // cout << "Enter " << ((tree) new_widget) << " {\n";
  new_widget << emit_mouse ("enter", x, y, 0, state);  
  // cout << "}\n\n";
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
x_display_rep::grab_pointer (widget wid) {
  Window win= ((x_window) wid->win)->win;
  if ((!nil (grab_ptr)) && (wid==grab_ptr->item)) return;
  widget old_widget; if (!nil (grab_ptr)) old_widget= grab_ptr->item;
  grab_ptr= list<widget> (wid, grab_ptr);
  widget new_widget= grab_ptr->item;
  XGrabPointer (dpy, win, false,
		PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
  // cout << "\n---> In grab " << pritty ((tree) wid) << "\n\n";
  if (!nil (old_widget)) emulate_leave_enter (old_widget, new_widget);
}

void
x_display_rep::ungrab_pointer () {
  if (nil (grab_ptr)) return;
  widget old_widget= grab_ptr->item;
  grab_ptr= grab_ptr->next;
  widget new_widget; if (!nil (grab_ptr)) new_widget= grab_ptr->item;
  if (nil (grab_ptr)) {
    XUngrabPointer (dpy, CurrentTime);
    // cout << "\n---> No grab\n\n";
  }
  else {
    x_window grab_win= (x_window) new_widget->win;
    XGrabPointer (dpy, grab_win->win, false,
		  PointerMotionMask | ButtonPressMask | ButtonReleaseMask,
		  GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
    // cout << "\n---> In grab " << pritty ((tree) new_widget) << "\n\n";
    emulate_leave_enter (old_widget, new_widget);
  }
}

bool
x_display_rep::has_grab_pointer (widget w) {
  return (!nil (grab_ptr)) && (grab_ptr->item == w);
}

void
x_display_rep::grab_keyboard (widget wid) { (void) wid;
  fatal_error ("Not yet implemented", "x_display_rep::grab_keyboard");
  // XSetInputFocus (dpy, win, RevertToNone, CurrentTime);
}

void
x_display_rep::ungrab_keyboard () {
  fatal_error ("Not yet implemented", "x_display_rep::ungrab_keyboard");
  // XSetInputFocus (dpy, win, RevertToNone, CurrentTime);
}

/******************************************************************************
* Selections
******************************************************************************/

Bool
my_predicate (Display* dpy, XEvent* ev, XPointer arg) { (void) dpy;
  x_window win= (x_window) arg;
  return (win->win==ev->xany.window) && (ev->type==SelectionNotify);
}

tree
x_display_rep::get_selection (widget wid, string key) {
  if (selections->contains (key)) return copy (selections [key]);
  if (key != "primary") return "none";
  if (XGetSelectionOwner (dpy, XA_PRIMARY) == None) return "none";
  
  Window win= ((x_window) wid->win)->win;
  Atom data= XInternAtom (dpy, "MY_STRING_SELECTION", false);
  XConvertSelection (dpy, XA_PRIMARY, XA_STRING, data, win, CurrentTime);

  int i;
  XEvent ev;
  for (i=0; i<1000000; i++)
    if (XCheckIfEvent (dpy, &ev, my_predicate,
		       (XPointer) ((x_window) wid->win))) break;
  if (i==1000000) return "none";
  XSelectionEvent& sel= ev.xselection;

  string s ("");
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
  return tuple ("extern", s);
}

bool
x_display_rep::set_selection (widget wid, string key, tree t, string s) {
  selections (key)= copy (t);
  if (key == "primary") {
    Window win= ((x_window) wid->win)->win;
    if (selection!=NULL) delete[] selection;
    XSetSelectionOwner (dpy, XA_PRIMARY, win, CurrentTime);
    if (XGetSelectionOwner(dpy, XA_PRIMARY)==None) return false;
    selection= as_charp (s);
  }
  return true;
}

void
x_display_rep::clear_selection (string key) {
  selections->reset (key);
  if ((key == "primary") && (selection != NULL)) {
    delete[] selection;
    selection= NULL;
  }
}

/******************************************************************************
* Delayed messages
******************************************************************************/

message_rep::message_rep (widget wid2, string s2, time_t t2):
  wid (wid2), s (s2), t (t2) {}
message::message (widget wid, string s, time_t t):
  rep (new message_rep (wid, s, t)) {}

ostream&
operator << (ostream& out, message m) {
  return out << "message " << m->s << " to " << m->wid
	     << "at time " << m->t << "\n";
}

static list<message>
insert_message (list<message> l, widget wid, string s, time_t cur, time_t t) {
  if (nil (l)) return list<message> (message (wid, s, t));
  time_t ref= l->item->t;
  if ((t-cur) <= (ref-cur)) return list<message> (message (wid, s, t), l);
  return list<message> (l->item, insert_message (l->next, wid, s, cur, t));
}

static list<message>
remove_all_messages (list<message> l, widget wid, string s, int& found) {
  if (nil (l))
    return l;
  else if (l->item->wid == wid && l->item->s == s) {
    found++;
    return remove_all_messages (l->next, wid, s, found);
  }
  else
    return list<message>(l->item, remove_all_messages(l->next, wid, s, found));
};

void
x_display_rep::delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  messages= insert_message (messages, wid, s, ct, ct+ delay);
}

int
x_display_rep::remove_all_delayed_messages (widget wid, string s) {
  int found= 0;
  messages= remove_all_messages (messages, wid, s, found);
  return found;
}

/******************************************************************************
* Set the current language
******************************************************************************/

void
x_display_rep::load_dictionary (string name, string from, string to) {
  dictionary dict= ::load_dictionary (from, to);
  dict->load (name);
}

void
x_display_rep::set_output_language (string s) {
  out_lan= s;

  iterator<Window> it= iterate (Window_to_window);
  while (it->busy()) {
    x_window win= (x_window) Window_to_window[it->next()];
    bool flag;
    win->w << ::set_language (s, flag);
    if (flag && win->w->attached ()) win->w << emit_update ();
  }
}

string
x_display_rep::get_output_language () {
  return out_lan;
}

string
x_display_rep::translate (string s, string from, string to) {
  if (N(from)==0) return s;
  dictionary dict= ::load_dictionary (from, to);
  return dict->translate (s);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
x_display_rep::set_help_balloon (widget wid, SI x, SI y) {
  unmap_balloon ();
  balloon_wid = wid;
  balloon_win = NULL;
  balloon_x   = x;
  balloon_y   = y;
  balloon_time= texmacs_time ();
}

void
x_display_rep::map_balloon () {
  balloon_win= popup_window (balloon_wid, balloon_x, balloon_y);
  balloon_win->map ();
}

void
x_display_rep::unmap_balloon () {
  if (!nil (balloon_wid)) {
    if (balloon_win != NULL) {
      balloon_win->unmap ();
      delete balloon_win;
      balloon_win= NULL;
    }
    balloon_wid= widget ();
  }
}

window (*get_current_window) (void)= NULL; // FIXME: dirty hack

// Definitions from X11/cursorfont.h
static
int fetch_X11_cursor_no (string name) {
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
x_display_rep::set_pointer (string name) {
  int no= fetch_X11_cursor_no (name);
  if (no==-1) return;
  Cursor cursor=XCreateFontCursor(dpy, no);
  if (get_current_window != NULL)
    XDefineCursor(dpy, ((x_window_rep*)get_current_window())->win, cursor);
}

void
x_display_rep::set_pointer (string curs_name, string mask_name) {
  static hashmap<string,tree> xpm_cache ("");
  if (mask_name=="") mask_name= curs_name;
  x_drawable_rep dra= x_drawable_rep (this);
  dra.xpm_initialize (curs_name);
  if (mask_name!=curs_name) dra.xpm_initialize (mask_name);
  dra.~x_drawable_rep ();
  Pixmap curs= (Pixmap) xpm_bitmap [curs_name];
  Pixmap mask= (Pixmap) xpm_bitmap [mask_name];

  if (!xpm_cache->contains (curs_name))
    xpm_cache (curs_name)= xpm_load (curs_name);

  if (!xpm_cache->contains (mask_name))
    xpm_cache (mask_name)= xpm_load (mask_name);

  array<string> cnames_curs= xpm_colors (xpm_cache[curs_name]);
  array<SI> hotspot= xpm_hotspot (xpm_cache[curs_name]);
  if (N(hotspot) == 0)
    fatal_error ("Missing hotspot", "x_display_rep::set_pointer");
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
  else
  if (XAllocColor (dpy, cols, &closest1)) fg= &closest1;
  else
    fatal_error ("Unable to allocate fgcolor", "x_display_rep::set_pointer");

  XColor exact2, closest2;
  XLookupColor(dpy, cols, bgcolor, &exact2, &closest2);
  if (XAllocColor (dpy, cols, &exact2)) bg= &exact2;
  else
  if (XAllocColor (dpy, cols, &closest2)) bg= &closest2;
  else
    fatal_error ("Unable to allocate bgcolor", "x_display_rep::set_pointer");

  delete[] bgcolor;
  delete[] fgcolor;

  SI x= hotspot[0], y= hotspot[1];
  Cursor cursor=XCreatePixmapCursor (dpy, curs, mask, fg, bg, x, y);
  if (get_current_window != NULL)
    XDefineCursor(dpy, ((x_window_rep*)get_current_window())->win, cursor);
}

void
x_display_rep::set_wait_indicator (string message, string arg) {
  if ((get_current_window == NULL) || (message == "")) return;
  if (arg != "") message= message * "#" * arg * "...";
  SI width= 400*PIXEL, height= 160*PIXEL;
  widget wait_wid= wait_widget (width, height, message);
  window wait_win= get_current_window ();
  x_window_rep* ww= (x_window_rep*) wait_win;
  SI mid_x= (ww->win_w>>1)*PIXEL, mid_y= -(ww->win_h>>1)*PIXEL + height;
  SI x1= mid_x- width/2, y1= mid_y- height/2;
  SI x2= mid_x+ width/2, y2= mid_y+ height/2;
  widget old_wid= ww->w;
  ww->w= wait_wid;
  wait_wid << emit_attach_window (ww);
  wait_wid << emit_position (x1, y1, x2-x1, y2-y1);
  wait_wid << emit_invalidate_all ();
  ww->repaint_invalid_regions ();
  ww->w= old_wid;
  XFlush (dpy);
  old_wid << emit_invalidate_all ();
}

void
beep () {
#ifdef OS_WIN32
  XBeep ();
#else
  cerr << '\a';
#endif
}
