
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
#include "X/x_window.hpp"
#include "iterator.hpp"
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
  return xc->c ^ ((int) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf; }

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

void
x_display_rep::set_pointer (string pixmap_name) {
  (void) pixmap_name;
}

window (*get_current_window) (void)= NULL; // FIXME: dirty hack

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
