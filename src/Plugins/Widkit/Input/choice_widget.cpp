
/******************************************************************************
* MODULE     : choice_widget.cpp
* DESCRIPTION: Select one or more items from a list
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"
#include "Widkit/basic_widget.hpp"
#include "Widkit/layout.hpp"
#include "Scheme/object.hpp"
#include "font.hpp"

/******************************************************************************
* choice widgets
******************************************************************************/

class choice_widget_rep: public basic_widget_rep {
  command       cb;
  array<string> names;
  array<string> selected;
  bool          multiple;
  int           over;

public:
  choice_widget_rep (command cb, array<string> n, array<string> a, bool mc);
  operator tree ();
  bool active (string s);
  int  find (SI y);
  void handle_get_size (get_size_event ev);
  void handle_repaint (repaint_event ev);
  void handle_mouse (mouse_event ev);
};

/******************************************************************************
* Implementation of choice widgets
******************************************************************************/

choice_widget_rep::choice_widget_rep
  (command cb2, array<string> n, array<string> a, bool mc):
    basic_widget_rep (),
    cb (cb2), names (n), selected (a), multiple (mc), over (-1) {}

choice_widget_rep::operator tree () {
  return "choice";
}

bool
choice_widget_rep::active (string s) {
  for (int i=0; i<N(selected); i++)
    if (selected[i] == s) return true;
  return false;
}

int
choice_widget_rep::find (SI ypos) {
  int i;
  SI y= 0, search= ypos*3;
  metric ex;
  font fn= get_default_font ();
  for (i=0; i<N(names); i++) {
    fn->var_get_extents (names[i], ex);
    if ((search >= (y+ fn->y1- fn->y2- 12*PIXEL)) && (search < y)) break;
    y += fn->y1- fn->y2- 12*PIXEL;
  }
  if (i == N(names)) return -1;
  return i;
}

void
choice_widget_rep::handle_get_size (get_size_event ev) {
  int i;
  metric ex;
  font fn= get_default_font ();
  ev->w= ev->h= 0;
  for (i=0; i<N(names); i++) {
    fn->var_get_extents (names[i], ex);
    ev->w  = max (ev->w, ((ex->x2- ex->x1+ 2)/3) + (6*PIXEL));
    ev->h += ((fn->y2- fn->y1+ 2)/3) + (4*PIXEL);
  }
  if (ev->mode == 1)
    gui_maximal_extents (ev->w, ev->h);
  abs_round (ev->w, ev->h);
}

void
choice_widget_rep::handle_repaint (repaint_event ev) { (void) ev;
  renderer ren= win->get_renderer ();
  int i; 
  metric ex;
  ren->set_background (white);
  ren->clear (0, -h, w, 0);
  font fn= get_default_font ();
  SI dy= fn->y1- fn->y2- 12*PIXEL;
  ren->set_shrinking_factor (3);
  SI y= 0;
  for (i=0; i<N(names); i++) {
    if (active (names[i])) {
      color blued= rgb_color (224, 224, 248);
      ren->set_background (blued);
      ren->clear (0, y + dy, 3*w, y);
      ren->set_background (white);
    }
    if (i == over) layout_dark_outline (ren, 0, y + dy, 3*w, y);
    ren->set_color (black);
    fn->var_get_extents (names[i], ex);
    fn ->draw (ren, names[i], 9*PIXEL, y-fn->y2-6*PIXEL);
    y += dy;
  }
  ren->set_shrinking_factor (1);
}

void
choice_widget_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;

  if ((type == "release-left") || (type == "release-right")) {
    int i= find (ev->y);
    if (i < 0) return;
    if (!multiple) selected= array<string> ();
    if (active (names[i])) {
      for (int j=0; j<N(selected); j++)
        if (selected[j] == names[i]) {
          selected= append (range (selected, 0, j),
                            range (selected, j+1, N(selected)));
          break;
        }
    }
    else selected << names[i];
    if (!multiple) cb (list_object (object (names[i])));
    else {
      object l= null_object ();
      for (int i=N(selected)-1; i>=0; i--)
        l= cons (selected[i], l);
      cb (list_object (l));
    }
    this << emit_invalidate_all ();
  }

  int new_over= over;
  if (type == "leave") new_over= -1;
  if (type == "enter" || type == "move") new_over= find (ev->y);
  if (new_over != over) {
    over= new_over;
    this << emit_invalidate_all ();
  }
}

/******************************************************************************
* Public interface
******************************************************************************/

wk_widget
choice_wk_widget (command cb, array<string> vals, string cur) {
  array<string> ch (1);
  ch[0]= cur;
  return tm_new<choice_widget_rep> (cb, vals, ch, false);
}

wk_widget
choice_wk_widget (command cb, array<string> vals, array<string> mc) {
  return tm_new<choice_widget_rep> (cb, vals, mc, true);
}
