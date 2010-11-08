
/******************************************************************************
* MODULE     : popup_button.cpp
* DESCRIPTION: Buttons which trigger a popup window.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "timer.hpp"
#include "window.hpp"
#include "promise.hpp"
#include "Widkit/Button/button_widget.hpp"
#include "message.hpp"

#ifdef OS_WIN32
#define MAP_DELAY 250
#else
#define MAP_DELAY 100
#endif

/******************************************************************************
* Popup buttons
******************************************************************************/

class popup_button_rep: public button_widget_rep {
  promise<wk_widget> prom;
  wk_widget          popup_w;
  window  popup;
  gravity where;
  time_t  entered_at;
  bool    require_map;
  bool    stick;

private:
  void map_popup ();
  void unmap_popup ();
  void consistent (string s);
public:
  popup_button_rep (wk_widget w, wk_widget pw,
		    gravity where, bool button_flag);
  popup_button_rep (wk_widget w, promise<wk_widget> prom,
		    gravity where);
  void handle_attach_window (attach_window_event ev);
  void handle_mouse (mouse_event ev);
  bool handle (event ev);
};

gravity opposite (gravity grav);

/******************************************************************************
* Routines for popup buttons
******************************************************************************/

popup_button_rep::popup_button_rep (
  wk_widget w, wk_widget pw, gravity wh, bool fl):
    button_widget_rep (w, wh==east, 0, fl),
    prom (), popup_w (popup_widget (pw, opposite (wh))), popup (NULL),
    where (wh), require_map (false), stick (false)
{
  this->has_pull_down= (where == south || where == south_east);
  if (where != east && where != south && where != south_east)
    WK_FAILED ("direction not implemented");
}

popup_button_rep::popup_button_rep (
  wk_widget w, promise<wk_widget> prom2, gravity where2):
    button_widget_rep (w, where2==east),
    prom (prom2), popup_w (), popup (NULL),
    where (where2), require_map (false), stick (false)
{
  this->has_pull_down= (where == south || where == south_east);
  if ((where!=east) && (where!=south) && (where!=south_east))
    WK_FAILED ("direction not implemented");
}

void
popup_button_rep::consistent (string s) {
  // status= true
  //   iff (inside button and left or right button pressed) or stick
  //   iff require_map=true or mouse grab activated on button
  //   iff require_map=true or popup window is mapped

  bool flag;
  if (status) flag=
		(require_map && (popup != NULL)) ||
		((!require_map) && (popup == NULL));
  else flag= (!require_map) && (popup != NULL);
  if (flag) {
    cerr << "status     = " << status << "\n";
    cerr << "require map= " << require_map << "\n";
    cerr << "popup      = " << (popup != NULL) << "\n";
    WK_FAILED ("inconsistency in " * s);
  }
}

void
popup_button_rep::map_popup () {
  require_map= false;
  stick      = false;

  if (!is_nil (prom)) {
    // time_t start_1= texmacs_time ();
    popup_w= popup_widget (prom (), opposite (where));
    // cout << "Mapping required " << (texmacs_time ()-start_1) << " ms\n";
  }

  // time_t start_2= texmacs_time ();
  SI x, y, w=0, h=0;
  win->get_position (x, y);
  popup_w << get_size (w, h);

  switch (where) {
  case east:
    x += x2()-12*PIXEL;
    y += y2()+3*PIXEL;
    break;
  case south_east:
    x += x1()-3*PIXEL;
    y += y1();
    break;
  case south:
    x += ((x1()+x2())>>1)- (w>>1)- 3*PIXEL;
    y += y1();
    break;
  default:
    break;
  }
  // cout << "Positioning required " << (texmacs_time ()-start_2) << " ms\n";

  // time_t start_3= texmacs_time ();
  wk_widget win_wid= popup_window_widget (popup_w, "Popup");
  set_position (abstract (win_wid), x, y);
  popup= win_wid -> win;
  // popup= popup_window (abstract (popup_w), x, y);
  // cout << "Window creation required " << (texmacs_time ()-start_3) << " ms\n";
  // time_t start_4= texmacs_time ();
  popup->set_visibility (true);
  // cout << "Mapping required " << (texmacs_time ()-start_4) << " ms\n";

  this << emit_invalidate_all ();
  wk_grab_pointer (this);
}

void
popup_button_rep::unmap_popup () {
  if (popup == NULL) WK_FAILED ("unexpected situation");
  popup->set_visibility (false);
  tm_delete (popup);
  popup= NULL;
  if (!is_nil (prom)) popup_w= wk_widget ();

  this << emit_invalidate_all ();
  if (!wk_has_pointer_grab (this))
    WK_FAILED ("I do not have the pointer grab");
  wk_ungrab_pointer (this);
}

void
popup_button_rep::handle_attach_window (attach_window_event ev) {
  if ((ev->win==NULL) && status) {
    consistent ("handle_attach_window (1)");
    status= false;
    if (require_map) require_map= false;
    else unmap_popup ();
    stick= false;    
    consistent ("handle_attach_window (2)");
  }
  basic_widget_rep::handle_attach_window (ev);
}

void
popup_button_rep::handle_mouse (mouse_event ev) {
  string type= ev->type;
  SI x= ev->x, y= ev->y;

  consistent ("handle_mouse (start)");

  bool old_inside= inside;

  if (type == "leave") {
    inside= false;
    if (require_map) {
      status= false;
      require_map= false;
    }
  }

  /*************************** button is inactive ****************************/
  else if (!status) {
    consistent ("handle_mouse (1)");
    inside= (y>=0) && (y<h) && (x>=0) && (x<w);
    status= inside && (ev->pressed ("left") || ev->pressed ("right"));
    if (status) {
      entered_at= texmacs_time ();
      require_map= true;
    }
  }

  /**************************** button is active *****************************/
  else {
    bool inside_popup=
      ((where == east) && (x > w-12*PIXEL)) ||
      ((where == south) && (y<0)) ||
      ((where == south_east) && (y<0));
    inside=
      (y>=0) && (y<h) && (x>=0) && (x<w);
    status=
      (inside || ((!stick) && inside_popup)) &&
      (ev->pressed ("left") || ev->pressed ("right"));

    // activate
    if (status) {
      if (inside_popup) {
	if (require_map) map_popup ();
	if (status) {
	  consistent ("handle_mouse (2)");
	  popup_w << set_integer ("stick", 0);
	  wk_grab_pointer (popup_w);
	}
      }
    }

    // stick or disactivate
    else {
      if (inside /* && false */) {
	status= true;
	if (require_map) { map_popup (); stick= true; }
	if (status) {
	  consistent ("handle_mouse (3)");
	  popup_w << set_integer ("stick", 1);
	  wk_grab_pointer (popup_w);
	}
      }
      else {
	if (require_map) require_map= false;
	else unmap_popup ();
      }
    }
  }

  if (inside != old_inside)
    this << emit_invalidate_all ();

  consistent ("handle_mouse (*)");

  /**************************** wait to be mapped ****************************/
  if (require_map) {
    time_t now;
    do {
      now= texmacs_time ();
      if (check_event (MENU_EVENT)) return;
    } while ((now-entered_at) < MAP_DELAY);
    map_popup ();
  }

  consistent ("handle_mouse (end)");
}

bool
popup_button_rep::handle (event ev) {
  switch (ev->type) {
  case GET_WIDGET_EVENT:
  case SET_WIDGET_EVENT:
  case CLEAN_EVENT:
  case INSERT_EVENT:
  case REMOVE_EVENT:
    if (!is_nil (popup_w)) popup_w << ev;
    return true;
  case GET_COORD2_EVENT:
  case SET_COORD2_EVENT:
    return button_widget_rep::handle (ev);
  default:
    return basic_widget_rep::handle (ev);
  }
}

gravity
opposite (gravity grav) {
  switch (grav) {
  case north_west: return south_east;
  case north     : return south;
  case north_east: return south_west;
  case west      : return east;
  case center    : return center;
  case east      : return west;
  case south_west: return north_east;
  case south     : return north;
  case south_east: return north_west;
  }
  FAILED ("unknown gravity");
  return center; // Because of bug in certain versions of g++
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
pulldown_button (wk_widget w, wk_widget pw, bool button_flag) {
  return tm_new<popup_button_rep> (w, pw, south_east, button_flag);
}

wk_widget
pullright_button (wk_widget w, wk_widget pw, bool button_flag) {
  return tm_new<popup_button_rep> (w, pw, east, button_flag);
}

wk_widget
pulldown_button (wk_widget w, promise<wk_widget> prom) {
  return tm_new<popup_button_rep> (w, prom, south_east);
}

wk_widget
pullright_button (wk_widget w, promise<wk_widget> prom) {
  return tm_new<popup_button_rep> (w, prom, east);
}
