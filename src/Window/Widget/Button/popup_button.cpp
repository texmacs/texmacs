
/******************************************************************************
* MODULE     : popup_button.cpp
* DESCRIPTION: Buttons which trigger a popup window.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "timer.hpp"
#include "window.hpp"
#include "Widget/make_widget.hpp"
#include "Widget/Button/button_widget.hpp"

#ifdef OS_WIN32
#define MAP_DELAY 250
#else
#define MAP_DELAY 100
#endif

/******************************************************************************
* Popup buttons
******************************************************************************/

class popup_button_rep: public button_widget_rep {
  make_widget mw;
  widget      popup_w;
  window      popup;
  gravity     where;
  time_t      entered_at;
  bool        require_map;
  bool        stick;

private:
  void map_popup ();
  void unmap_popup ();
  void consistent (string s);
public:
  popup_button_rep (widget w, widget pw, gravity where);
  popup_button_rep (widget w, make_widget mw, gravity where);
  void handle_attach_window (attach_window_event ev);
  void handle_mouse (mouse_event ev);
  bool handle (event ev);
};

gravity opposite (gravity grav);

/******************************************************************************
* Routines for popup buttons
******************************************************************************/

popup_button_rep::popup_button_rep (widget w, widget pw, gravity where2):
  button_widget_rep (w, where2==east),
  mw (), popup_w (popup_widget (pw, opposite (where2))), popup (NULL),
  where (where2), require_map (false), stick (false)
{
  if ((where!=east) && (where!=south) && (where!=south_east))
    fatal_error ("direction not implemented",
		 "popup_button_rep::popup_button_rep");
}

popup_button_rep::popup_button_rep (widget w, make_widget mw2, gravity where2):
  button_widget_rep (w, where2==east),
  mw (mw2), popup_w (), popup (NULL),
  where (where2), require_map (false), stick (false)
{
  if ((where!=east) && (where!=south) && (where!=south_east))
    fatal_error ("direction not implemented",
		 "popup_button_rep::popup_button_rep");
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
    fatal_error ("Inconsistency in " * s, "popup_button_rep::consistent");
  }
}

void
popup_button_rep::map_popup () {
  require_map= false;
  stick      = false;

  if (!nil (mw)) {
    // int start_1= texmacs_time ();
    popup_w= popup_widget (mw (dis), opposite (where));
    // cout << "Mapping required " << (texmacs_time ()-start_1) << " ms\n";
  }

  // int start_2= texmacs_time ();
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

  // int start_3= texmacs_time ();
  popup= popup_window (popup_w, x, y);
  // cout << "Window creation required " << (texmacs_time ()-start_3) << " ms\n";
  // int start_4= texmacs_time ();
  popup->map ();
  // cout << "Mapping required " << (texmacs_time ()-start_4) << " ms\n";

  this << emit_invalidate_all ();
  this << emit_mouse_grab (true);
}

void
popup_button_rep::unmap_popup () {
  if (popup == NULL)
    fatal_error ("Unexpected situation", "popup_button_rep::unmap_popup");
  popup->unmap ();
  delete popup;
  popup= NULL;
  if (!nil (mw)) popup_w= widget ();

  this << emit_invalidate_all ();
  if (!dis->has_grab_pointer (this))
    fatal_error ("I do not have the pointer grab",
		 "popup_button_rep::unmap_popup");
  this << emit_mouse_grab (false);
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
  SI     x= ev->x, y= ev->y;

  consistent ("handle_mouse (start)");

  if (type == "leave") {
    if (require_map) {
      inside= false;
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
	  popup_w << set_integer ("grabbed", 1);
	  popup_w << emit_mouse_grab (true);
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
	  popup_w << set_integer ("grabbed", 1);
	  popup_w << set_integer ("stick", 1);
	  popup_w << emit_mouse_grab (true);
	}
      }
      else {
	if (require_map) require_map= false;
	else unmap_popup ();
      }
    }
  }

  consistent ("handle_mouse (*)");

  /**************************** wait to be mapped ****************************/
  if (require_map) {
    time_t now;
    do {
      now= texmacs_time ();
      if (win->check_event (MENU_EVENT)) return;
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
    if (!nil (popup_w)) popup_w << ev;
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
  fatal_error ("unknown gravity", "operator -", "popup_button.cpp");
  return center; // Because of bug in certain versions of g++
}

/******************************************************************************
* Interface
******************************************************************************/

widget
pulldown_button (widget w, widget pw) {
  return new popup_button_rep (w, pw, south_east);
}

widget
pullright_button (widget w, widget pw) {
  return new popup_button_rep (w, pw, east);
}

widget
pulldown_button (widget w, make_widget mw) {
  return new popup_button_rep (w, mw, south_east);
}

widget
pullright_button (widget w, make_widget mw) {
  return new popup_button_rep (w, mw, east);
}
