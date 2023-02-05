
/******************************************************************************
* MODULE     : sdl_gui.hpp
* DESCRIPTION: Graphical user interface for SDL
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SDL_GUI_H
#define SDL_GUI_H
#include "tm_timer.hpp"
#include "gui.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"
#include "colors.hpp"

#include <SDL2/SDL.h>

class sdl_gui_rep;
class sdl_drawable_rep;
class sdl_window_rep;
typedef sdl_gui_rep* sdl_gui;
typedef sdl_window_rep* sdl_window;
extern sdl_gui the_gui;


/******************************************************************************
* Delayed messages
******************************************************************************/

struct message_rep: concrete_struct {
  widget wid;
  string s;
  time_t t;
  message_rep (widget wid, string s, time_t t);
  friend class message;
};

class message {
  CONCRETE(message);
  message (widget wid, string s, time_t t);
};
CONCRETE_CODE(message);

tm_ostream& operator << (tm_ostream& out, message m);

/******************************************************************************
* The sdl_gui class
******************************************************************************/

class sdl_gui_rep {
public:

  int screen_width, screen_height;
  
  unsigned int    mouse_state;
  list<widget>    grab_ptr;
  list<widget>    grab_kbd;
//  unsigned int    state;
  list<message>   messages;
  sdl_window_rep* gswindow;
  widget          balloon_wid;
  window          balloon_win;
  SI              balloon_x;
  SI              balloon_y;
  time_t          balloon_time;
  bool            interrupted;
  time_t          interrupt_time;

  list<SDL_Window*>            windows_l;
  hashmap<string,tree>         selection_t;
  hashmap<string,string>       selection_s;
  SDL_Window*                  selection_w;

  // support for runloop
  bool wait;
  int count, delay;
  
public:
  sdl_gui_rep (int& argc, char** argv);
  ~sdl_gui_rep ();
  
  void update_mouse_state (Uint32 mask = 0); // update mouse_state

  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
  void   set_button_state (unsigned int state);
  void   emulate_leave_enter (widget old_widget, widget new_widget);
  void   obtain_mouse_grab (widget wid);
  void   release_mouse_grab ();
  bool   has_mouse_grab (widget w);

  /*********************** interclient communication *************************/

  bool   get_selection (string key, tree& t, string& s);
  bool   set_selection (string key, tree t, string s);
  void   clear_selection (string key);

  /**************************** miscellaneous ********************************/
  void   show_help_balloon (widget wid, SI x, SI y);
  void   map_balloon ();
  void   unmap_balloon ();
  void   set_mouse_pointer (widget w, string name);
  void   set_mouse_pointer (widget w, string curs_name, string mask_name);
  void   show_wait_indicator (widget w, string message, string arg);
  void   external_event (string s, time_t t);
  bool   check_event (int type);
  void set_default_font (string name);
  font default_font_sub (bool tt, bool mini, bool bold);
  font default_font (bool tt, bool mini, bool bold);

  /************************** Event processing *******************************/
  void process_event (SDL_Event* event);
  void event_loop ();
  bool run_gui ();
  
  /************************** window interface *******************************/
  void create_window (int id, string name, int x, int y, int w, int h, bool popup);
  void destroy_window (int id);

  void   created_window (int id);
  void   deleted_window (int id);
  void   focussed_window (int id);

  void get_window_position (int id, int& x, int& y);
  void set_window_position (int id, int x, int y);
  void get_window_size (int id, int& w, int& h);
  void set_window_size (int id, int w, int h);
  void set_window_limits (int id, int min_w, int min_h, int max_w, int max_h);
  void set_window_title (int id, string name);
  void set_window_visibility (int id, bool show);
  void set_window_fullscreen (int id, bool full);
  void sync_window (int id, picture backing_store);

};

#endif // defined SDL_GUI_H
