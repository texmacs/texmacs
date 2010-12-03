
/******************************************************************************
* MODULE     : x_gui.hpp
* DESCRIPTION: Graphical user interface for X11
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef X_GUI_H
#define X_GUI_H
#include "timer.hpp"
#include "gui.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"

class x_gui_rep;
class x_drawable_rep;
class x_window_rep;
typedef x_gui_rep* x_gui;
typedef x_window_rep* x_window;
extern x_gui the_gui;

#define XK_CYRILLIC

#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/Sunkeysym.h>

extern bool true_color;
#define CONVERT(c) (true_color? c: gui->cmap[c])

/******************************************************************************
* For anti aliasing of TeX fonts
******************************************************************************/

struct x_character_rep: concrete_struct {
  int          c;
  font_glyphs  fng;
  int          sf;
  color        fg;
  color        bg;
  x_character_rep (int c, font_glyphs fng, int sf, color fg, color bg);
  friend class x_character;
};

class x_character {
  CONCRETE(x_character);
  x_character (int c=0, font_glyphs fng= font_glyphs (),
	       int sf=1, color fg= 0, color bg= 1);
  operator tree ();
};
CONCRETE_CODE(x_character);

bool operator == (x_character xc1, x_character xc2);
bool operator != (x_character xc1, x_character xc2);
int hash (x_character xc);

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
* The x_gui class
******************************************************************************/

class x_gui_rep {
public:
  Display*        dpy;
  GC              gc;
  GC              pixmap_gc;
  int             scr;
  Window          root;
  int             screen_width;
  int             screen_height;
  int             depth;
  Colormap        cols;
  color*          cmap;
  bool            im_ok;
  XIM             im;
  list<widget>    grab_ptr;
  list<widget>    grab_kbd;
  unsigned int    state;
  list<message>   messages;
  x_window_rep*   gswindow;
  int             argc;
  char**          argv;
  widget          balloon_wid;
  window          balloon_win;
  SI              balloon_x;
  SI              balloon_y;
  time_t          balloon_time;
  bool            interrupted;
  time_t          interrupt_time;

  hashmap<x_character,pointer> color_scale;       // for anti-aliasing
  hashmap<x_character,pointer> character_bitmap;  // bitmaps of all characters
  hashmap<x_character,pointer> character_pixmap;  // pixmaps of all characters
  hashmap<string,int>          xpm_bitmap;        // bitmaps of all xpms
  hashmap<string,int>          xpm_pixmap;        // pixmaps of all xpms
  hashmap<int,string>          lower_key;
  hashmap<int,string>          upper_key;

  list<Window>                 windows_l;
  hashmap<string,tree>         selection_t;
  hashmap<string,string>       selection_s;
  Window                       selection_w;

  Atom XA_CLIPBOARD;
  Atom XA_TARGETS;

public:
  x_gui_rep (int& argc, char** argv);
  ~x_gui_rep ();

  /******************************* Colors ************************************/
  void prepare_color (int sfactor, color fg, color bg);

  /****************************** Keyboard ***********************************/
  void initialize_input_method ();
  void insert_keysym (array<int>& a, int i, int j);
  void get_xmodmap ();
  void map (int key, string s);
  void Map (int key, string s);
  void initialize_keyboard_pointer ();
  string look_up_key (XKeyEvent* ev);
  string look_up_mouse (XButtonEvent* ev);
  unsigned int get_button_mask (XButtonEvent* ev);

  /******************************** Fonts ************************************/
  void set_shrinking_factor (int sfactor);
  void set_default_font (string name);
  font default_font_sub (bool tt, bool mini);
  font default_font (bool tt= false, bool mini= false);
  void get_ps_char (Font fn, int c, metric& ex, glyph& gl);
  void load_system_font (string family, int size, int dpi,
			 font_metric& fnm, font_glyphs& fng);

  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
  void   set_button_state (unsigned int state);
  void   emulate_leave_enter (widget old_widget, widget new_widget);
  void   obtain_mouse_grab (widget wid);
  void   release_mouse_grab ();
  bool   has_mouse_grab (widget w);

  /*********************** interclient communication *************************/
  void   created_window (Window win);
  void   deleted_window (Window win);
  void   focussed_window (Window win);
  bool   get_selection (string key, tree& t, string& s);
  bool   set_selection (string key, tree t, string s);
  void   clear_selection (string key);

  /**************************** miscellaneous ********************************/
  void   show_help_balloon (widget wid, SI x, SI y);
  void   map_balloon ();
  void   unmap_balloon ();
  void   image_auto_gc ();
  void   image_gc (string name);
  void   set_mouse_pointer (widget w, string name);
  void   set_mouse_pointer (widget w, string curs_name, string mask_name);
  void   show_wait_indicator (widget w, string message, string arg);
  void   external_event (string s, time_t t);
  bool   check_event (int type);

  /************************** Event processing *******************************/
  void process_event (x_window win, XEvent* ev);
  void event_loop ();

  /*************************** And our friends *******************************/
  friend class x_drawable_rep;
  friend class x_window_rep;
  friend class x_ps_font_rep;
  friend class x_tex_font_rep;
};

#endif // defined X_GUI_H
