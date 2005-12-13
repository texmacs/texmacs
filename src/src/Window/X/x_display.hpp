
/******************************************************************************
* MODULE     : x_display.hpp
* DESCRIPTION: Abstract display class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef X_DISPLAY_H
#define X_DISPLAY_H
#include "timer.hpp"
#include "display.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"

class x_display_rep;
class x_drawable_rep;
class x_window_rep;
typedef x_display_rep* x_display;
typedef x_window_rep* x_window;

#define XK_CYRILLIC

#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/Sunkeysym.h>

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

ostream& operator << (ostream& out, message m);

/******************************************************************************
* The x_display class
******************************************************************************/

class x_display_rep: public display_rep {
public:
  Display*        dpy;
  GC              gc;
  GC              pixmap_gc;
  int             scr;
  Window          root;
  int             display_width;
  int             display_height;
  int             depth;
  Colormap        cols;
  color*          cmap;
  bool            im_ok;
  XIM             im;
  //bool            im_spot;
  //int             im_sz;
  //XFontSet        im_fs;
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

  char*                        selection;
  hashmap<string,tree>         selections;

public:
  x_display_rep (int argc, char** argv);
  ~x_display_rep ();

  /****************************** Color **************************************/
  int   alloc_color (int r, int g, int b);
  void  init_color_map ();
  void  initialize_colors ();
  void  prepare_color (int sfactor, color fg, color bg);
  color get_color (string s);
  color rgb (int r, int g, int b);
  void  get_rgb (color col, int& r, int& g, int& b);

  /****************************** Keyboard ***********************************/
  void initialize_input_method ();
  void insert_keysym (array<int>& a, int i, int j);
  void get_xmodmap ();
  void map (int key, string s);
  void Map (int key, string s);
  void initialize_keyboard_pointer ();
  string look_up_key (XKeyEvent* ev);
  string look_up_mouse (XButtonEvent* ev);
  unsigned int get_kbd_modifiers ();
  unsigned int get_button_mask (XButtonEvent* ev);

  /******************************** Fonts ************************************/
  void set_shrinking_factor (int sfactor);
  void set_default_font (string name);
  font default_font_sub (bool tt);
  font default_font (bool tt= false);
  void get_ps_char (Font fn, int c, metric& ex, glyph& gl);
  void load_system_font (string family, int size, int dpi,
			 font_metric& fnm, font_glyphs& fng);

  /************************** Server languages *******************************/
  void   load_dictionary (string name, string from, string to);
  void   set_output_language (string lan);
  string get_output_language ();
  string translate (string s, string from, string to);

  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
  void   set_button_state (unsigned int state);
  void   emulate_leave_enter (widget old_widget, widget new_widget);
  void   grab_pointer (widget wid);
  void   ungrab_pointer ();
  bool   has_grab_pointer (widget w);
  void   grab_keyboard (widget wid);
  void   ungrab_keyboard ();

  /*********************** interclient communication *************************/
  tree   get_selection (widget wid, string key);
  bool   set_selection (widget wid, string key, tree t, string s);
  void   clear_selection (string key);
  void   delayed_message (widget wid, string s, time_t delay);
  int    remove_all_delayed_messages (widget wid, string s);

  /**************************** miscellaneous ********************************/
  void   set_help_balloon (widget wid, SI x, SI y);
  void   map_balloon ();
  void   unmap_balloon ();
  void   image_auto_gc ();
  void   image_gc (string name);
  void   set_pointer (string name);
  void   set_pointer (string curs_name, string mask_name);
  void   set_wait_indicator (string message, string arg);
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

#endif // defined X_DISPLAY_H
