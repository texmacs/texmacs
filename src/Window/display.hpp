
/******************************************************************************
* MODULE     : display.hpp
* DESCRIPTION: Abstract display class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DISPLAY_H
#define DISPLAY_H
#include "tree.hpp"
#include "bitmap_font.hpp"
#include "timer.hpp"

struct font;
class widget;
class window_rep;
typedef window_rep* window;
class display_rep;
typedef display_rep* display;
typedef int color;

#define INTERRUPT_EVENT   0
#define INTERRUPTED_EVENT 1
#define ANY_EVENT         2
#define DRAG_EVENT        3
#define MOTION_EVENT      4
#define MENU_EVENT        5

class display_rep {
public:
  display_rep ();
  virtual ~display_rep ();

  /* important routines */
  virtual void get_extents (SI& width, SI& height) = 0;
  virtual void get_max_size (SI& width, SI& height) = 0;
  virtual void event_loop () = 0;

  /* color */
  color black, white, red, green, blue;
  color yellow, magenta, orange, brown, pink;
  color light_grey, grey, dark_grey;
  virtual color  get_color (string s) = 0;
  virtual color  rgb (int r, int g, int b) = 0;
  virtual void   get_rgb (color col, int& r, int& g, int& b) = 0;
  virtual string get_name (color c) = 0;

  /* language support */
  string  out_lan;
  virtual void load_dictionary (string name, string from, string to) = 0;
  virtual void set_output_language (string lan) = 0;
  virtual string get_output_language () = 0;
  virtual string translate (string lan, string from, string to) = 0;

  /* fonts */
  virtual void set_default_font (string name) = 0;
  virtual font default_font (bool tt= false) = 0;
  virtual void load_system_font (string family, int size, int dpi,
				 font_metric& fnm, font_glyphs& fng) = 0;

  /* pointer and keyboard */
  virtual void grab_pointer (widget wid) = 0;
  virtual void ungrab_pointer () = 0;
  virtual bool has_grab_pointer (widget w) = 0;
  virtual void request_keyboard_focus (widget wid) = 0;
  virtual unsigned int get_kbd_modifiers () = 0;

  /* interclient communication */
  virtual tree get_selection (widget wid, string k) = 0;
  virtual bool set_selection (widget wid, string k, tree t, string s="") = 0;
  virtual void clear_selection (string key) = 0;
  virtual void delayed_message (widget wid, string mess, time_t delay) = 0;
  virtual int  remove_all_delayed_messages (widget wid, string s) = 0;

  /* miscellaneous */
  virtual void set_help_balloon (widget wid, SI x, SI y) = 0;
  virtual void image_gc (string name= "*") = 0;
  virtual void set_pointer (string name) = 0;
  virtual void set_pointer (string curs_name, string mask_name) = 0;
  virtual void set_wait_indicator (string message, string arg) = 0;
  virtual bool check_event (int type) = 0;
};

extern display the_display;
void open_display (int argc=0, char** argv=NULL);
void close_display ();
void set_interpose_handler (void (*) (void));
void beep ();

#endif // defined DISPLAY_H
