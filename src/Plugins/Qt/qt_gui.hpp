
/******************************************************************************
* MODULE     : qt_gui.hpp
* DESCRIPTION: QT GUI class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef QT_GUI_HPP
#define QT_GUI_HPP

#include <QPixmap>
#include <QApplication>
#include <QTimer>



#include "timer.hpp"
#include "gui.hpp"
#include "font.hpp"
#include "widget.hpp" 
#include "array.hpp"
#include "hashmap.hpp"

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
* QT images
******************************************************************************/

struct qt_image_rep: concrete_struct {
  QPixmap *img;
  SI xo,yo;
  int w,h;
  qt_image_rep (QPixmap *img2, SI xo2, SI yo2, int w2, int h2);
  ~qt_image_rep();
  friend class qt_image;
  friend class qt_drawable;
};

class qt_image {
  CONCRETE_NULL(qt_image);
  qt_image (QPixmap *img2, SI xo2, SI yo2, int w2, int h2);
 // qt_image ();
};

CONCRETE_NULL_CODE(qt_image);

/******************************************************************************
* The qt_gui class
******************************************************************************/

typedef class qt_gui_rep* qt_gui;

class qt_drawable_rep;


extern qt_gui the_gui;

class qt_gui_rep {
public:
  QColor*          cmap;
 
//  unsigned int    state;

  bool interrupted;
  time_t          interrupt_time;
  
  hashmap<x_character,pointer> color_scale;       // for anti-aliasing
  hashmap<x_character,qt_image> character_image;  // bitmaps of all characters

    char*                        selection;
  hashmap<string,tree>         selection_t;
  hashmap<string,string>       selection_s;

   hashmap<string,qt_image> images; 

public:
  qt_gui_rep(int argc, char **argv);
  virtual ~qt_gui_rep();
  
  
  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
 // void   set_button_state (unsigned int state);

  /* important routines */
   void event_loop ();
  
  /* color */
  void init_color_map ();
  void initialize_colors ();

  
  /* fonts */
  virtual void set_default_font (string name);
  virtual font default_font (bool tt= false);
  virtual void load_system_font (string family, int size, int dpi,
                                 font_metric& fnm, font_glyphs& fng);
  
  
  /* interclient communication */
  virtual bool get_selection (string key, tree& t, string& s);
  virtual bool set_selection (string key, tree t, string s);
  virtual void clear_selection (string key);
  
  /* miscellaneous */
  void image_gc (string name= "*");
  void set_mouse_pointer (string name);
  void set_mouse_pointer (string curs_name, string mask_name);
  void show_wait_indicator (widget w, string message, string arg);
  bool check_event (int type);
  
  void update();
  void update_fast ();

  void  prepare_color (int sf, color fg, color bg);
//  void emulate_leave_enter (widget old_widget, widget new_widget);
//  void set_button_state (int s);
  font default_font_sub (bool tt);

  
  friend class qt_renderer_rep;
  friend class qt_window_rep;
};

#endif // defined QT_GUI_HPP
