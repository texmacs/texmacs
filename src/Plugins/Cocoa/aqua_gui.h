
/******************************************************************************
* MODULE     : aqua_gui.hpp
* DESCRIPTION: Aqua GUI class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef AQUA_GUI_H
#define AQUA_GUI_H
#include "mac_cocoa.h"
#include "timer.hpp"
#include "gui.hpp"
#include "font.hpp"
#include "widget.hpp" 
#include "array.hpp"
#include "hashmap.hpp"

/******************************************************************************
* The aqua_gui class
******************************************************************************/

typedef class aqua_gui_rep* aqua_gui;
extern aqua_gui the_gui;

class aqua_gui_rep {
public:
  bool interrupted;
  time_t interrupt_time;
  
    char*                        selection;
  hashmap<string,tree>         selection_t;
  hashmap<string,string>       selection_s;

public:
  aqua_gui_rep(int argc, char **argv);
  virtual ~aqua_gui_rep();
  
  
  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
 // void   set_button_state (unsigned int state);

  /* important routines */
   void event_loop ();
  
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

};

#endif // defined AQUA_GUI_H
