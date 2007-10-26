
/******************************************************************************
* MODULE     : widget.hpp
* DESCRIPTION: Definition of abstract widgets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef WIDGET_H
#define WIDGET_H
#include "list.hpp"
#include "tree.hpp"
#include "blackbox.hpp"
#include "command.hpp"
#include "timer.hpp"

class window_rep;
typedef window_rep* window;
class display_rep;
typedef display_rep* display;
typedef int color;
class url;
class widget;
class slot;
class widget_connection;
template<class T> class promise;

enum gravity { north_west, north,  north_east,
	       west,       center, east,
	       south_west, south,  south_east };

/******************************************************************************
* The abstract widget class
******************************************************************************/

class widget_rep: public abstract_struct {
protected:
  list<widget_connection> in;
  list<widget_connection> out;

public:
  widget_rep ();
  virtual inline ~widget_rep ();
  virtual ostream& print (ostream& out);

  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, int type_id);
  virtual void connect (slot s, widget w2, slot s2);
  virtual void deconnect (slot s, widget w2, slot s2);

  friend class widget;
};

class widget {
public:
ABSTRACT_NULL(widget);
  inline bool operator == (widget w) { return rep == w.rep; }
  inline bool operator != (widget w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(widget);

inline ostream&
operator << (ostream& out, widget w) {
  if (nil (w)) return out << "nil";
  else return w->print (out);
}

/******************************************************************************
* Exported special widgets and window widget destruction
******************************************************************************/

widget horizontal_list (array<widget> a);
widget horizontal_list (array<widget> a, array<string> name);
widget vertical_list (array<widget> a);
widget vertical_list (array<widget> a, array<string> name);
widget vertical_menu (array<widget> a);
widget tile (array<widget> a, int cols);
widget tile (array<widget> a, int cols, array<string> name);
widget horizontal_array (array<widget> a, int stretch_me= -1);
widget horizontal_array (array<widget> a, array<string> s, int stretch_me= -1);
widget switch_widget (array<widget> a, array<string> name, int init= 0);
widget optional_widget (widget w, bool on= true);
widget glue_widget (bool hx=true, bool vx=true, SI w=0, SI h=0);
widget separator_widget (SI pre=0, SI post=0, bool vert=false);
widget text_widget (string s, bool tsp= false, string lan="");
widget menu_text_widget (string s, color col, string lan="", bool tt= false);
widget xpm_widget (url file_name, bool transp= true);
widget command_button (widget w, command cmd, bool button_flag= false);
widget command_button (widget lw, widget rw, command cmd);
widget command_button (widget lw, widget cw, widget rw, command cmd,
		       bool e=true, bool c=false);
widget pulldown_button (widget w, widget m, bool button_flag= false);
widget pullright_button (widget w, widget m, bool button_flag= false);
widget pulldown_button (widget w, promise<widget> pw);
widget pullright_button (widget w, promise<widget> pw);
widget popup_widget (widget w, gravity quit=center);
widget canvas_widget (widget w, gravity grav=north_west);
widget input_text_widget (command call_back);
widget input_text_widget (command cb, string type, array<string> def);
widget inputs_list_widget (command call_back, array<string> prompts);
widget file_chooser_widget (command cmd, string type="texmacs", string mgn="");
widget balloon_widget (widget w, widget help);
widget wait_widget (SI w, SI h, string message);
widget texmacs_widget (int mask, command quit);
widget plain_window_widget (widget W, char* s, SI w=0, SI h=0, SI x=0, SI y=0);
widget popup_window_widget (widget W, SI x, SI y);
void   destroy_window_widget (widget w);

#endif // defined WIDGET_H
