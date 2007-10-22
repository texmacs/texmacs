
/******************************************************************************
* MODULE     : widget.hpp
* DESCRIPTION: Definition of abstract widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef WIDGET_H
#define WIDGET_H
#include "tree.hpp"
#include "command.hpp"
#include "timer.hpp"

class window_rep;
typedef window_rep* window;
class display_rep;
typedef display_rep* display;
display current_display ();
typedef int color;
class url;
template<class T> class promise;

class widkit_widget;
typedef widkit_widget widget;
enum gravity { north_west, north,  north_east,
	       west,       center, east,
	       south_west, south,  south_east };

/******************************************************************************
* The abstract widget class
******************************************************************************/

/*
class widget {
public:
  ABSTRACT_NULL(widget);
  inline widget operator [] (int i);
         widget operator [] (string s);
  inline operator tree ();
  inline bool operator == (widget w);
  inline bool operator != (widget w);
};

class widget_rep: public abstract_struct {
public:
  window        win;              // underlying window
  SI            ox, oy;           // origin of widget in window
  SI            w, h;             // width and height of widget
  gravity       grav;             // position of the origin in the widget
  array<widget> a;                // children of widget
  array<string> name;             // names for the children

  widget_rep (array<widget> a, array<string> name, gravity grav);
  virtual ~widget_rep ();

  virtual operator tree () = 0;
  virtual bool handle (event ev) = 0;

  SI       x1 (); SI y1 (); // lower left window coordinates of widget
  SI       x2 (); SI y2 (); // upper right window coordinates of widget
  bool     attached ();
  void     fatal_error (string message, string in="", string fname="");

  friend   class widget;
};

ABSTRACT_NULL_CODE(widget);
inline widget widget::operator [] (int i) { return rep->a[i]; }
inline widget::operator tree () { return (tree) (*rep); }
inline bool widget::operator == (widget w) { return rep == w.rep; }
inline bool widget::operator != (widget w) { return rep != w.rep; }

ostream& operator << (ostream& out, widget w);
widget operator << (widget w, event ev);
*/

/******************************************************************************
* Exported special widgets
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
widget inputs_list_widget (command call_back, array<string> prompts);
widget resize_widget (widget wdgt, SI w, SI h, bool xr= false, bool yr= false);
widget file_chooser_widget (command cmd, string type="texmacs", string mgn="");
widget balloon_widget (widget w, widget help);
widget wait_widget (SI w, SI h, string message);

#endif // defined WIDGET_H
