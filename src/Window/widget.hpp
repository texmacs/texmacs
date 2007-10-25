
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
class widget_connection;
template<class T> class promise;

enum gravity { north_west, north,  north_east,
	       west,       center, east,
	       south_west, south,  south_east };

/******************************************************************************
* C++ stupidity does not allow forward declaration of enums.
* Slots should really be defined in message.hpp
******************************************************************************/

typedef enum slot {
  SLOT_WINDOW_ID,
  SLOT_NAME,
  SLOT_MINIMAL_SIZE,
  SLOT_DEFAULT_SIZE,
  SLOT_MAXIMAL_SIZE,
  SLOT_SIZE,
  SLOT_POSITION,
  SLOT_GRAVITY,
  SLOT_GEOMETRY, // FIXME: this is a bit redundant
  SLOT_KEYBOARD,
  SLOT_KEYBOARD_FOCUS,
  SLOT_MOUSE,
  SLOT_REPAINT,
  SLOT_INVALIDATE_ALL,
  SLOT_INVALIDATE,
  SLOT_INVALID,
  SLOT_DESTROY,

  SLOT_SHRINKING_FACTOR,
  SLOT_EXTENTS,
  SLOT_VISIBLE_PART,
  SLOT_SCROLLBARS_VISIBILITY,
  SLOT_SCROLL_POSITION,

  SLOT_HEADER_VISIBILITY,
  SLOT_MAIN_MENU,
  SLOT_MAIN_ICONS_VISIBILITY,
  SLOT_MAIN_ICONS,
  SLOT_CONTEXT_ICONS_VISIBILITY,
  SLOT_CONTEXT_ICONS,
  SLOT_USER_ICONS_VISIBILITY,
  SLOT_USER_ICONS,
  SLOT_FOOTER_VISIBILITY,
  SLOT_LEFT_FOOTER,
  SLOT_RIGHT_FOOTER,
  SLOT_INTERACTIVE_MODE,
  SLOT_INTERACTIVE_PROMPT,
  SLOT_INTERACTIVE_INPUT,

  SLOT_FORM_FIELD,
  SLOT_STRING_INPUT,
  SLOT_INPUT_TYPE,
  SLOT_INPUT_PROPOSAL,
  SLOT_FILE,
  SLOT_DIRECTORY
};

//extern bool* slot_state_table;
//inline bool is_state_slot (slot s) { return slots_state_table[s]; }

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
widget input_text_widget (command cb, string type, array<string> def);
widget inputs_list_widget (command call_back, array<string> prompts);
widget file_chooser_widget (command cmd, string type="texmacs", string mgn="");
widget balloon_widget (widget w, widget help);
widget wait_widget (SI w, SI h, string message);
widget texmacs_widget (int mask, command quit);

#endif // defined WIDGET_H
