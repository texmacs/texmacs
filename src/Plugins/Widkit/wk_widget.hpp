
/******************************************************************************
* MODULE     : wk_widget.hpp
* DESCRIPTION: Definition of abstract native widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WK_WIDGET_H
#define WK_WIDGET_H
#include "widget.hpp"
#include "Widkit/event.hpp"

/******************************************************************************
* The abstract widkit implementation of widgets
******************************************************************************/

class wk_widget;
class wk_widget_rep: public widget_rep {
public:
  window   win;             // underlying window
  SI       ox, oy;          // origin of widget in window
  SI       w, h;            // width and height of widget
  gravity  grav;            // position of the origin in the widget
  array<wk_widget>  a;      // children of widget
  array<string>     name;   // names for the children

  wk_widget_rep (array<wk_widget> a, array<string> name, gravity grav);
  ~wk_widget_rep ();

  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  void notify (slot s, blackbox new_val);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);

  virtual operator tree () = 0;
  virtual bool handle (event ev) = 0;
  virtual bool is_window_widget ();

  SI    x1 (); SI y1 (); // lower left window coordinates of widget
  SI    x2 (); SI y2 (); // upper right window coordinates of widget
  bool  attached ();
  void  wk_error (string message);

  friend class wk_widget;
};

class wk_widget {
public:
ABSTRACT_NULL(wk_widget);
  inline wk_widget operator [] (int i) { return rep->a[i]; }
  wk_widget operator [] (string s);
  inline operator tree () { return (tree) (*rep); }
  inline bool operator == (wk_widget w) { return rep == w.rep; }
  inline bool operator != (wk_widget w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(wk_widget);

inline widget abstract (wk_widget w) {
  return widget (w.rep); }
inline wk_widget concrete (widget w) {
  return wk_widget ((wk_widget_rep*) w.rep); }
array<widget> abstract (array<wk_widget> a);
array<wk_widget> concrete (array<widget> a);

tm_ostream& operator << (tm_ostream& out, wk_widget w);
wk_widget operator << (wk_widget w, event ev);

void wk_grab_pointer (wk_widget w);
void wk_ungrab_pointer (wk_widget w);
bool wk_has_pointer_grab (wk_widget w);

#define WK_FAILED(msg) { wk_error (msg); FAILED ("widget_error"); }

/******************************************************************************
* Exported special widgets and window widget destruction
******************************************************************************/

wk_widget horizontal_list (array<wk_widget> a);
wk_widget horizontal_list (array<wk_widget> a, array<string> name);
wk_widget vertical_list (array<wk_widget> a);
wk_widget vertical_list (array<wk_widget> a, array<string> name);
wk_widget vertical_menu (array<wk_widget> a);
wk_widget tile (array<wk_widget> a, int cols);
wk_widget tile (array<wk_widget> a, int cols, array<string> name);
wk_widget horizontal_array (array<wk_widget> a, int stretch_me= -1);
wk_widget horizontal_array (array<wk_widget> a, array<string> s,
			    int stretch_me= -1);
wk_widget switch_widget (array<wk_widget> a, array<string> name, int init= 0);
wk_widget optional_widget (wk_widget w, bool on= true);
wk_widget glue_wk_widget (bool hx=true, bool vx=true, SI w=0, SI h=0);
wk_widget separator_wk_widget (SI pre=0, SI post=0, bool vert=false);
wk_widget text_wk_widget (string s, bool tsp= false, string lan= "");
wk_widget menu_text_wk_widget (string s, color col, bool tsp=true,
			       string lan="", bool tt= false);
wk_widget xpm_wk_widget (url file_name, bool transp= true);
wk_widget command_button (wk_widget w, command cmd, bool button_flag= false);
wk_widget command_button (wk_widget lw, wk_widget rw, command cmd);
wk_widget command_button (wk_widget lw, wk_widget cw, wk_widget rw,
			  command cmd, bool e=true, bool c=false);
wk_widget pulldown_button (wk_widget w, wk_widget m, bool button_flag= false);
wk_widget pullright_button (wk_widget w, wk_widget m, bool button_flag= false);
wk_widget pulldown_button (wk_widget w, promise<wk_widget> pw);
wk_widget pullright_button (wk_widget w, promise<wk_widget> pw);
wk_widget popup_widget (wk_widget w, gravity quit=center);
wk_widget canvas_widget (wk_widget w, gravity grav=north_west);
wk_widget input_text_wk_widget (command call_back);
wk_widget input_text_wk_widget (command cb, string type, array<string> def);
wk_widget inputs_list_wk_widget (command call_back, array<string> prompts);
wk_widget file_chooser_wk_widget (command cmd, string type="texmacs",
				  string mgn="");
wk_widget balloon_widget (wk_widget w, wk_widget help);
wk_widget wait_wk_widget (SI w, SI h, string message);
wk_widget texmacs_wk_widget (int mask, command quit);
wk_widget plain_window_widget (wk_widget wid, string s);
wk_widget popup_window_widget (wk_widget wid, string s);
void      destroy_window_widget (wk_widget w);

#endif // defined WK_WIDGET_H
