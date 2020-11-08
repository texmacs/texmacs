
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
  virtual tm_ostream& print (tm_ostream& out);
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

SI decode_length (string s, wk_widget w, int style);

#define WK_FAILED(msg) { wk_error (msg); FAILED ("widget_error"); }

/******************************************************************************
* Exported special widgets and window widget destruction
******************************************************************************/

wk_widget extend (wk_widget w, array<wk_widget> a);
wk_widget horizontal_list (array<wk_widget> a);
wk_widget horizontal_list (array<wk_widget> a, array<string> name);
wk_widget vertical_list (array<wk_widget> a);
wk_widget vertical_list (array<wk_widget> a, array<string> name);
wk_widget vertical_menu (array<wk_widget> a);
wk_widget tile (array<wk_widget> a, int cols);
wk_widget tile (array<wk_widget> a, int cols, array<string> name);
wk_widget aligned_widget (array<wk_widget> lhs, array<wk_widget> rhs,
			  SI hsep= 3*PIXEL, SI vsep= 3*PIXEL,
                          SI lpad= 0, SI rpad= 0);
wk_widget horizontal_array (array<wk_widget> a, int stretch_me= -1);
wk_widget horizontal_array (array<wk_widget> a, array<string> s,
			    int stretch_me= -1);
wk_widget tabs_widget (array<wk_widget> tabs, array<wk_widget> bodies);
wk_widget icon_tabs_widget (array<url> us, array<wk_widget> tabs,
                            array<wk_widget> bodies);
wk_widget switch_widget (array<wk_widget> a, array<string> name, int init= 0);
wk_widget optional_widget (wk_widget w, bool on= true);
wk_widget wrapped_widget (wk_widget w, command cmd);
wk_widget glue_wk_widget (bool hx=true, bool vx=true, SI w=0, SI h=0);
wk_widget glue_wk_widget (tree c, bool hx=true, bool vx=true, SI w=0, SI h=0);
wk_widget separator_wk_widget (SI pre=0, SI post=0, bool vert=false);
wk_widget text_wk_widget (string s, int style= 0, bool tsp= false);
wk_widget menu_text_wk_widget (string s, int style, color col,
			       bool tsp=true, bool tt= false);
wk_widget xpm_wk_widget (url file_name, bool transp= true);
wk_widget minibar_widget (wk_widget w);
wk_widget command_button (wk_widget w, command cmd, int style= 0);
wk_widget command_button (wk_widget lw, wk_widget rw, command cmd, int st= 0);
wk_widget command_button (wk_widget lw, wk_widget cw, wk_widget rw,
			  command c, int style= 0);
wk_widget pulldown_button (wk_widget w, wk_widget m, int style= 0);
wk_widget pullright_button (wk_widget w, wk_widget m, int style= 0);
wk_widget pulldown_button (wk_widget w, promise<wk_widget> pw);
wk_widget pullright_button (wk_widget w, promise<wk_widget> pw);
wk_widget toggle_wk_widget (command cmd, bool on= false, int style= 0);
wk_widget popup_widget (wk_widget w, gravity quit=center);
wk_widget canvas_widget (wk_widget w, gravity grav=north_west, bool rf= false);
wk_widget user_canvas_widget (wk_widget wid, int style= 0);
wk_widget resize_widget (wk_widget w, int style, string w1, string h1,
                         string w2, string h2, string w3, string h3,
                         string hpos, string vpos);
wk_widget hsplit_widget (wk_widget l, wk_widget r);
wk_widget vsplit_widget (wk_widget t, wk_widget b);
wk_widget input_text_wk_widget (command cb,
				int style= 0, string w= "1w", bool pf= true);
wk_widget input_text_wk_widget (command cb, string type, array<string> def,
				int style= 0, string w= "1w", bool pf= false);
wk_widget inputs_list_wk_widget (command call_back, array<string> prompts);
wk_widget enum_wk_widget (command cb, array<string> vals, string cur,
                          int style= 0, string w= "1w");
wk_widget choice_wk_widget (command cb, array<string> vals, string val);
wk_widget choice_wk_widget (command cb, array<string> vals, array<string> mc);
wk_widget file_chooser_wk_widget (command cmd, string type);
wk_widget color_picker_wk_widget (command cmd, bool bg, array<tree> proposals);
wk_widget balloon_widget (wk_widget w, wk_widget help);
wk_widget wait_wk_widget (SI w, SI h, string message);
wk_widget ink_wk_widget (command cb);
wk_widget refresh_wk_widget (string tmwid, string kind);
wk_widget refreshable_wk_widget (object promise, string kind);
wk_widget texmacs_wk_widget (int mask, command quit);
wk_widget plain_window_widget (wk_widget wid, string s, command q= command ());
wk_widget popup_window_widget (wk_widget wid, string s);
wk_widget tooltip_window_widget (wk_widget wid, string s);
void      destroy_window_widget (wk_widget w);

#endif // defined WK_WIDGET_H
