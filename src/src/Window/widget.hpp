
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
#include "ntuple.hpp"
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

  virtual void set_blackbox (string key, blackbox val);
  virtual blackbox get_blackbox (string key, int type_id);
  virtual void changed (string key, int type_id);
  virtual void connect (string key, widget w2, string key2);
  virtual void deconnect (string key, widget w2, string key2);

  template<class T1> inline void
  set (string key, T1 val) {
    set_blackbox (key, close_box (val)); }
  template<class T1, class T2> void
  set (string key, T1 val1, T2 val2) {
    typedef pair<T1,T2> T;
    set_blackbox (key, close_box<T> (T (val1, val2))); }
  template<class T1, class T2, class T3> void
  set (string key, T1 val1, T2 val2, T3 val3) {
    typedef triple<T1,T2,T3> T;
    set_blackbox (key, close_box<T> (T (val1, val2, val3))); }
  template<class T1, class T2, class T3, class T4> void
  set (string key, T1 val1, T2 val2, T3 val3, T4 val4) {
    typedef quadruple<T1,T2,T3,T4> T;
    set_blackbox (key, close_box<T> (T (val1, val2, val3, val4))); }

  template<class T1> inline T1
  get (string key) {
    return open_box<T1> (get_blackbox (key, type_helper<T1>::id)); }
  template<class T1, class T2> void
  get (string key, T1& val1, T2& val2) {
    typedef pair<T1,T2> T;
    T p= open_box<T> (get_blackbox (key, type_helper<T>::id));
    val1= p.x1; val2= p.x2; }
  template<class T1, class T2, class T3> void
  get (string key, T1& val1, T2& val2, T3& val3) {
    typedef triple<T1,T2,T3> T;
    T t= open_box<T> (get_blackbox (key, type_helper<T>::id));
    val1= t.x1; val2= t.x2; val3= t.x3; }
  template<class T1, class T2, class T3, class T4> void
  get (string key, T1& val1, T2& val2, T3& val3, T4& val4) {
    typedef quadruple<T1,T2,T3,T4> T;
    T q= open_box<T> (get_blackbox (key, type_helper<T>::id));
    val1= q.x1; val2= q.x2; val3= q.x3; val4= q.x4; }

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
