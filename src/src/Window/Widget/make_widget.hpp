
/******************************************************************************
* MODULE     : make_widget.hpp
* DESCRIPTION: Abstract dynamic make_widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MAKE_WIDGET_H
#define MAKE_WIDGET_H
#include "widget.hpp"

/******************************************************************************
* Abstract widget creation functions
******************************************************************************/

class make_widget_rep: public abstract_struct {
public:
  inline make_widget_rep () {}
  inline virtual ~make_widget_rep () {}
  inline virtual ostream& print (ostream& out);
  virtual widget get_widget () = 0;
};

class make_widget {
public:
  ABSTRACT_NULL(make_widget);
  inline widget operator () ();
  inline friend ostream& operator << (ostream& out, make_widget cmd);
};
ABSTRACT_NULL_CODE(make_widget);

inline ostream& make_widget_rep::print (ostream& out) {
  return out << "make_widget"; }
inline widget make_widget::operator () () {
  return rep->get_widget (); }
inline bool operator == (make_widget mw1, make_widget mw2) {
  return mw1.rep == mw2.rep; }
inline ostream& operator << (ostream& out, make_widget cmd) {
  if (nil(cmd)) return out << "(null)"; else return cmd->print(out); }

/******************************************************************************
* Exported routines
******************************************************************************/

widget pulldown_button (widget w, make_widget mw);
widget pullright_button (widget w, make_widget mw);

#endif // defined MAKE_WIDGET_H
