
/******************************************************************************
* MODULE     : widkit_widget.hpp
* DESCRIPTION: Definition of abstract native widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef WIDKIT_WIDGET_H
#define WIDKIT_WIDGET_H
#include "widget.hpp"
#include "Widkit/event.hpp"

/******************************************************************************
* The abstract widkit_widget class
******************************************************************************/

class widkit_widget_rep;
class widkit_widget {
public:
  ABSTRACT_NULL(widkit_widget);
  inline widkit_widget operator [] (int i);
         widkit_widget operator [] (string s);
  inline operator tree ();
  inline bool operator == (widkit_widget w);
  inline bool operator != (widkit_widget w);
};

class widkit_widget_rep: public abstract_struct {
public:
  window   win;                 // underlying window
  SI       ox, oy;              // origin of widget in window
  SI       w, h;                // width and height of widget
  gravity  grav;                // position of the origin in the widget
  array<widkit_widget>  a;      // children of widget
  array<string>         name;   // names for the children

  widkit_widget_rep (array<widkit_widget> a, array<string> name, gravity grav);
  virtual ~widkit_widget_rep ();

  virtual operator tree () = 0;
  virtual bool handle (event ev) = 0;

  SI       x1 (); SI y1 (); // lower left window coordinates of widget
  SI       x2 (); SI y2 (); // upper right window coordinates of widget
  bool     attached ();
  void     fatal_error (string message, string in="", string fname="");

  friend   class widkit_widget;
};
ABSTRACT_NULL_CODE(widkit_widget);

inline widkit_widget widkit_widget::operator [] (int i) { return rep->a[i]; }
inline widkit_widget::operator tree () { return (tree) (*rep); }
inline bool widkit_widget::operator == (widkit_widget w) {
  return rep == w.rep; }
inline bool widkit_widget::operator != (widkit_widget w) {
  return rep != w.rep; }

ostream& operator << (ostream& out, widkit_widget w);
widkit_widget operator << (widkit_widget w, event ev);

typedef widkit_widget widget;

#endif // defined WIDKIT_WIDGET_H
