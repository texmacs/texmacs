
/******************************************************************************
* MODULE     : composite_widget.hpp
* DESCRIPTION: composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef COMPOSITE_WIDGET_H
#define COMPOSITE_WIDGET_H
#include "Widget/basic_widget.hpp"
#include "Event/composite_event.hpp"

/******************************************************************************
* Abstract composite widgets
******************************************************************************/

class composite_widget_rep: public basic_widget_rep {
public:
  composite_widget_rep (gravity grav= north_west);
  composite_widget_rep (array<widget> a, gravity grav= north_west);
  composite_widget_rep (array<widget> a,
			array<string> name, gravity grav= north_west);

  virtual void handle_clean  (clean_event ev);
  virtual void handle_insert (insert_event ev);
  virtual void handle_remove (remove_event ev);
  virtual bool handle        (event ev);
};

#endif // defined COMPOSITE_WIDGET_H
