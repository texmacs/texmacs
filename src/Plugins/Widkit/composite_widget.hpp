
/******************************************************************************
* MODULE     : composite_widget.hpp
* DESCRIPTION: composite widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef COMPOSITE_WIDGET_H
#define COMPOSITE_WIDGET_H
#include "Widkit/basic_widget.hpp"
#include "Widkit/Event/composite_event.hpp"

/******************************************************************************
* Abstract composite widgets
******************************************************************************/

class composite_widget_rep: public basic_widget_rep {
public:
  composite_widget_rep (gravity grav= north_west);
  composite_widget_rep (array<wk_widget> a, gravity grav= north_west);
  composite_widget_rep (array<wk_widget> a,
			array<string> name, gravity grav= north_west);

  virtual void handle_clean  (clean_event ev);
  virtual void handle_insert (insert_event ev);
  virtual void handle_remove (remove_event ev);
  virtual bool handle        (event ev);
};

#endif // defined COMPOSITE_WIDGET_H
