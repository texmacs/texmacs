
/******************************************************************************
* MODULE     : attribute_widget.hpp
* DESCRIPTION: Attribute widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ATTRIBUTE_WIDGET_H
#define ATTRIBUTE_WIDGET_H
#include "Widkit/basic_widget.hpp"
#include "Widkit/Event/attribute_event.hpp"

/******************************************************************************
* Abstract attribute widgets
******************************************************************************/

class attribute_widget_rep: public basic_widget_rep {
public:
  attribute_widget_rep (gravity grav= north_west);
  attribute_widget_rep (array<wk_widget> a, gravity grav= north_west);
  attribute_widget_rep (array<wk_widget> a,
			array<string> name, gravity grav= north_west);

  virtual void handle_get_integer   (get_integer_event ev);
  virtual void handle_get_double    (get_double_event ev);
  virtual void handle_get_string    (get_string_event ev);
  virtual void handle_get_coord1    (get_coord1_event ev);
  virtual void handle_get_coord2    (get_coord2_event ev);
  virtual void handle_get_coord3    (get_coord3_event ev);
  virtual void handle_get_coord4    (get_coord4_event ev);

  virtual void handle_set_integer   (set_integer_event ev);
  virtual void handle_set_double    (set_double_event ev);
  virtual void handle_set_string    (set_string_event ev);
  virtual void handle_set_coord1    (set_coord1_event ev);
  virtual void handle_set_coord2    (set_coord2_event ev);
  virtual void handle_set_coord3    (set_coord3_event ev);
  virtual void handle_set_coord4    (set_coord4_event ev);

  virtual bool handle               (event ev);
};

#endif // defined ATTRIBUTE_WIDGET_H
