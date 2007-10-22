
/******************************************************************************
* MODULE     : attribute_widget.hpp
* DESCRIPTION: Attribute widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  attribute_widget_rep (array<widget> a, gravity grav= north_west);
  attribute_widget_rep (array<widget> a,
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
