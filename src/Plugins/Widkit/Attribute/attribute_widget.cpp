
/******************************************************************************
* MODULE     : attribute_widget.cpp
* DESCRIPTION: Abstract attribute widgets accept events for
*              setting and retrieving attributes of a widget
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/attribute_widget.hpp"

/******************************************************************************
* Constructors for abstract attribute widgets
******************************************************************************/

attribute_widget_rep::attribute_widget_rep (gravity grav):
  basic_widget_rep (grav) {}
attribute_widget_rep::attribute_widget_rep (
  array<wk_widget> a, gravity grav):
    basic_widget_rep (a, grav) {}
attribute_widget_rep::attribute_widget_rep (
  array<wk_widget> a, array<string> name, gravity grav):
    basic_widget_rep (a, name, grav) {}

/******************************************************************************
* Retrieving information from attribute widgets
******************************************************************************/

void
attribute_widget_rep::handle_get_integer (get_integer_event ev) {
  WK_FAILED ("could not get integer attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_double (get_double_event ev) {
  WK_FAILED ("could not get double attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_string (get_string_event ev) {
  WK_FAILED ("could not get string attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_coord1 (get_coord1_event ev) {
  WK_FAILED ("could not get coord1 attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_coord2 (get_coord2_event ev) {
  WK_FAILED ("could not get coord2 attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_coord3 (get_coord3_event ev) {
  WK_FAILED ("could not get coord3 attribute " * ev->which);
}

void
attribute_widget_rep::handle_get_coord4 (get_coord4_event ev) {
  WK_FAILED ("could not get coord4 attribute " * ev->which);
}

/******************************************************************************
* Setting attributes of attribute widgets
******************************************************************************/

void
attribute_widget_rep::handle_set_integer (set_integer_event ev) {
  WK_FAILED ("could not set integer attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_double (set_double_event ev) {
  WK_FAILED ("could not set double attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_string (set_string_event ev) {
  WK_FAILED ("could not set string attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_coord1 (set_coord1_event ev) {
  WK_FAILED ("could not set coord1 attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_coord2 (set_coord2_event ev) {
  WK_FAILED ("could not set coord2 attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_coord3 (set_coord3_event ev) {
  WK_FAILED ("could not set coord3 attribute " * ev->which);
}

void
attribute_widget_rep::handle_set_coord4 (set_coord4_event ev) {
  WK_FAILED ("could not set coord4 attribute " * ev->which);
}

/******************************************************************************
* The main event handler
******************************************************************************/

bool
is_extra_width_event (event ev) {
  if (ev->type == GET_COORD2_EVENT) {
    get_coord2_event e (ev);
    return e->which == "extra width";
  }
  if (ev->type == SET_COORD2_EVENT) {
    set_coord2_event e (ev);
    return e->which == "extra width";
  }
  return false;
}

bool
attribute_widget_rep::handle (event ev) {
  if (!is_extra_width_event (ev))
    if (basic_widget_rep::handle (ev))
      return true;
  switch (ev->type) {
  case GET_INTEGER_EVENT:
    handle_get_integer (ev);
    return true;
  case GET_DOUBLE_EVENT:
    handle_get_double (ev);
    return true;
  case GET_STRING_EVENT:
    handle_get_string (ev);
    return true;
  case GET_COORD1_EVENT:
    handle_get_coord1 (ev);
    return true;
  case GET_COORD2_EVENT:
    handle_get_coord2 (ev);
    return true;
  case GET_COORD3_EVENT:
    handle_get_coord3 (ev);
    return true;
  case GET_COORD4_EVENT:
    handle_get_coord4 (ev);
    return true;

  case SET_INTEGER_EVENT:
    handle_set_integer (ev);
    return true;
  case SET_DOUBLE_EVENT:
    handle_set_double (ev);
    return true;
  case SET_STRING_EVENT:
    handle_set_string (ev);
    return true;
  case SET_COORD1_EVENT:
    handle_set_coord1 (ev);
    return true;
  case SET_COORD2_EVENT:
    handle_set_coord2 (ev);
    return true;
  case SET_COORD3_EVENT:
    handle_set_coord3 (ev);
    return true;
  case SET_COORD4_EVENT:
    handle_set_coord4 (ev);
    return true;
  }
  return false;
}
