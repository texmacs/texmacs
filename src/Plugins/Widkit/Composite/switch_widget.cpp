
/******************************************************************************
* MODULE     : switch_widget.cpp
* DESCRIPTION: switch between several possibilities
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Widkit/composite_widget.hpp"
#include "Widkit/Event/attribute_event.hpp"

/******************************************************************************
* Switch widgets
******************************************************************************/

class switch_widget_rep: public composite_widget_rep {
  int current;
  array<wk_widget> variant;
  array<string>    variant_name;
public:
  switch_widget_rep (array<wk_widget> a, array<string> name, int i);
  operator tree ();

  void handle_attach_window (attach_window_event ev);
  void handle_get_widget    (get_widget_event ev);
  void handle_set_widget    (set_widget_event ev);
  void handle_get_integer   (get_integer_event ev);
  void handle_set_integer   (set_integer_event ev);
  bool handle               (event ev);
};

switch_widget_rep::switch_widget_rep (array<wk_widget> v,
  array<string> name, int init):
    composite_widget_rep (1),
    current (init),
    variant (v), variant_name (name)
{
  a[0]= variant [current];
}

switch_widget_rep::operator tree () {
  int i;
  tree t (TUPLE, N(variant)+1);
  t[0]= "switch";
  for (i=0; i<N(variant); i++) t[i+1]= (tree) variant[i];
  return t;
}

void
switch_widget_rep::handle_attach_window (attach_window_event ev) {
  int i, n= N(variant);
  composite_widget_rep::handle_attach_window (ev);
  for (i=0; i<n; i++)
    if (i != current)
      variant[i] << emit_attach_window (win);
}

void
switch_widget_rep::handle_get_widget (get_widget_event ev) {
  int i, n= N(variant);
  for (i=0; i<n; i++)
    if (variant_name[i] == ev->which) {
      ev->w= variant[i];
      return;
    }
  for (i=0; i<n; i++)
    if (variant_name[i] == "default") {
      variant[i] << ev;
      return;
    }
  variant [current] << ev;
}

void
switch_widget_rep::handle_set_widget (set_widget_event ev) {
  int i, n= N(variant);
  for (i=0; i<n; i++)
    if (variant_name[i] == ev->which) {
      variant[i]= ev->w;
      return;
    }
  for (i=0; i<n; i++)
    if (variant_name[i] == "default") {
      variant[i] << ev;
      return;
    }
  variant [current] << ev;
}

void
switch_widget_rep::handle_get_integer (get_integer_event ev) {
  if (ev->which == "switch") ev->i= current;
  else variant [current] << ev;
}

void
switch_widget_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "switch") {
    current= ev->i;
    a[0]   = variant [current];
  }
  else variant [current] << ev;
}

bool
switch_widget_rep::handle (event ev) {
  switch (ev->type) {
  case ATTACH_WINDOW_EVENT:
    handle_attach_window (ev);
    return true;
  case GET_WIDGET_EVENT:
    handle_get_widget (ev);
    return true;
  case SET_WIDGET_EVENT:
    handle_set_widget (ev);
    return true;
  case GET_INTEGER_EVENT:
    handle_get_integer (ev);
    return true;
  case SET_INTEGER_EVENT:
    handle_set_integer (ev);
    return true;
  default:
    return composite_widget_rep::handle (ev);
  }
  return false;
}

/******************************************************************************
* Wrapped widgets
******************************************************************************/

class wrapped_widget_rep: public composite_widget_rep {
  command quit;
public:
  wrapped_widget_rep (wk_widget w, command quit);
  ~wrapped_widget_rep ();
  void handle_destroy (destroy_event ev);
  operator tree ();
};

wrapped_widget_rep::wrapped_widget_rep (wk_widget w, command q):
  composite_widget_rep (1), quit (q) { a[0]= w; }

wrapped_widget_rep::~wrapped_widget_rep () {
  if (!is_nil (quit)) quit (); }

void
wrapped_widget_rep::handle_destroy (destroy_event ev) {
  if (!is_nil (quit)) quit ();
  quit= command (); }

wrapped_widget_rep::operator tree () {
  return tree (TUPLE, "wrapped", (tree) a[0]); }

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
switch_widget (array<wk_widget> a, array<string> name, int init) {
  return tm_new<switch_widget_rep> (a, name, init);
}

wk_widget
optional_widget (wk_widget w, bool on) {
  array<wk_widget> a (2);
  a[0]= w;
  a[1]= glue_wk_widget (false, false);
  array<string> name (2);
  name[0]= "default";
  return switch_widget (a, name, on? 0: 1);
}

wk_widget
wrapped_widget (wk_widget w, command cmd) {
  return tm_new<wrapped_widget_rep> (w, cmd);
}
