
/******************************************************************************
* MODULE     : new_view.hpp
* DESCRIPTION: View management
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_VIEW_H
#define NEW_VIEW_H
#include "tree.hpp"
#include "url.hpp"
class editor;

array<url> get_view_history ();
array<url> buffer_to_views (url name);
array<url> get_all_views ();
editor get_current_editor ();
editor view_to_editor (url u);
bool has_current_view ();
void set_current_view (url u);
url  get_current_view ();
url  get_current_view_safe ();
url  window_to_view (url win);
url  view_to_buffer (url u);
url  view_to_window (url u);
url  get_new_view (url name);
url  get_recent_view (url name);
url  get_passive_view (url name);
void delete_view (url u);
void window_set_view (url win, url new_u, bool focus);
void switch_to_buffer (url name);
void focus_on_editor (editor ed);

// Low level types and routines
class tm_view_rep;
typedef tm_view_rep* tm_view;
tm_view concrete_view (url name);
url     abstract_view (tm_view vw);
void    detach_view (url u);
url     get_recent_view (url name, bool s, bool o, bool a, bool p);

#endif // defined NEW_VIEW_H
