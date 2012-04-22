
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
array<url> get_buffer_views (url name);
editor get_editor ();
bool has_view ();
void set_this_view (url u);
url  get_this_view (bool must_be_valid= true);
url  get_window_view (url win);
url  get_view_buffer (url u);
url  get_new_view (url name);
url  get_recent_view (url name);
url  get_passive_view (url name);
void delete_view (url u);
void window_set_view (url win, url new_u, bool focus);

url  create_buffer ();
void switch_to_buffer (url name);
void focus_on_editor (editor ed);
void kill_buffer (url name);
url  open_window (tree geom= "");
void clone_window ();
void kill_window ();
void kill_window_and_buffer ();
bool is_aux_buffer (url name);

// Low level types and routines
class tm_view_rep;
typedef tm_view_rep* tm_view;
tm_view search_view (url name);
url     get_name_view (tm_view vw);
void    detach_view (url u);

bool needs_to_be_saved (array<tm_view> vws);
bool needs_to_be_autosaved (array<tm_view> vws);
void set_master (array<tm_view> vws, url master);
void set_title (array<tm_view> vws, string title, url name);
void pretend_modified (array<tm_view> vws);
void pretend_saved (array<tm_view> vws);
void pretend_autosaved (array<tm_view> vws);
void set_data (array<tm_view> vws, new_data data);
void delete_views (array<tm_view> vws);

#endif // defined NEW_VIEW_H
