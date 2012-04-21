
/******************************************************************************
* MODULE     : new_window.hpp
* DESCRIPTION: Window management
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_WINDOW_H
#define NEW_WINDOW_H
#include "tree.hpp"
#include "url.hpp"

array<url> windows_list ();
array<url> buffer_to_windows (url name);
url  get_this_window ();
url  window_to_buffer (url win);
void window_set_buffer (url win, url name);
void window_focus (url win);

// Low level types and routines
class tm_window_rep;
typedef tm_window_rep* tm_window;
tm_window search_window (url win);
url get_name_window (tm_window win);

#endif // defined NEW_WINDOW_H
