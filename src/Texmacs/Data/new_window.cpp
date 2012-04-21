
/******************************************************************************
* MODULE     : new_window.cpp
* DESCRIPTION: Global window management
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_data.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "web_files.hpp"
#include "tm_link.hpp"
#include "message.hpp"
#include "dictionary.hpp"
#include "new_document.hpp"

/******************************************************************************
* Manage global list of windows
******************************************************************************/

static int last_window= 1;
static array<url> all_windows;

static path
reset (path p, int i) {
  if (is_nil (p)) return p;
  else if (p->item == i) return p->next;
  else return path (p->item, reset (p->next, i));
}

url
create_window_id () {
  url r= "tmfs://window/" * as_string (last_window);
  all_windows << r;
  last_window++;
  return r;
}

void
destroy_window_id (url win) {
  for (int i=0; i<N(all_windows); i++)
    if (all_windows[i] == win) {
      all_windows= append (range (all_windows, 0, i),
                           range (all_windows, i+1, N(all_windows)));
      return;
    }
}

url
get_name_window (tm_window win) {
  return win->id;
}

/******************************************************************************
* Low level creation and destruction of windows
******************************************************************************/

static hashmap<url,tm_window> tm_window_table (NULL);

class kill_window_command_rep: public command_rep {
public:
  inline kill_window_command_rep () {}
  inline void apply () { exec_delayed (scheme_cmd ("(safely-kill-window)")); }
  tm_ostream& print (tm_ostream& out) { return out << "kill window"; }
};

url
new_window (bool map_flag= true, tree geom= "") {
  int mask= 0;
  if (get_preference ("header") == "on") mask += 1;
  if (get_preference ("main icon bar") == "on") mask += 2;
  if (get_preference ("mode dependent icons") == "on") mask += 4;
  if (get_preference ("focus dependent icons") == "on") mask += 8;
  if (get_preference ("user provided icons") == "on") mask += 16;
  if (get_preference ("status bar") == "on") mask += 32;
  command quit= tm_new<kill_window_command_rep> ();
  tm_window win= tm_new<tm_window_rep> (texmacs_widget (mask, quit), geom);
  tm_window_table (win->id)= win;
  if (map_flag) win->map ();
  return get_name_window (win);
}

static bool
delete_view_from_window (tm_window win) {
  int i, j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= buf->vws[j];
      if (vw->win == win) {
	detach_view (get_name_view (vw));
	delete_view (get_name_view (vw));
	return true;
      }
    }
  }
  return false;
}

void
delete_window (url win_u) {
  tm_window win= search_window (win_u);
  if (win == NULL) return;
  while (delete_view_from_window (win)) {}
  win->unmap ();
  tm_window_table->reset (win->id);
  destroy_window_widget (win->win);
  tm_delete (win);
}

tm_window
search_window (url win) {
  return tm_window_table [win];
}

/******************************************************************************
* Manage global list of windows
******************************************************************************/

array<url>
windows_list () {
  return all_windows;
}

url
get_this_window () {
  tm_window win= get_window ();
  return get_name_window (win);
}

array<url>
buffer_to_windows (url name) {
  array<url> r;
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return r;
  for (int i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->win != NULL)
      r << get_name_window (buf->vws[i]->win);
  return r;
}

url
window_to_buffer (url win) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (get_name_window (bufs[i]->vws[j]->win) == win)
	  return bufs[i]->buf->name;
  return url_none ();
}

url
get_window_view (url win) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (get_name_window (bufs[i]->vws[j]->win) == win)
	  return get_name_view (bufs[i]->vws[j]);
  return url_none ();
}

void
window_set_buffer (url win, url name) {
  url old= get_window_view (win);
  if (is_none (old) || get_view_buffer (old) == name) return;
  window_set_view (win, get_passive_view (name), false);
}

void
window_focus (url win) {
  if (win == get_this_window ()) return;
  url old= get_window_view (win);
  if (is_none (old)) return;
  tm_view vw= search_view (old);
  set_view (vw);
  vw->buf->buf->last_visit= texmacs_time ();
}

/******************************************************************************
* Other subroutines
******************************************************************************/

void
create_buffer (url name, tree doc) {
  if (!is_nil (search_buffer (name))) return;
  set_buffer_tree (name, doc);
}

void
new_buffer_in_this_window (url name, tree doc) {
  if (is_nil (search_buffer (name)))
    create_buffer (name, doc);
  switch_to_buffer (name);
}

void
new_buffer_in_new_window (url name, tree doc, tree geom) {
  if (is_nil (search_buffer (name)))
    create_buffer (name, doc);
  url win= new_window (true, geom);
  window_set_view (win, get_passive_view (name), true);
}

/******************************************************************************
* Exported routines
******************************************************************************/

url
open_window (tree geom) {
  url name= make_new_buffer ();
  new_buffer_in_new_window (name, tree (DOCUMENT), geom);
  return name;
}

void
clone_window () {
  url win= new_window ();
  window_set_view (win, get_passive_view (get_this_buffer ()), true);
}

void
kill_window () {
  int i, j;
  url win= get_this_window ();
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= buf->vws[j];
      if (vw->win != NULL && get_name_window (vw->win) != win) {
	set_view (vw);
        vw->buf->buf->last_visit= texmacs_time ();
	delete_window (win);
	return;
      }
    }
  }
  if (number_of_servers () == 0) get_server () -> quit ();
  else delete_window (win);
}

void
kill_window_and_buffer () {
  if (N(bufs) <= 1) get_server () -> quit();
  url name= get_this_buffer ();
  int i;
  bool kill= true;
  tm_buffer buf= get_buffer();
  tm_window win= get_window ();
  for (i=0; i<N(buf->vws); i++) {
    tm_view old_vw= buf->vws[i];
    if (old_vw->win != win) kill= false;
  }
  kill_window ();
  if (kill) remove_buffer (name);
}
