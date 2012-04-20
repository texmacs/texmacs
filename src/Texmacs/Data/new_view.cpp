
/******************************************************************************
* MODULE     : new_view.cpp
* DESCRIPTION: View management
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
* Associating URLs to views
******************************************************************************/

static hashmap<tree,int> view_number_table (0);
static hashmap<tree,pointer> view_table (NULL);

static int
new_view_number (url u) {
  view_number_table (u->t) += 1;
  return view_number_table [u->t];
}

tm_view_rep::tm_view_rep (tm_buffer buf2, editor ed2):
  buf (buf2), ed (ed2), win (NULL), nr (new_view_number (buf->buf->name)) {}

static string
encode_url (url u) {
  return get_root (u) * "/" * as_string (unroot (u));
}

static url
decode_url (string s) {
  int i= search_forwards ("/", 0, s);
  if (i < 0) return url_none ();
  return url_root (s (0, i)) * url (s (i+1, N(s)));
}

url
get_name_view (tm_view vw) {
  string name= encode_url (vw->buf->buf->name);
  //cout << vw->buf->buf->name << " -> " << name << "\n";
  string nr  = as_string (vw->nr);
  return "tmfs://view/" * nr * "/" * name;
}

tm_view
search_view (url u) {
  string s= as_string (u);
  if (!starts (s, "tmfs://view/")) return NULL;
  s= s (N (string ("tmfs://view/")), N(s));
  int i= search_forwards ("/", 0, s);
  if (i < 0) return NULL;
  int nr= as_int (s (0, i));
  url name= decode_url (s (i+1, N(s)));
  //cout << s (i+1, N(s)) << " -> " << name << "\n";
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return NULL;
  for (i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->nr == nr)
      return buf->vws[i];
  return NULL;
}

/******************************************************************************
* Views associated to editor, window, or buffer
******************************************************************************/

url
get_this_view () {
  tm_view vw= get_view ();
  if (vw == NULL) return url_none ();
  return get_name_view (vw);
}

url
get_window_view (int id) {
  tm_view vw= window_find_view (id);
  if (vw == NULL) return url_none ();
  return get_name_view (vw);
}

array<url>
get_buffer_views (url name) {
  tm_buffer buf= search_buffer (name);
  array<url> r;
  if (is_nil (buf)) return r;
  for (int i=0; i<N(buf->vws); i++)
    r << get_name_view (buf->vws[i]);
  return r;
}

url
get_view_buffer (url u) {
  tm_view vw= search_view (u);
  if (vw == NULL) return url_none ();
  return vw->buf->buf->name;
}

/******************************************************************************
* Viewing history
******************************************************************************/

array<url> view_history;

void
notify_set_view (url u) {
  int i;
  for (i= 0; i<N(view_history); i++)
    if (view_history[i] == u) break;
  if (i >= N(view_history))
    view_history= append (u, view_history);
  else {
    int j;
    for (j=i; j>0; j--)
      view_history[j]= view_history[j-1];
    view_history[j]= u;
  }
}

url
get_recent_view (url name, bool same, bool other, bool active, bool passive) {
  // Get most recent view with the following filters:
  //   If same, then the name of the buffer much be name
  //   If other, then the name of the buffer much be other than name
  //   If active, then the buffer must be active
  //   If passive, then the buffer must be passive
  int i;
  for (i= 0; i < N(view_history); i++) {
    tm_view vw= search_view (view_history[i]);
    if (vw != NULL) {
      if (same && vw->buf->buf->name != name) continue;
      if (other && vw->buf->buf->name == name) continue;
      if (active && vw->win == NULL) continue;
      if (passive && vw->win != NULL) continue;
      return view_history[i];
    }
  }
  return url_none ();
}

array<url>
get_view_history () {
  return view_history;
}

/******************************************************************************
* Creation of views on buffers
******************************************************************************/

url tm_init_buffer_file= url_none ();
url my_init_buffer_file= url_none ();

url
get_new_view (url name) {
  //cout << "Creating new view\n";

  create_buffer (name, tree (DOCUMENT));
  tm_buffer buf= search_buffer (name);
  editor    ed = new_editor (get_server () -> get_server (), buf);
  tm_view   vw = tm_new<tm_view_rep> (buf, ed);
  buf->vws << vw;
  ed->set_data (buf->data);

  tm_view temp_vw= get_view (false);
  set_view (vw);
  if (is_none (tm_init_buffer_file))
    tm_init_buffer_file= "$TEXMACS_PATH/progs/init-buffer.scm";
  if (is_none (my_init_buffer_file))
    my_init_buffer_file= "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm";
  if (exists (tm_init_buffer_file)) exec_file (tm_init_buffer_file);
  if (exists (my_init_buffer_file)) exec_file (my_init_buffer_file);
  set_view (temp_vw);

  //cout << "View created\n";
  return get_name_view (vw);
}

url
get_passive_view (url name) {
  // Get a view on a buffer, but not one which is attached to a window
  // Create a new view if no such view exists
  tm_buffer buf= search_buffer_insist (name);
  if (is_nil (buf)) return url_none ();
  for (int i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->win == NULL)
      return get_name_view (buf->vws[i]);
  return get_new_view (buf->buf->name);
}

url
get_recent_view (url name) {
  // Get (most) recent view on a buffer, with a preference for
  // the current buffer or another view attached to a window
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf) || N(buf->vws) == 0)
    return get_new_view (name);
  tm_view vw= get_view ();
  if (vw->buf == buf) return get_name_view (vw);
  url r= get_recent_view (name, true, false, true, false);
  if (!is_none (r)) return r;
  r= get_recent_view (name, true, false, false, false);
  if (!is_none (r)) return r;
  return get_name_view (buf->vws[0]);
}

/******************************************************************************
* Destroying a view
******************************************************************************/

void
delete_view (url u) {
  tm_view vw= search_view (u);
  if (vw == NULL) return;
  tm_buffer buf= vw->buf;
  int i, j, n= N(buf->vws);
  for (i=0; i<n; i++)
    if (buf->vws[i] == vw) {
      array<tm_view> a (n-1);
      for (j=0; j<n-1; j++)
	if (j<i) a[j]= buf->vws[j];
	else a[j]= buf->vws[j+1];
      buf->vws= a;
    }
  // tm_delete (vw);
  // FIXME: causes very annoying segfault;
  // recently introduced during reorganization
}

/******************************************************************************
* Replacing views
******************************************************************************/

void
attach_view (int id, url u) {
  tm_window win= search_window (id);
  tm_view   vw = search_view (u);
  if (win == NULL || vw == NULL) return;
  // cout << "Attach view " << vw->buf->buf->name << "\n";
  vw->win= win;
  widget wid= win->wid;
  set_scrollable (wid, vw->ed);
  vw->ed->cvw= wid.rep;
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->resume ();
  win->set_window_name (vw->buf->buf->title);
  win->set_window_url (vw->buf->buf->name);
  notify_set_view (u);
  vw->buf->buf->last_visit= texmacs_time ();
  // cout << "View attached\n";
}

void
detach_view (url u) {
  tm_view vw = search_view (u);
  if (vw == NULL) return;
  tm_window win= vw->win;
  if (win == NULL) return;
  // cout << "Detach view " << vw->buf->buf->name << "\n";
  vw->win= NULL;
  widget wid= win->wid;
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->suspend ();
  set_scrollable (wid, glue_widget ());
  win->set_window_name ("TeXmacs");
  win->set_window_url (url_none ());
  // cout << "View detached\n";
}

void
window_set_view (int id, url new_u) {
  tm_window win= search_window (id);
  if (win == NULL) return;
  tm_view new_vw= search_view (new_u);
  if (new_vw == NULL || new_vw->win == win) return;
  ASSERT (new_vw->win == NULL, "view attached to other window");
  tm_view old_vw= window_find_view (id);
  if (old_vw != NULL) detach_view (get_name_view (old_vw));
  attach_view (win->id, new_u);
  if (get_view () == old_vw) set_view (new_vw);
}

/******************************************************************************
* Creating, destroying, and changing buffers
******************************************************************************/

url
create_buffer () {
  url name= make_new_buffer ();
  switch_to_buffer (name);
  return name;
}

void
kill_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  if (N(bufs) <= 1) get_server () -> quit();
  for (int i=0; i<N(buf->vws); i++) {
    tm_view old_vw= buf->vws[i];
    if (old_vw->win != NULL) {
      url prev= get_recent_view (name, false, true, false, true);
      if (is_none (prev)) {
        prev= get_recent_view (name, false, true, false, false);
        prev= get_new_view (get_view_buffer (prev));
      }
      window_set_view (old_vw->win->id, prev);
    }
  }
  remove_buffer (name);
}

void
switch_to_buffer (url name) {
  // cout << "Switching to buffer " << buf->buf->name << "\n";
  tm_window win= get_window ();
  url u= get_passive_view (name);
  tm_view vw= search_view (u);
  if (vw == NULL) return;
  window_set_view (win->id, u);
  tm_window nwin= vw->win;
  nwin->set_shrinking_factor (nwin->get_shrinking_factor ());
  // cout << "Switched to buffer " << new_vw->buf->buf->name << "\n";
}

/******************************************************************************
* Routines on collections of views
******************************************************************************/

bool
needs_to_be_saved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
needs_to_be_autosaved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}

void
set_master (array<tm_view> vws, url master) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->set_master (master);
}

void
set_title (array<tm_view> vws, string title, url name) {
  for (int i=0; i<N(vws); i++) {
    tm_view vw= vws[i];
    if (vw->win != NULL) {
      vw->win->set_window_name (title);
      vw->win->set_window_url (name);
    }
  }
}

void
pretend_modified (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->require_save ();
}

void
pretend_saved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->notify_save ();
}

void
pretend_autosaved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->notify_save (false);
}

void
set_data (array<tm_view> vws, new_data data) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->set_data (data);
}

void
delete_views (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    delete_view (get_name_view (vws[i]));
}
