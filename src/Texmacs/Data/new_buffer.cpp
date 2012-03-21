
/******************************************************************************
* MODULE     : new_buffer.cpp
* DESCRIPTION: Buffer management
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

array<tm_buffer> bufs;

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
pretend_saved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->notify_save ();
}

void
set_data (array<tm_view> vws, new_data data) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->set_data (data);
}

void
delete_views (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    delete_view (vws[i]);
}

/******************************************************************************
* Check for changes in the buffer
******************************************************************************/

bool
tm_buffer_rep::needs_to_be_saved () {
  if (buf->read_only) return false;
  return ::needs_to_be_saved (vws);
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (buf->read_only) return false;
  return ::needs_to_be_autosaved (vws);
}

/******************************************************************************
* Main buffer management
******************************************************************************/

void
insert_buffer (url name) {
  if (is_none (name)) return;
  if (!is_nil (search_buffer (name))) return;
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  bufs << buf;
}

int
find_buffer (url name) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->buf->name == name)
      return i;
  return -1;
}

void
remove_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return;
  int n= N(bufs);
  tm_buffer buf= bufs[nr];
  for (int i=nr; i<n-1; i++)
    bufs[i]= bufs[i+1];
  bufs->resize (n-1);
  delete_views (buf->vws);
  tm_delete (buf);
}

int
number_buffers () {
  return N(bufs);
}

url
get_all_buffers () {
  url u= url_none ();
  for (int i=N(bufs)-1; i>=0; i--)
    u= bufs[i]->buf->name | u;
  return u;
}

tm_buffer
search_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return nil_buffer ();
  return bufs[nr];
}

/******************************************************************************
* Information attached to buffers
******************************************************************************/

string
propose_title (string old_title, url u, tree doc) {
  string name= as_string (tail (u));
  if (starts (name, "no_name_") && ends (name, ".tm")) {
    string no_name= translate ("No name");
    for (int i=0; i<N(no_name); i++)
      if (((unsigned char) (no_name[i])) >= (unsigned char) 128)
	{ no_name= "No name"; break; }
    name= no_name * " [" * name (8, N(name) - 3) * "]";
  }
  if ((name == "") || (name == "."))
    name= as_string (tail (u * url_parent ()));
  if ((name == "") || (name == "."))
    name= as_string (u);
  if (is_rooted_tmfs (u))
    name= as_string (call ("tmfs-title", as_string (u), object (doc)));

  int i, j;
  for (j=1; true; j++) {
    bool flag= true;
    string ret (name);
    if (j>1) ret= name * " (" * as_string (j) * ")";
    if (ret == old_title) return ret;
    for (i=0; i<N(bufs); i++)
      if (bufs[i]->buf->title == ret) flag= false;
    if (flag) return ret;
  }
}

url
get_this_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->buf->name;
}

url
get_name_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return bufs[i]->buf->name;
  return url_none ();
}

void
rename_buffer (url name, url new_name) {
  if (new_name == name) return;
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  buf->buf->name= new_name;
  buf->buf->master= new_name;
  tree doc= subtree (the_et, buf->rp);
  set_title_buffer (new_name, propose_title (buf->buf->title, new_name, doc));
}

url
get_master_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return url_none ();
  return buf->buf->master;
}

void
set_master_buffer (url name, url master) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->master == master) return;
  buf->buf->master= master;
  set_master (buf->vws, master);
}

string
get_title_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return "";
  return buf->buf->title;
}

void
set_title_buffer (url name, string title) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->title == title) return;
  buf->buf->title= title;
  set_title (buf->vws, buf->buf->title, buf->buf->name);
}

bool
is_aux_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return false;
  return buf->buf->master != buf->buf->name;
}

double
last_visited (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return (double) texmacs_time ();
  return (double) buf->buf->last_visit;
}

bool
buffer_modified (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return false;
  return buf->needs_to_be_saved ();
}

void
pretend_buffer_saved (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  pretend_saved (buf->vws);
}

void
set_buffer_data (url name, new_data data) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return;
  set_data (buf->vws, data);
}

void
set_buffer_tree (url name, tree doc) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) {
    insert_buffer (name);
    buf= search_buffer (name);
    tree body= detach_data (doc, buf->data);
    set_document (buf->rp, body);
    buf->buf->title= propose_title (buf->buf->title, name, body);
    if (buf->data->project != "") {
      url prj_name= head (name) * as_string (buf->data->project);
      buf->prj= load_passive_buffer (prj_name);
    }
  }
  else {
    string old_title= buf->buf->title;
    string old_project= buf->data->project->label;
    tree body= detach_data (doc, buf->data);
    assign (buf->rp, body);
    set_buffer_data (name, buf->data);
    buf->buf->title= propose_title (old_title, name, body);
    if (buf->data->project != "" && buf->data->project != old_project) {
      url prj_name= head (name) * as_string (buf->data->project);
      buf->prj= load_passive_buffer (prj_name);
    }
    pretend_buffer_saved (name);
  }
}

tree
get_buffer_tree (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return "";
  tree body= subtree (the_et, buf->rp);
  return attach_data (body, buf->data, true);
}

void
set_buffer_body (url name, tree body) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) {
    new_data data;
    set_buffer_tree (name, attach_data (body, data));
  }
  else {
    assign (buf->rp, body);
    pretend_buffer_saved (name);
  }
}

tree
get_buffer_body (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) return "";
  return subtree (the_et, buf->rp);
}

url
make_new_buffer () {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    if (is_nil (search_buffer (name))) {
      set_buffer_tree (name, tree (DOCUMENT));
      return name;
    }
    else i++;
  }
}

bool
buffer_has_name (url name) {
  return !is_scratch (name);
}

/******************************************************************************
* Creating and destroying buffers
******************************************************************************/

void
create_buffer (url name, tree doc) {
  if (!is_nil (search_buffer (name))) return;
  set_buffer_tree (name, doc);
}

url
create_buffer () {
  url name= make_new_buffer ();
  switch_to_buffer (name);
  return name;
}

void
revert_buffer () {
  tm_buffer buf= get_buffer ();
  web_cache_invalidate (buf->buf->name);
  tree doc= load_tree (buf->buf->name, buf->buf->fm);
  if (doc == "error") set_message ("Error: file not found", "revert buffer");
  else set_buffer_tree (buf->buf->name, doc);
}

void
kill_buffer () {
  int i, nr;
  if (N(bufs) <= 1) get_server () -> quit();
  url name= get_this_buffer ();
  tm_buffer buf= get_buffer();
  for (nr=0; nr<N(bufs); nr++) if (buf == bufs[nr]) break;
  ASSERT (nr != N(bufs), "buffer not found");
  for (nr=N(bufs)-1; nr>=0; nr--) if (buf != bufs[nr]) break;
  ASSERT (nr >= 0, "no suitable new buffer");
  tm_buffer new_buf = bufs[nr];

  for (i=0; i<N(buf->vws); i++) {
    tm_view old_vw= buf->vws[i];
    if (old_vw->win != NULL) {
      tm_window win = old_vw->win;
      tm_view new_vw= get_passive_view (new_buf);
      detach_view (old_vw);
      attach_view (win, new_vw);
      if (get_view () == old_vw) set_view (new_vw);
    }
  }
  remove_buffer (name);
}

/******************************************************************************
* Switching to another buffer
******************************************************************************/

void
switch_to_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) {
    load_passive_buffer (name);
    buf= search_buffer (name);
    if (is_nil (buf)) return;
  }
  
  // cout << "Switching to buffer " << buf->buf->name << "\n";
  tm_window win    = get_window ();
  tm_view   old_vw = get_view ();
  tm_view   new_vw = get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
  set_view (new_vw);
  buf->buf->last_visit= texmacs_time ();
  tm_window nwin= new_vw->win;
  nwin->set_shrinking_factor (nwin->get_shrinking_factor ());
  // cout << "Switched to buffer " << buf->buf->name << "\n";
}

bool
switch_to_buffer (path p) {
  url name= get_name_buffer (p);
  if (!is_none (name)) switch_to_buffer (name);
  return !is_none (name);
}
