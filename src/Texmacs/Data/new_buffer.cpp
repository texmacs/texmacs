
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
* Check for changes in the buffer
******************************************************************************/

bool
tm_buffer_rep::needs_to_be_saved () {
  if (!buf->in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (!buf->in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}

/******************************************************************************
* Finding buffers
******************************************************************************/

url
get_all_buffers () {
  url u= url_none ();
  for (int i=N(bufs)-1; i>=0; i--)
    u= bufs[i]->buf->name | u;
  return u;
}

int
nr_bufs () {
  return N(bufs);
}

bool
no_bufs () {
  return N(bufs) == 0;
}

int
find_buffer (tm_buffer buf) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i] == buf)
      return i;
  return -1;
}

int
find_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return i;
  return -1;
}

int
find_buffer (url name) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->buf->name == name)
      return i;
  return -1;
}

/******************************************************************************
* Information attached to buffers
******************************************************************************/

string
new_menu_name (url u) {
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
    name= as_string (call ("tmfs-name", as_string (u)));

  int i, j;
  for (j=1; true; j++) {
    bool flag= true;
    string ret (name);
    if (j>1) ret= name * " (" * as_string (j) * ")";
    for (i=0; i<N(bufs); i++)
      if (bufs[i]->buf->abbr == ret) flag= false;
    if (flag) return ret;
  }
}

url
get_this_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->buf->name;
}

url
get_name_buffer () {
  tm_buffer buf= get_buffer ();
  if (!is_none (buf->buf->extra)) return buf->buf->extra;
  return buf->buf->name;
}

url
get_name_buffer (path p) {
  int nr= find_buffer (p);
  if (nr == -1) return url_none ();
  return bufs[nr]->buf->name;
}

void
set_name_buffer (url name, url new_name) {
  if (new_name == name) return;
  int nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf= bufs[nr];
  buf->buf->name= new_name;
  set_abbr_buffer (new_name, new_menu_name (new_name));
}

url
get_base_url_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return url_none ();
  return bufs[nr]->buf->base_name;
}

void
set_base_url_buffer (url name, url base_name) {
  int nr= find_buffer (name);
  if (nr == -1) return;
  bufs[nr]->buf->base_name= base_name;
}

string
get_abbr_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return "";
  else return bufs[nr]->buf->abbr;
}

void
set_abbr_buffer (url name, string abbr) {
  int nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf= bufs[nr];
  if (buf->buf->abbr == abbr) return;
  buf->buf->abbr= abbr;
  for (int i=0; i<N(buf->vws); i++) {
    tm_view vw2= buf->vws[i];
    if (vw2->win != NULL) {
      vw2->win->set_window_name (buf->buf->abbr);
      vw2->win->set_window_url (is_none (buf->buf->extra)?
                                buf->buf->name: buf->buf->extra);
    }
  }
}

bool
buffer_in_menu (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return false;
  else return bufs[nr]->buf->in_menu;
}

bool
is_aux_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return false;
  else return !is_none (bufs[nr]->buf->extra);
}

double
last_visited (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return (double) texmacs_time ();
  else return (double) bufs[nr]->buf->last_visit;
}

bool
buffer_modified (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return false;
  return bufs[nr]->needs_to_be_saved ();
}

void
set_buffer_tree (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr == -1) create_buffer (name, tree (DOCUMENT, ""));
  nr= find_buffer (name);
  tm_buffer buf= bufs[nr];
  assign (buf->rp, doc);
}

tree
get_buffer_tree (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return "";
  tm_buffer buf= bufs[nr];
  return subtree (the_et, buf->rp);
}

/******************************************************************************
* Creating and destroying buffers
******************************************************************************/

tm_buffer
create_buffer (url name) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  buf->buf->abbr= new_menu_name (name);
  bufs << buf;
  return buf;
}

tm_buffer
create_buffer (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= create_buffer (name);
  tree body= detach_data (doc, buf->data);
  set_document (buf->rp, body);
  if (buf->data->project != "") {
    url prj_name= head (name) * as_string (buf->data->project);
    buf->prj= load_passive_buffer (prj_name);
  }
  return buf;
}

url
create_buffer () {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    int nr= find_buffer (name);
    if (nr == -1) {
      new_buffer_in_this_window (name, tree (DOCUMENT));
      return name;
    }
    else i++;
  }
}

void
revert_buffer (url name, tree doc) {
  int i, nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf= bufs[nr];
  tree body= detach_data (doc, buf->data);
  if (N(buf->vws)==0) set_document (buf->rp, body);
  else for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    if (i==0) assign (vw->ed->rp, body);
    vw->ed->set_data (buf->data);
    vw->ed->notify_save ();
  }
}

void
revert_buffer () {
  tm_buffer buf= get_buffer ();
  web_cache_invalidate (buf->buf->name);
  tree doc= load_tree (buf->buf->name, buf->buf->fm);
  if (doc == "error") set_message ("Error: file not found", "revert buffer");
  else revert_buffer (buf->buf->name, doc);
}

void
delete_buffer (tm_buffer buf) {
  int nr= find_buffer (buf), n= N(bufs);
  if (nr >= 0) {
    for (int i=nr; i<(n-1); i++) bufs[i]= bufs[i+1];
    bufs->resize (n-1);
  }
  for (int i=0; i<N(buf->vws); i++)
    delete_view (buf->vws[i]);
  tm_delete (buf);
}

void
kill_buffer () {
  int i, nr;
  if (N(bufs) <= 1) get_server () -> quit();
  tm_buffer buf= get_buffer();
  for (nr=0; nr<N(bufs); nr++) if (buf == bufs[nr]) break;
  ASSERT (nr != N(bufs), "buffer not found");
  for (nr=0; nr<N(bufs); nr++) if (buf != bufs[nr]) break;
  ASSERT (nr != N(bufs), "no suitable new buffer");
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
  delete_buffer (buf);
}

/******************************************************************************
* Switching to another buffer
******************************************************************************/

void
switch_to_buffer (int nr) {
  // cout << "Switching to buffer " << nr << "\n";
  tm_window win    = get_window ();
  tm_buffer buf    = bufs[nr];
  tm_view   old_vw = get_view ();
  tm_view   new_vw = get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
  set_view (new_vw);
  buf->buf->last_visit= texmacs_time ();
  tm_window nwin= new_vw->win;
  nwin->set_shrinking_factor (nwin->get_shrinking_factor ());
  // cout << "Switched to buffer " << nr << "\n";
}

bool
switch_to_buffer (path p) {
  int nr= find_buffer (p);
  if (nr != -1) switch_to_buffer (nr);
  return nr != -1;
}

void
switch_to_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) {
    load_passive_buffer (name);
    nr= find_buffer (name);
  }
  if (nr != -1) switch_to_buffer (nr);
}

void
switch_to_active_buffer (url name) {
  // This function is a temporary hack for coq
  // Switching to buffers in other windows should be completely rewritten

  int nr= find_buffer (name);
  if (nr == -1) {
    load_passive_buffer (name);
    nr= find_buffer (name);
  }
  if (nr != -1) {
    int i;
    tm_buffer buf= bufs[nr];
    for (i=0; i<N(buf->vws); i++) // search active view
      if (buf->vws[i]->win != NULL) {
        tm_view vw= buf->vws[i];
        set_view (vw);
        buf->buf->last_visit= texmacs_time ();
        return;
      }
  }
  switch_to_buffer (name);
}

/******************************************************************************
* Auxiliary buffers and help buffers
******************************************************************************/

void
set_aux (string aux, url name) {
  int i, nr= find_buffer (aux);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    buf->buf->extra= name;
    buf->buf->base_name= name;
    if (starts (aux, "Help - ")) {
      buf->buf->fm= "help";
      buf->buf->read_only= true;
    }
    for (i=0; i<N(buf->vws); i++) {
      tm_view vw= buf->vws[i];
      vw->ed->set_base_name (name);
    }
  }
}

void
set_aux_buffer (string aux, url name, tree doc) {
  int nr= find_buffer (aux);
  if (nr == -1) create_buffer (aux, doc);
  else revert_buffer (aux, doc);
  nr= find_buffer (aux);
  if (nr != -1) {
    set_aux (aux, name);
    switch_to_buffer (nr);
  }
}

static string
get_doc_title (tree t) {
  if (is_atomic (t)) return "";
  if (is_compound (t, "doc-title") ||
      is_compound (t, "tmdoc-title") ||
      is_compound (t, "tmdoc-title*") ||
      is_compound (t, "tmweb-title"))
    return tree_to_verbatim (t[0]);
  else {
    for (int i=0; i<N(t); i++) {
      string r= get_doc_title (t[i]);
      if (r != "") return r;
    }
    return "";
  }
}

string
get_help_title (url name, tree t) {
  string s= get_doc_title (t);
  if (s == "") return "Help - " * as_string (tail (name));
  else return "Help - " * s;
}

void
set_help_buffer (url name, tree doc) {
  set_aux_buffer (get_help_title (name, doc), name, doc);
}
