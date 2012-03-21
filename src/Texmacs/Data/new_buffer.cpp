
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

void
remove_buffer (url name) {
  int nr, n= N(bufs);
  for (nr=0; nr<n; nr++)
    if (bufs[nr]->buf->name == name) {
      tm_buffer buf= bufs[nr];
      for (int i=nr; i<n-1; i++)
        bufs[i]= bufs[i+1];
      bufs->resize (n-1);
      delete_views (buf->vws);
      tm_delete (buf);
      return;
    }
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
  int i, n= N(bufs);
  for (i=0; i<n; i++)
    if (bufs[i]->buf->name == name)
      return bufs[i];
  return nil_buffer ();
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
