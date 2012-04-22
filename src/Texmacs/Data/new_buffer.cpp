
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
#include "merge_sort.hpp"

array<tm_buffer> bufs;

string propose_title (string old_title, url u, tree doc);

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
* Manipulation of buffer list
******************************************************************************/

void
insert_buffer (url name) {
  if (is_none (name)) return;
  if (!is_nil (concrete_buffer (name))) return;
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  bufs << buf;
}

void
remove_buffer (tm_buffer buf) {
  int nr, n= N(bufs);
  for (nr=0; nr<n; nr++)
    if (bufs[nr] == buf) {
      if (n == 1 && number_of_servers () == 0)
        get_server () -> quit ();
      for (int i=nr; i<n-1; i++)
        bufs[i]= bufs[i+1];
      bufs->resize (n-1);
      delete_views (buf->vws);
      tm_delete (buf);
      return;
    }
}

void
remove_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (!is_nil (buf)) remove_buffer (buf);
}

int
number_buffers () {
  return N(bufs);
}

array<url>
get_all_buffers () {
  array<url> r;
  for (int i=N(bufs)-1; i>=0; i--)
    r << bufs[i]->buf->name;
  return r;
}

tm_buffer
concrete_buffer (url name) {
  int i, n= N(bufs);
  for (i=0; i<n; i++)
    if (bufs[i]->buf->name == name)
      return bufs[i];
  return nil_buffer ();
}

tm_buffer
concrete_buffer_insist (url u) {
  tm_buffer buf= concrete_buffer (u);
  if (!is_nil (buf)) return buf;
  buffer_load (u);
  return concrete_buffer (u);
}

/******************************************************************************
* Buffer names
******************************************************************************/

url
get_current_buffer () {
  tm_view vw= concrete_view (get_current_view ());
  return vw->buf->buf->name;
}

url
abstract_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return bufs[i]->buf->name;
  return url_none ();
}

void
rename_buffer (url name, url new_name) {
  if (new_name == name || is_nil (concrete_buffer (name))) return;
  kill_buffer (new_name);
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  buf->buf->name= new_name;
  buf->buf->master= new_name;
  tree doc= subtree (the_et, buf->rp);
  string title= propose_title (buf->buf->title, new_name, doc);
  set_title_buffer (new_name, title);
}

url
make_new_buffer () {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    if (is_nil (concrete_buffer (name))) {
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
* Buffer title
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

string
get_title_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  return buf->buf->title;
}

void
set_title_buffer (url name, string title) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->title == title) return;
  buf->buf->title= title;
  set_title (buf->vws, buf->buf->title, buf->buf->name);
}

/******************************************************************************
* Setting and getting the buffer tree contents
******************************************************************************/

void
set_buffer_data (url name, new_data data) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  set_data (buf->vws, data);
}

void
set_buffer_tree (url name, tree doc) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) {
    insert_buffer (name);
    buf= concrete_buffer (name);
    tree body= detach_data (doc, buf->data);
    set_document (buf->rp, body);
    buf->buf->title= propose_title (buf->buf->title, name, body);
    if (buf->data->project != "") {
      url prj_name= head (name) * as_string (buf->data->project);
      buf->prj= concrete_buffer_insist (prj_name);
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
      buf->prj= concrete_buffer_insist (prj_name);
    }
  }
  pretend_buffer_saved (name);
}

tree
get_buffer_tree (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  tree body= subtree (the_et, buf->rp);
  return attach_data (body, buf->data, true);
}

void
set_buffer_body (url name, tree body) {
  tm_buffer buf= concrete_buffer (name);
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
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  return subtree (the_et, buf->rp);
}

/******************************************************************************
* Further information attached to buffers
******************************************************************************/

url
get_master_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return url_none ();
  return buf->buf->master;
}

void
set_master_buffer (url name, url master) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->master == master) return;
  buf->buf->master= master;
  set_master (buf->vws, master);
}

void
set_last_save_buffer (url name, int t) {
  tm_buffer buf= concrete_buffer (name);
  if (!is_nil (buf)) buf->buf->last_save= t;
  //cout << "Set last save " << name << " -> " << t << "\n";
}

int
get_last_save_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) {
    //cout << "Get last save " << name << " -> *\n";
    return - (int) (((unsigned int) (-1)) >> 1);
  }
  //cout << "Get last save " << name << " -> " << buf->buf->last_save << "\n";
  return (int) buf->buf->last_save;
}

bool
is_aux_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->buf->master != buf->buf->name;
}

double
last_visited (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return (double) texmacs_time ();
  return (double) buf->buf->last_visit;
}

bool
buffer_modified (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->needs_to_be_saved ();
}

bool
buffer_modified_since_autosave (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->needs_to_be_autosaved ();
}

void
pretend_buffer_modified (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  pretend_modified (buf->vws);
}

void
pretend_buffer_saved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  pretend_saved (buf->vws);
  set_last_save_buffer (name, last_modified (name));
}

void
pretend_buffer_autosaved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  pretend_autosaved (buf->vws);
}

/******************************************************************************
* Loading
******************************************************************************/

tree
attach_subformat (tree t, url u, string fm) {
  if (fm != "verbatim") return t;
  string s= suffix (u);
  if (s == "scm") fm= "scheme";
  if (s == "cpp" || s == "hpp" || s == "cc" || s == "hh") fm= "cpp";
  if (s == "mmx" || s == "mmh") fm= "mathemagix";
  if (fm == "verbatim") return t;
  hashmap<string,tree> h (UNINIT, extract (t, "initial"));
  h (MODE)= "prog";
  h (PROG_LANGUAGE)= fm;
  return change_doc_attr (t, "initial", make_collection (h));
}

tree
import_tree (url u, string fm) {
  u= resolve (u, "fr");
  set_file_focus (u);
  string s;
  if (is_none (u) || load_string (u, s, false)) return "error";
  if (fm == "generic") fm= get_format (s, suffix (u));
  if (fm == "texmacs" && starts (s, "(document (TeXmacs")) fm= "stm";
  if (fm == "verbatim" && starts (s, "(document (TeXmacs")) fm= "stm";
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return attach_subformat (t, u, fm);
}

bool
buffer_import (url name, url src, string fm) {
  tree t= import_tree (src, fm);
  if (t == "error") return true;
  set_buffer_tree (name, t);
  return false;
}

bool
buffer_load (url name) {
  string fm= file_format (name);
  return buffer_import (name, name, fm);
}

/******************************************************************************
* Saving
******************************************************************************/

bool
export_tree (tree doc, url u, string fm) {
  if (fm == "generic") fm= "verbatim";
  string s= tree_to_generic (doc, fm * "-document");
  if (s == "* error: unknown format *") return true;
  return save_string (u, s);
}

bool
buffer_export (url name, url dest, string fm) {
  tm_view vw= concrete_view (get_recent_view (name));
  ASSERT (vw != NULL, "view expected");

  if (fm == "postscript" || fm == "pdf") {
    int old_stamp= last_modified (dest, false);
    vw->ed->print_to_file (dest);
    int new_stamp= last_modified (dest, false);
    return new_stamp <= old_stamp;
  }

  tree body= subtree (the_et, vw->buf->rp);
  if (fm == "verbatim")
    body= vw->ed->exec_verbatim (body);
  if (fm == "html")
    body= vw->ed->exec_html (body);
  if (fm == "latex")
    body= vw->ed->exec_latex (body);

  vw->ed->get_data (vw->buf->data);
  tree doc= attach_data (body, vw->buf->data, !vw->ed->get_save_aux());

  object arg1 (vw->buf->buf->name);
  object arg2 (body);
  tree links= as_tree (call ("get-link-locations", arg1, arg2));
  if (N (links) != 0)
    doc << compound ("links", links);
  
  return export_tree (doc, dest, fm);
}

bool
buffer_save (url name) {
  string fm= file_format (name);
  if (fm == "generic") fm= "verbatim";
  bool r= buffer_export (name, name, fm);
  if (!r) pretend_buffer_saved (name);
  return r;
}

/******************************************************************************
* Loading inclusions
******************************************************************************/

static hashmap<string,tree> document_inclusions ("");

void
reset_inclusions () {
  document_inclusions= hashmap<string,tree> ("");
}

void
reset_inclusion (url name) {
  string name_s= as_string (name);
  document_inclusions->reset (name_s);
}

tree
load_inclusion (url name) {
  // url name= relative (base_file_name, file_name);
  string name_s= as_string (name);
  if (document_inclusions->contains (name_s))
    return document_inclusions [name_s];
  tree doc= extract_document (import_tree (name, "generic"));
  if (!is_func (doc, ERROR)) document_inclusions (name_s)= doc;
  return doc;
}
