
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

void
tm_buffer_rep::attach_notifier () {
  if (notify) return;
  string id= as_string (buf->name, URL_UNIX);
  tree& st (subtree (the_et, rp));
  call ("buffer-initialize", id, st, buf->name);
  lns= link_repository (true);
  lns->insert_locus (id, st, "buffer-notify");
  notify= true;
}

bool
tm_buffer_rep::needs_to_be_saved () {
  if (buf->read_only) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (buf->read_only) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
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
      for (int i=0; i<N(buf->vws); i++)
        delete_view (abstract_view (buf->vws[i]));
      if (n == 1 && number_of_servers () == 0)
        get_server () -> quit ();
      for (int i=nr; i<n-1; i++)
        bufs[i]= bufs[i+1];
      bufs->resize (n-1);
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
get_current_buffer_safe () {
  url v= get_current_view_safe ();
  if (is_none (v)) return v;
  return concrete_view (v)->buf->buf->name;
}

url
path_to_buffer (path p) {
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
  notify_rename_before (name);
  buf->buf->name= new_name;
  buf->buf->master= new_name;
  array<url> vs= buffer_to_views (new_name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_change (THE_ENVIRONMENT);
  notify_rename_after (new_name);
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
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    tm_window win= concrete_window (view_to_window (vs[i]));
    if (win != NULL) {
      win->set_window_name (title);
      win->set_window_url (name);
    }
  }
}

/******************************************************************************
* Setting and getting the buffer tree contents
******************************************************************************/

void
set_buffer_data (url name, new_data data) {
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    view_to_editor (vs[i]) -> set_data (data);
    view_to_editor (vs[i]) -> init_update ();
  }
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
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_change (THE_ENVIRONMENT);
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
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> require_save ();
}

void
pretend_buffer_saved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_save ();
  set_last_save_buffer (name, last_modified (name));
}

void
pretend_buffer_autosaved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_save (false);
}

void
attach_buffer_notifier (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  buf->attach_notifier ();
}

/******************************************************************************
* Loading
******************************************************************************/

tree
attach_subformat (tree t, url u, string fm) {
  if (fm != "verbatim" && fm != "scheme" && fm != "cpp") return t;
  string s= suffix (u);
  if (s == "scm") fm= "scheme";
  if (s == "java") fm= "java";
  if (s == "scala") fm= "scala";
  if (s == "py")  fm= "python";
  if (s == "cpp" || s == "hpp" || s == "cc" || s == "hh") fm= "cpp";
  if (s == "mmx" || s == "mmh") fm= "mathemagix";
  if (s == "sce" || s == "sci") fm= "scilab";
  if (fm == "verbatim") return t;
  hashmap<string,tree> h (UNINIT, extract (t, "initial"));
  h (MODE)= "prog";
  h (PROG_LANGUAGE)= fm;
  return change_doc_attr (t, "initial", make_collection (h));
}

tree
import_loaded_tree (string s, url u, string fm) {
  set_file_focus (u);
  if (fm == "generic" && suffix (u) == "txt") fm= "verbatim";
  if (fm == "generic") fm= get_format (s, suffix (u));
  if (fm == "texmacs" && starts (s, "(document (TeXmacs")) fm= "stm";
  if (fm == "verbatim" && starts (s, "(document (TeXmacs")) fm= "stm";
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return attach_subformat (t, u, fm);
}

tree
import_tree (url u, string fm) {
  u= resolve (u, "fr");
  set_file_focus (u);
  string s;
  if (is_none (u) || load_string (u, s, false)) return "error";
  return import_loaded_tree (s, u, fm);
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

hashmap<string,tree> style_tree_cache ("");

tree
load_style_tree (string package) {
  if (style_tree_cache->contains (package))
    return style_tree_cache [package];
  url name= url_none ();
  url styp= "$TEXMACS_STYLE_PATH";
  if (ends (package, ".ts")) name= package;
  else name= styp * (package * ".ts");
  name= resolve (name);
  string doc_s;
  if (!load_string (name, doc_s, false)) {
    tree doc= texmacs_document_to_tree (doc_s);
    if (is_compound (doc)) doc= extract (doc, "body");
    style_tree_cache (package)= doc;
    return doc;
  }
  style_tree_cache (package)= "";
  return "";
}

tree
with_package_definitions (string package, tree body) {
  // FIXME: it would be more robust to execute the package
  // yet an alternative idea is to fetch the complete environment of
  // a style using get_style_env.
  tree w (WITH);
  tree doc= load_style_tree (package);
  if (!is_func (doc, DOCUMENT)) return body;
  for (int i=0; i<N(doc); i++)
    if (is_func (doc[i], ASSIGN, 2))
      w << doc[i][0] << doc[i][1];
  w << body;
  return w;
}

/******************************************************************************
* Saving
******************************************************************************/

bool
export_tree (tree doc, url u, string fm) {
  tree aux= doc;
  // NOTE: hook for encryption
  tree init= extract (aux, "initial");
  if (fm == "texmacs")
    for (int i=0; i<N(init); i++)
      if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == "encryption") {
	aux= as_tree (call ("tree-export-encrypted", u, aux));
	break;
      }
  // END hook
  if (fm == "generic") fm= "verbatim";
  string s= tree_to_generic (aux, fm * "-document");
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
  //if (fm == "latex")
  //body= vw->ed->exec_latex (body);

  vw->ed->get_data (vw->buf->data);
  tree doc= attach_data (body, vw->buf->data, !vw->ed->get_save_aux());

  if (fm == "latex")
    doc= change_doc_attr (doc, "view", as_string (abstract_view (vw)));

  object arg1 (vw->buf->buf->name);
  object arg2 (body);
  tree links= as_tree (call ("get-link-locations", arg1, arg2));
  if (N (links) != 0)
    doc << compound ("links", links);
  
  return export_tree (doc, dest, fm);
}

tree
latex_expand (tree doc, url name) {
  tm_view vw= concrete_view (get_recent_view (name));
  tree body= vw->ed->exec_latex (extract (doc, "body"));
  return change_doc_attr (doc, "body", body);
}

tree
latex_expand (tree doc) {
  tm_view vw= concrete_view (url (as_string (extract (doc, "view"))));
  tree body= vw->ed->exec_latex (extract (doc, "body"));
  doc= change_doc_attr (doc, "body", body);
  return remove_doc_attr (doc, "view");
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
  document_inclusions = hashmap<string,tree> ("");
}

void
reset_inclusion (url name) {
  string name_s= as_string (name);
  document_inclusions -> reset (name_s);
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
