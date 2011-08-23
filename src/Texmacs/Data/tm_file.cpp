
/******************************************************************************
* MODULE     : tm_file.cpp
* DESCRIPTION: Loading and saving TeXmacs files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_data.hpp"
#include "tm_buffer.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "merge_sort.hpp"
#include "drd_std.hpp"

/******************************************************************************
* Loading files
******************************************************************************/

static void
notify_recent_buffer (string name) {
  object a= call ("assoc-set!", null_object (), object ("0"), object (name));
  call ("learn-interactive", object ("recent-buffer"), a);
}

tree
tm_data_rep::load_tree (url u, string fm) {
  string s, suf= suffix (u);
  string action= "load " * fm * " file";
  u= resolve (u);
  set_file_focus (u);
  if (is_none (u) || load_string (u, s, false)) {
    tree vname= verbatim (as_string (u));
    set_message (concat ("Error: file ", vname, " not found"), action);
    return "error";
  }
  if ((fm == "generic") || (fm == "help")) fm= get_format (s, suf);
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return t;
}

void
tm_data_rep::load_buffer (url u, string fm, int where, bool autosave_flag) {
  // cout << "Load= " << u << ", " << fm << "\n";
  string name= as_string (tail (u));
  tree vname= verbatim (name);
  string action= "load " * fm * " file";
  if (fm == "generic")
    action= "load " * suffix_to_format (suffix (u)) * " file";

  url v= u;
  u= resolve (u);
  if (is_none (u)) {
    if ((fm == "generic") || (fm == "texmacs"))
      if (is_name (v) || (is_rooted_name (v) && is_rooted (v, "default"))) {
	tree doc (DOCUMENT,
		  compound ("style", "generic"),
		  compound ("body", tree (DOCUMENT, "")));
	switch (where) {
	case 0: new_buffer_in_this_window (v, doc); break;
	case 1: new_buffer_in_new_window (v, doc); break;
	case 2: new_buffer (v, doc); break;
	default: FAILED ("bad value for 'where'");
	}
      }
    if (!no_bufs ())
      set_message (concat ("Error: file ", vname, " not found"), action);
    return;
  }

  if (fm == "help") {
    extern string get_help_title (url name, tree t);
    tree doc= load_tree (u, fm);
    if (doc == "error") return;
    if (where == 1)
      new_buffer_in_new_window (get_help_title (u, doc), doc);
    set_help_buffer (u, doc);
    return;
  }

  v= u;
  if (autosave_flag) v= unglue (v, 1);
  int nr= find_buffer (v);
  tree doc= ((nr == -1)? load_tree (u, fm): tree (DOCUMENT));
  if (doc == "error") return;
  switch (where) {
  case 0: new_buffer_in_this_window (v, doc); break;
  case 1: new_buffer_in_new_window (v, doc); break;
  case 2: new_buffer (v, doc); break;
  default: FAILED ("bad value for 'where'");
  }
  nr= find_buffer (v);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    buf->fm= fm;
    if ((fm == "help") || is_rooted_web (v)) {
      tm_buffer buf= get_buffer ();
      buf->read_only= true;
    }
  }
  if ((fm == "generic") || (fm == "texmacs"))
    notify_recent_buffer (as_string (u));
}

tm_buffer
tm_data_rep::load_passive_buffer (url u) {
  int nr= find_buffer (u);
  if (nr != -1) return bufs[nr];
  load_buffer (u, "texmacs", 2, false);
  nr= find_buffer (u);
  if (nr != -1) return bufs[nr];
  return NULL;
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
  tree doc= extract_document (get_server() -> load_tree (name, "generic"));
  if (!is_func (doc, ERROR)) document_inclusions (name_s)= doc;
  return doc;
}

/******************************************************************************
* Get environment and drd of style files
******************************************************************************/

static hashmap<string,bool> style_busy (false);
static hashmap<string,tree> style_void (UNINIT);
static hashmap<tree,hashmap<string,tree> > style_cached (style_void);
static drd_info drd_void ("void");
static hashmap<tree,drd_info> drd_cached (drd_void);

bool
compute_env_and_drd (tree style) {
  ASSERT (is_tuple (style), "style tuple expected");
  bool busy= false;
  for (int i=0; i<N(style); i++)
    busy= busy || style_busy->contains (as_string (style[i]));
  hashmap<string,bool> old_busy= copy (style_busy);
  for (int i=0; i<N(style); i++)
    style_busy (as_string (style[i]))= true;

  //cout << "Get environment of " << style << INDENT << LF;
  hashmap<string,tree> H;
  drd_info drd ("none", std_drd);
  url none= url ("$PWD/none");
  hashmap<string,tree> lref;
  hashmap<string,tree> gref;
  hashmap<string,tree> laux;
  hashmap<string,tree> gaux;
  edit_env env (drd, none, lref, gref, laux, gaux);
  if (!busy) {
    tree t;
    bool ok;
    get_server () -> style_get_cache (style, H, t, ok);
    if (ok) {
      env->patch_env (H);
      ok= drd->set_locals (t);
      drd->set_environment (H);
    }
    if (!ok) {
      env->exec (tree (USE_PACKAGE, A (style)));
      env->read_env (H);
      drd->heuristic_init (H);
    }
    style_cached (style)= H;
    drd_cached (style)= drd;
  }
  //cout << UNINDENT << "Got environment of " << style << LF;

  style_busy= old_busy;
  return !busy;
}

hashmap<string,tree>
get_style_env (tree style) {
  if (style_cached->contains (style))
    return style_cached [style];
  else if (compute_env_and_drd (style))
    return style_cached [style];
  else {
    //cout << "Busy style: " << style << "\n";
    return hashmap<string,tree> ();
  }
}

drd_info
get_style_drd (tree style) {
  init_std_drd ();
  if (drd_cached->contains (style))
    return drd_cached [style];
  else if (compute_env_and_drd (style))
    return drd_cached [style];
  else {
    //cout << "Busy drd: " << style << "\n";
    return std_drd;
  }
}

tree
get_document_preamble (tree t) {
  if (is_atomic (t)) return "";
  else if (is_compound (t, "hide-preamble", 1)) return t[0];
  else if (is_compound (t, "show-preamble", 1)) return t[0];
  else {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree p= get_document_preamble (t[i]);
      if (p != "") return p;
    }
    return "";
  }
}

drd_info
get_document_drd (tree doc) {
  tree style= extract (doc, "style");
  if (extract (doc, "TeXmacs") == "") {
    if (the_drd->get_syntax (make_tree_label ("theorem")) != tree (UNINIT))
      return the_drd;
    style= tree (TUPLE, "generic");
  }
  //cout << "style= " << style << "\n";
  drd_info drd= get_style_drd (style);
  tree p= get_document_preamble (doc);
  if (p != "") {
    drd= drd_info ("preamble", drd);
    url none= url ("$PWD/none");
    hashmap<string,tree> lref;
    hashmap<string,tree> gref;
    hashmap<string,tree> laux;
    hashmap<string,tree> gaux;
    edit_env env (drd, none, lref, gref, laux, gaux);
    hashmap<string,tree> H;
    env->exec (tree (USE_PACKAGE, A (style)));
    env->exec (p);
    env->read_env (H);
    drd->heuristic_init (H);
  }
  return drd;
}

/******************************************************************************
* Saving files
******************************************************************************/

tree
tm_data_rep::make_document (tm_view vw, string fm) {
  tree body= subtree (the_et, vw->buf->rp);
  if (fm == "verbatim")
    body= vw->ed->exec_verbatim (body);
  if (fm == "html")
    body= vw->ed->exec_html (body);
  if (fm == "latex")
    body= vw->ed->exec_latex (body);

  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  object arg1 (vw->buf->name);
  object arg2 (body);
  tree links= as_tree (call ("get-link-locations", arg1, arg2));

  if (vw->buf->project != "")
    doc << compound ("project", vw->buf->project);
  if (vw->ed->get_style() != tree (TUPLE))
    doc << compound ("style", copy (vw->ed->get_style()));
  if (body != tree (DOCUMENT, ""))
    doc << compound ("body", body);
  if (N (vw->ed->get_init()) != 0)
    doc << compound ("initial", make_collection (vw->ed->get_init()));
  if (N (vw->ed->get_fin()) != 0)
    doc << compound ("final", make_collection (vw->ed->get_fin()));
  if (N (links) != 0)
    doc << compound ("links", links);
  if (vw->ed->get_save_aux()) {
    if (N (vw->buf->ref) != 0)
      doc << compound ("references", make_collection (vw->buf->ref));
    if (N (vw->buf->aux) != 0)
      doc << compound ("auxiliary", make_collection (vw->buf->aux));
  }
  return doc;
}

void
tm_data_rep::save_buffer (url u, string fm) {
  string name= as_string (u);
  tree vname= verbatim (name);
  if (fm == "generic") {
    fm= suffix_to_format (suffix (u));
    if (fm == "generic") fm= "verbatim";
  }
  string action= "save " * fm * " file";
  u= resolve (u, "");
  if (is_none (u)) {
    set_message (concat ("Error: can't save file ", vname), action);
    return;
  }

  tm_buffer buf= get_buffer ();
  if ((u == buf->name && buf->read_only) ||
      (u == buf->name && has_permission (u,"r") && !has_permission (u,"w")) ||
      (!is_none (buf->extra) && buf->name != "* Aux *")) {
    set_message ("Error: file is read only", action);
    return;
  }
  if ((fm == "texmacs") && (!buf->needs_to_be_saved ()))
    if (buf->name == u) {
      set_message ("No changes need to be saved", action);
      return;
    }

  tm_view vw= get_view ();
  tree doc= make_document (vw, fm);
  string s= tree_to_generic (doc, fm * "-document");
  if (s != "* error: unknown format *") {
    if (save_string (u, s))
      set_message (concat ("Error: ", vname, " did not open"), action);
    else {
      set_message (concat ("saved ", vname), action);
      if (fm == "texmacs") {
        if (no_name () && exists (get_name_buffer ()))
          remove (get_name_buffer ());
        set_name_buffer (u);
        pretend_save_buffer ();
        if (suffix (u) == "ts") style_clear_cache ();
        if ((fm == "generic") || (fm == "texmacs"))
          if (!no_name ())
            notify_recent_buffer (as_string (u));
      }
    }
  }
  else set_message ("Error: unknown format", action);
}

void
tm_data_rep::auto_save () {
  int i, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= bufs[i];
    if ((buf->needs_to_be_autosaved () && (!buf->read_only))) {
      url name= buf->name;
      tree vname= verbatim (as_string (name));
      if (!is_scratch (name))
	name= glue (buf->name, rescue_mode? "#": "~");
      if (N(buf->vws)!=0) {
	tree doc= make_document (buf->vws[0]);
        bool err= save_string (name, tree_to_texmacs (doc));
        if (!rescue_mode) {
          if (!err)
            call ("set-temporary-message",
                  "saved " * as_string (name), "save TeXmacs file", 2500);
	  else
            set_message (concat ("Error: ", vname, " did not open"),
                         "save TeXmacs file");
        }
      }
      if (!rescue_mode)
        for (int j=0; j<N(buf->vws); j++)
          buf->vws[j]->ed->notify_save (false);
    }
  }
  call ("delayed-auto-save");
}

/******************************************************************************
* Other routines concerning loading and saving buffers
******************************************************************************/

bool
tm_data_rep::no_name () {
  tm_buffer buf= get_buffer ();
  return is_scratch (buf->name);
}

bool
tm_data_rep::help_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->fm == "help";
}

bool
tm_data_rep::buffer_unsaved () {
  tm_buffer buf= get_buffer ();
  return buf->needs_to_be_saved ();
}

bool
tm_data_rep::exists_unsaved_buffer () {
  bool flag= false;
  int i, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= bufs[i];
    flag = flag || buf->needs_to_be_saved ();
  }
  return flag;
}

void
tm_data_rep::pretend_save_buffer () {
  tm_buffer buf= get_buffer ();
  for (int i=0; i<N(buf->vws); i++)
    buf->vws[i]->ed->notify_save ();
}
