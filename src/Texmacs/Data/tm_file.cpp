
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
#include "new_style.hpp"

/******************************************************************************
* Loading files
******************************************************************************/

static void
notify_recent_buffer (string name) {
  if (ends (name, "~") || ends (name, "#")) name= name (0, N(name) - 1);
  object a= call ("assoc-set!", null_object (), object ("0"), object (name));
  call ("learn-interactive", object ("recent-buffer"), a);
}

tree
load_tree (url u, string fm) {
  string s, suf= suffix (u);
  string action= "load " * fm * " file";
  u= resolve (u);
  set_file_focus (u);
  if (is_none (u) || load_string (u, s, false)) {
    tree vname= verbatim (as_string (u));
    set_message (concat ("Error: file ", vname, " not found"), action);
    return "error";
  }
  if (fm == "texmacs" && starts (s, "(")) fm= "generic";
  if (fm == "generic" || fm == "help") fm= get_format (s, suf);
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return t;
}

void
load_buffer (url u, string fm, int where, bool autosave_flag) {
  // cout << "Load= " << u << ", " << fm << "\n";
  string name= as_string (tail (u));
  tree vname= verbatim (name);
  string action= "load " * fm * " file";
  if (fm == "generic")
    action= "load " * suffix_to_format (suffix (u)) * " file";
  
  url v= u;
  u= resolve (u);
  if (is_none (u)) {
    if (fm == "generic" || fm == "texmacs")
      if (is_name (v) || (is_rooted_name (v) && is_rooted (v, "default"))) {
        tree doc (DOCUMENT,
                  compound ("style", "generic"),
                  compound ("body", tree (DOCUMENT, "")));
        switch (where) {
          case 0: new_buffer_in_this_window (v, doc); break;
          case 1: new_buffer_in_new_window (v, doc); break;
          case 2: ::create_buffer (v, doc); break;
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
    case 2: ::create_buffer (v, doc); break;
    default: FAILED ("bad value for 'where'");
  }
  nr= find_buffer (v);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    if (autosave_flag && N(buf->vws) == 1 && buf->vws[0]->ed != NULL)
      buf->vws[0]->ed->require_save();
    buf->buf->fm= fm;
    if (fm == "help" || is_rooted_web (v)) {
      tm_buffer buf= get_buffer ();
      buf->buf->read_only= true;
    }
  }
  if (fm == "generic" || fm == "texmacs")
    notify_recent_buffer (as_string (u));
}

tm_buffer
load_passive_buffer (url u) {
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
  tree doc= extract_document (load_tree (name, "generic"));
  if (!is_func (doc, ERROR)) document_inclusions (name_s)= doc;
  return doc;
}

/******************************************************************************
* Saving files
******************************************************************************/

tree
make_document (tm_view vw, string fm) {
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

  return doc;
}

void
save_buffer (url u, string fm) {
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
  if ((u == buf->buf->name && buf->buf->read_only) ||
      (u == buf->buf->name &&
       has_permission (u,"r") && !has_permission (u,"w")) ||
      (buf->buf->base_name != buf->buf->name &&
       buf->buf->name != "* Aux *")) {
    set_message ("Error: file is read only", action);
    return;
  }
  if ((fm == "texmacs") && (!buf->needs_to_be_saved ()))
    if (buf->buf->name == u) {
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
        if (no_name () && exists (get_this_buffer ()))
          remove (get_this_buffer ());
        rename_buffer (buf->buf->name, u);
        pretend_save_buffer ();
        if (suffix (u) == "ts") get_server () -> style_clear_cache ();
        if ((fm == "generic") || (fm == "texmacs"))
          if (!no_name ())
            notify_recent_buffer (as_string (u));
      }
    }
  }
  else set_message ("Error: unknown format", action);
}

void
auto_save () {
  int i, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= bufs[i];
    if ((buf->needs_to_be_autosaved () && (!buf->buf->read_only))) {
      url name= buf->buf->name;
      tree vname= verbatim (as_string (name));
      if (!is_scratch (name))
        name= glue (buf->buf->name, rescue_mode? "#": "~");
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
no_name () {
  tm_buffer buf= get_buffer ();
  return is_scratch (buf->buf->name);
}

bool
help_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->buf->fm == "help";
}

bool
buffer_unsaved () {
  tm_buffer buf= get_buffer ();
  return buf->needs_to_be_saved ();
}

bool
exists_unsaved_buffer () {
  bool flag= false;
  int i, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= bufs[i];
    flag = flag || buf->needs_to_be_saved ();
  }
  return flag;
}

void
pretend_save_buffer () {
  tm_buffer buf= get_buffer ();
  for (int i=0; i<N(buf->vws); i++)
    buf->vws[i]->ed->notify_save ();
}
