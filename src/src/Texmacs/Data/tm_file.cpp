
/******************************************************************************
* MODULE     : tm_file.cpp
* DESCRIPTION: Loading and saving TeXmacs files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_data.hpp"
#include "tm_buffer.hpp"
#include "file.hpp"
#include "convert.hpp"

/******************************************************************************
* Loading files
******************************************************************************/

tree
tm_data_rep::load_tree (url u, string fm) {
  string s, name= as_string (tail (u)), suf= suffix (name);
  string action= "load " * fm * " file";
  u= resolve (u);
  if (is_none (u) || load_string (u, s)) {
    set_message ("Error: file#" * name * "#not found", action);
    return "error";
  }
  if ((fm == "generic") || (fm == "help")) fm= get_format (s, suf);
  return generic_to_tree (s, fm * "-document");
}

void
tm_data_rep::load_buffer (url u, string fm, int where, bool autosave_flag) {
  // cout << "Load= " << u << ", " << fm << "\n";
  string name= as_string (tail (u));
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
	default: fatal_error ("Bad value for 'where'", "load_buffer");
	}
      }
    if (!no_bufs ())
      set_message ("Error: file#" * name * "#not found", action);
    return;
  }

  if (fm == "help") {
    tree doc= load_tree (u, fm);
    if (doc == "error") return;
    if (where == 1) new_buffer_in_new_window ("* Help *", doc);
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
  default: fatal_error ("Bad value for 'where'", "load_buffer");
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
* Saving files
******************************************************************************/

tree
tm_data_rep::make_document (tm_view vw, string fm) {
  tree body= vw->buf->t;
  if (fm == "html")
    body= vw->ed->exec_html (body);

  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  if (vw->buf->project != "")
    doc << compound ("project", vw->buf->project);
  if (vw->ed->get_style() != tree (TUPLE))
    doc << compound ("style", copy (vw->ed->get_style()));
  if (vw->buf->t != tree (DOCUMENT, ""))
    doc << compound ("body", body);
  if (N (vw->ed->get_init()) != 0)
    doc << compound ("initial", ((tree) vw->ed->get_init()));
  if (N (vw->ed->get_fin()) != 0)
    doc << compound ("final", ((tree) vw->ed->get_fin()));
  if (N (vw->buf->ref) != 0)
    doc << compound ("references", ((tree) vw->buf->ref));
  if (N (vw->buf->aux) != 0)
    doc << compound ("auxiliary", ((tree) vw->buf->aux));
  return doc;
}

void
tm_data_rep::save_buffer (url u, string fm) {
  string name= as_string (u);
  if (fm == "generic") {
    fm= suffix_to_format (suffix (u));
    if (fm == "generic") fm= "verbatim";
  }
  string action= "save " * fm * " file";
  u= resolve (u, "");
  if (is_none (u)) {
    set_message ("Error: can't save file#" * name, action);
    return;
  }

  tm_buffer buf= get_buffer ();
  if ((buf->read_only && (u == buf->name)) ||
      ((!is_none (buf->extra)) && (buf->name != "* Aux *"))) {
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
      set_message ("Error:#" * name * "#did not open", action);
    else {
      set_message ("saved " * name, action);
      if (fm == "texmacs") {
	set_name_buffer (u);
	pretend_save_buffer ();
	if (suffix (u) == "ts") style_clear_cache ();
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
      url name= glue (buf->name, "~");
      if (is_without_name (buf->name))
	name= "$TEXMACS_HOME_PATH/system/autosave.tm";
      if (N(buf->vws)!=0) {
	tree doc= make_document (buf->vws[0]);
	if (save_string (name, tree_to_texmacs (doc)))
	  set_message ("Error: " * as_string (name) * " did not open",
		       "save TeXmacs file");
	else {
	  set_message ("saved " * as_string (name), "save TeXmacs file");
	  buf->mark_undo_block ();
	  buf->need_autosave= false;
	  buf->last_autosave= buf->undo_depth- 1;
	}
      }
    }
  }
  delayed_autosave();
}

void
tm_data_rep::delayed_autosave () {
  display d= get_display();
  d->remove_all_delayed_messages (get_meta()->get_this(), "auto save");
  string s= as_string(eval ("(get-preference \"autosave\")"));
  int p;
  if (is_int(s)) p= as_int(s) * 1000;
  else p= 120000;
  if (p>0) {
    d->delayed_message (get_meta()->get_this(), "auto save", p);
  }
}

/******************************************************************************
* Other routines concerning loading and saving buffers
******************************************************************************/

bool
tm_data_rep::no_name () {
  tm_buffer buf= get_buffer ();
  return is_without_name (buf->name);
}

bool
tm_data_rep::help_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->name == "* Help *";
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
  buf->mark_undo_block ();
  buf->need_save= buf->need_autosave= false;
  buf->last_save= buf->last_autosave= buf->undo_depth- 1;
}
