
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

tree attach_subformat (tree t, url u, string fm);

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
  if (fm == "generic") fm= get_format (s, suf);
  if (fm == "texmacs" && starts (s, "(document (TeXmacs")) fm= "stm";
  if (fm == "verbatim" && starts (s, "(document (TeXmacs")) fm= "stm";
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return attach_subformat (t, u, fm);
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
          case 2: create_buffer (v, doc); break;
          default: FAILED ("bad value for 'where'");
        }
      }
    if (number_buffers () != 0)
      set_message (concat ("Error: file ", vname, " not found"), action);
    return;
  }
  
  v= u;
  if (autosave_flag) v= unglue (v, 1);
  tm_buffer buf= search_buffer (v);
  tree doc= (is_nil (buf)? load_tree (u, fm): tree (DOCUMENT));
  if (doc == "error") return;
  switch (where) {
    case 0: new_buffer_in_this_window (v, doc); break;
    case 1: new_buffer_in_new_window (v, doc); break;
    case 2: create_buffer (v, doc); break;
    default: FAILED ("bad value for 'where'");
  }
  buf= search_buffer (v);
  if (!is_nil (buf)) {
    set_last_save_buffer (v, last_modified (v));
    if (autosave_flag && N(buf->vws) == 1 && buf->vws[0]->ed != NULL)
      buf->vws[0]->ed->require_save();
    buf->buf->fm= fm;
  }
  if (fm == "generic" || fm == "texmacs")
    notify_recent_buffer (as_string (u));
}

tm_buffer
load_passive_buffer (url u) {
  tm_buffer buf= search_buffer (u);
  if (!is_nil (buf)) return buf;
  load_buffer (u, "texmacs", 2, false);
  return search_buffer (u);
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
