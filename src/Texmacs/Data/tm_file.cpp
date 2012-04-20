
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

tm_buffer
load_passive_buffer (url u) {
  tm_buffer buf= search_buffer (u);
  if (!is_nil (buf)) return buf;
  buffer_load (u);
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
