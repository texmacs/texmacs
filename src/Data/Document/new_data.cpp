
/******************************************************************************
* MODULE     : new_data.cpp
* DESCRIPTION: Data attached to full buffers which are not necessarily
*              well represented by trees
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_data.hpp"
#include "convert.hpp"
#include "merge_sort.hpp"
#include "vars.hpp"

tree
attach_data (tree body, new_data data, bool no_aux) {
  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  if (data->project != "")
    doc << compound ("project", copy (data->project));
  if (data->style != tree (TUPLE))
    doc << compound ("style", copy (data->style));
  if (body != tree (DOCUMENT, ""))
    doc << compound ("body", body);
  if (N (data->init) != 0) {
    hashmap<string,tree> init= copy (data->init);
    init->reset (PAGE_SCREEN_WIDTH);
    init->reset (PAGE_SCREEN_HEIGHT);
    if (!init->contains ("no-zoom")) init->reset (ZOOM_FACTOR);
    init->reset ("full-screen-mode");
    doc << compound ("initial", make_collection (init));
  }
  if (N (data->fin) != 0)
    doc << compound ("final", make_collection (data->fin));
  if (N (data->att) != 0)
    doc << compound ("attachments", make_collection (data->att));
  if (!no_aux) {
    if (N (data->ref) != 0)
      doc << compound ("references", make_collection (data->ref));
    if (N (data->aux) != 0)
      doc << compound ("auxiliary", make_collection (data->aux));
  }
  //object arg1 (data->buffer_name);
  //object arg2 (body);
  //tree links= as_tree (call ("get-link-locations", arg1, arg2));
  //if (N (links) != 0)
  //  doc << compound ("links", links);
  return doc;  
}

tree
detach_data (tree doc, new_data& data) {
  data->project= extract (doc, "project");
  data->style  = extract (doc, "style");
  data->init   = hashmap<string,tree> (UNINIT, extract (doc, "initial"));
  data->fin    = hashmap<string,tree> (UNINIT, extract (doc, "final"));
  data->ref    = hashmap<string,tree> (UNINIT, extract (doc, "references"));
  data->aux    = hashmap<string,tree> (UNINIT, extract (doc, "auxiliary"));
  data->att    = hashmap<string,tree> (UNINIT, extract (doc, "attachments"));
  //tree links= extract (doc, "links");
  //if (N (links) != 0)
  //  (void) call ("register-link-locations", object (u), object (links));
  return extract (doc, "body");
}
