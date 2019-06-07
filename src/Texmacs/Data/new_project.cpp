
/******************************************************************************
* MODULE     : new_project.cpp
* DESCRIPTION: Project management
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

/******************************************************************************
* Projects
******************************************************************************/

void
project_attach (string prj_name) {
  url name= get_current_buffer ();
  tm_buffer buf= concrete_buffer (name);
  buf->data->project= prj_name;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    editor ed= view_to_editor (vs[i]);
    ed->init_update ();
    ed->notify_change (THE_DECORATIONS);
    ed->require_save ();
  }
  if (prj_name == "") buf->prj= NULL;
  else {
    url full_name= head (buf->buf->name) * prj_name;
    buf->prj= concrete_buffer_insist (full_name);
  }
}

bool
is_implicit_project (tm_buffer buf) {
  if (suffix (buf->buf->name) == "tp") return true;
  array<url> vs= buffer_to_views (buf->buf->name);
  for (int i=0; i<N(vs); i++) {
    editor ed= view_to_editor (vs[i]);
    if (ed->get_init_string ("project-flag") == "true") return true;
    if (ed->get_init_string ("project-flag") == "false") return false;
  }
  return buf->data->init ["project-flag"] == "true";
}

bool
project_attached () {
  tm_buffer buf= concrete_buffer (get_current_buffer ());
  if (is_implicit_project (buf)) return true;
  return buf->data->project != "";
}

url
project_get () {
  url name = get_current_buffer ();
  tm_buffer buf= concrete_buffer (name);
  if (is_implicit_project (buf)) return buf->buf->name;
  if (buf->data->project == "") return url_none ();
  url prj_name = head (name) * as_string (buf->data->project);
  buf->prj = concrete_buffer_insist (prj_name);
  return buf->prj->buf->name;
}
