
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
project_attached () {
  tm_buffer buf= concrete_buffer (get_current_buffer ());
  return buf->data->project != "";
}

url
project_get () {
  tm_buffer buf= concrete_buffer (get_current_buffer ());
  if (buf->data->project == "") return url_none ();
  return buf->prj->buf->name;
}
