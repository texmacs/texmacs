
/******************************************************************************
* MODULE     : new_view.cpp
* DESCRIPTION: View management
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
* Creating and destroying buffers
******************************************************************************/

void
create_buffer (url name, tree doc) {
  if (!is_nil (search_buffer (name))) return;
  set_buffer_tree (name, doc);
}

url
create_buffer () {
  url name= make_new_buffer ();
  switch_to_buffer (name);
  return name;
}

void
revert_buffer () {
  tm_buffer buf= get_buffer ();
  web_cache_invalidate (buf->buf->name);
  tree doc= load_tree (buf->buf->name, buf->buf->fm);
  if (doc == "error") set_message ("Error: file not found", "revert buffer");
  else set_buffer_tree (buf->buf->name, doc);
}

void
kill_buffer () {
  int i, nr;
  if (N(bufs) <= 1) get_server () -> quit();
  url name= get_this_buffer ();
  tm_buffer buf= get_buffer();
  for (nr=0; nr<N(bufs); nr++) if (buf == bufs[nr]) break;
  ASSERT (nr != N(bufs), "buffer not found");
  for (nr=N(bufs)-1; nr>=0; nr--) if (buf != bufs[nr]) break;
  ASSERT (nr >= 0, "no suitable new buffer");
  tm_buffer new_buf = bufs[nr];

  for (i=0; i<N(buf->vws); i++) {
    tm_view old_vw= buf->vws[i];
    if (old_vw->win != NULL) {
      tm_window win = old_vw->win;
      tm_view new_vw= get_passive_view (new_buf);
      detach_view (old_vw);
      attach_view (win, new_vw);
      if (get_view () == old_vw) set_view (new_vw);
    }
  }
  remove_buffer (name);
}

/******************************************************************************
* Switching to another buffer
******************************************************************************/

void
switch_to_buffer (url name) {
  tm_buffer buf= search_buffer (name);
  if (is_nil (buf)) {
    load_passive_buffer (name);
    buf= search_buffer (name);
    if (is_nil (buf)) return;
  }
  
  // cout << "Switching to buffer " << buf->buf->name << "\n";
  tm_window win    = get_window ();
  tm_view   old_vw = get_view ();
  tm_view   new_vw = get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
  set_view (new_vw);
  buf->buf->last_visit= texmacs_time ();
  tm_window nwin= new_vw->win;
  nwin->set_shrinking_factor (nwin->get_shrinking_factor ());
  // cout << "Switched to buffer " << buf->buf->name << "\n";
}

bool
switch_to_buffer (path p) {
  url name= get_name_buffer (p);
  if (!is_none (name)) switch_to_buffer (name);
  return !is_none (name);
}

/******************************************************************************
* Routines on collections of views
******************************************************************************/

bool
needs_to_be_saved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
needs_to_be_autosaved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}

void
set_master (array<tm_view> vws, url master) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->set_master (master);
}

void
set_title (array<tm_view> vws, string title, url name) {
  for (int i=0; i<N(vws); i++) {
    tm_view vw= vws[i];
    if (vw->win != NULL) {
      vw->win->set_window_name (title);
      vw->win->set_window_url (name);
    }
  }
}

void
pretend_saved (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->notify_save ();
}

void
set_data (array<tm_view> vws, new_data data) {
  for (int i=0; i<N(vws); i++)
    vws[i]->ed->set_data (data);
}

void
delete_views (array<tm_view> vws) {
  for (int i=0; i<N(vws); i++)
    delete_view (vws[i]);
}
