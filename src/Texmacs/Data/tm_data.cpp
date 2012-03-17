
/******************************************************************************
* MODULE     : tm_data.cpp
* DESCRIPTION: Buffer management for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
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

url tm_init_buffer_file= url_none ();
url my_init_buffer_file= url_none ();

/******************************************************************************
* Low level view routines
******************************************************************************/

tm_view
new_view (url name) {
  // cout << "Creating new view\n";

  tm_buffer buf= create_buffer (name);
  editor    ed = new_editor (get_server () -> get_server (), buf);
  tm_view   vw = tm_new<tm_view_rep> (buf, ed);
  buf->vws << vw;
  ed->set_data (buf->data);

  tm_view temp_vw= get_view (false);
  set_view (vw);
  if (is_none (tm_init_buffer_file))
    tm_init_buffer_file= "$TEXMACS_PATH/progs/init-buffer.scm";
  if (is_none (my_init_buffer_file))
    my_init_buffer_file= "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm";
  if (exists (tm_init_buffer_file)) exec_file (tm_init_buffer_file);
  if (exists (my_init_buffer_file)) exec_file (my_init_buffer_file);
  set_view (temp_vw);

  // cout << "View created\n";
  return vw;
}

tm_view
get_passive_view (tm_buffer buf) {
  int i;
  for (i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->win == NULL)
      return buf->vws[i];
  return new_view (buf->buf->name);
}

void
delete_view (tm_view vw) {
  tm_buffer buf= vw->buf;
  int i, j, n= N(buf->vws);
  for (i=0; i<n; i++)
    if (buf->vws[i] == vw) {
      array<tm_view> a (n-1);
      for (j=0; j<n-1; j++)
	if (j<i) a[j]= buf->vws[j];
	else a[j]= buf->vws[j+1];
      buf->vws= a;
    }
  // tm_delete (vw);
  // FIXME: causes very annoying segfault;
  // recently introduced during reorganization
}

void
attach_view (tm_window win, tm_view vw) {
  // cout << "Attach view " << vw->buf->buf->name << "\n";
  vw->win= win;
  widget wid= win->wid;
  set_scrollable (wid, vw->ed);
  vw->ed->cvw= wid.rep;
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->resume ();
  win->set_window_name (vw->buf->buf->abbr);
  win->set_window_url (is_none (vw->buf->buf->extra)? vw->buf->buf->name: vw->buf->buf->extra);
  // cout << "View attached\n";
}

void
detach_view (tm_view vw) {
  // cout << "Detach view " << vw->buf->buf->name << "\n";
  tm_window win= vw->win;
  if (win == NULL) return;
  vw->win= NULL;
  widget wid= win->wid;
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->suspend ();
  set_scrollable (wid, glue_widget ());
  win->set_window_name ("TeXmacs");
  win->set_window_url (url_none ());
  // cout << "View detached\n";
}

/******************************************************************************
* Low level window routines
******************************************************************************/

class kill_window_command_rep: public command_rep {
public:
  inline kill_window_command_rep () {}
  inline void apply () { exec_delayed (scheme_cmd ("(safely-kill-window)")); }
  tm_ostream& print (tm_ostream& out) { return out << "kill window"; }
};

tm_window
new_window (bool map_flag, tree geom) {
  int mask= 0;
  if (get_preference ("header") == "on") mask += 1;
  if (get_preference ("main icon bar") == "on") mask += 2;
  if (get_preference ("mode dependent icons") == "on") mask += 4;
  if (get_preference ("focus dependent icons") == "on") mask += 8;
  if (get_preference ("user provided icons") == "on") mask += 16;
  if (get_preference ("status bar") == "on") mask += 32;
  command quit= tm_new<kill_window_command_rep> ();
  tm_window win= tm_new<tm_window_rep> (texmacs_widget (mask, quit), geom);
  if (map_flag) win->map ();
  return win;
}

bool
delete_view_from_window (tm_window win) {
  int i, j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= buf->vws[j];
      if (vw->win == win) {
	detach_view (vw);
	delete_view (vw);
	return true;
      }
    }
  }
  return false;
}

void
delete_window (tm_window win) {
  while (delete_view_from_window (win)) {}
  win->unmap ();
  destroy_window_widget (win->win);
  tm_delete (win);
}

/******************************************************************************
* Other subroutines
******************************************************************************/

void
new_buffer_in_this_window (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr != -1) switch_to_buffer (nr);
  else {
    (void) create_buffer (name, doc);
    switch_to_buffer (name);
  }
}

void
new_buffer_in_new_window (url name, tree doc, tree geom) {
  tm_window win= new_window (true, geom);
  tm_buffer buf= create_buffer (name, doc);
  tm_view   vw = get_passive_view (buf);
  attach_view (win, vw);
  set_view (vw);
  buf->buf->last_visit= texmacs_time ();
}

/******************************************************************************
* Exported routines
******************************************************************************/

url
open_window (tree geom) {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    int nr= find_buffer (name);
    if (nr == -1) {
      new_buffer_in_new_window (name, tree (DOCUMENT), geom);
      return name;
    }
    else i++;
  }
}

void
clone_window () {
  tm_window win= new_window ();
  tm_buffer buf= get_buffer ();
  tm_view   vw = get_passive_view (buf);
  attach_view (win, vw);
  set_view (vw);
  buf->buf->last_visit= texmacs_time ();
}

void
kill_window () {
  int i, j;
  tm_window win= get_window ();
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= buf->vws[j];
      if ((vw->win != NULL) && (vw->win != win)) {
	set_view (vw);
        vw->buf->buf->last_visit= texmacs_time ();
	delete_window (win);
	return;
      }
    }
  }
  if (number_of_servers () == 0) get_server () -> quit ();
  else delete_window (win);
}

void
kill_window_and_buffer () {
  if (N(bufs) <= 1) get_server () -> quit();
  int i;
  bool kill= true;
  tm_buffer buf= get_buffer();
  tm_window win= get_window ();
  for (i=0; i<N(buf->vws); i++) {
    tm_view old_vw= buf->vws[i];
    if (old_vw->win != win) kill= false;
  }
  kill_window ();
  if (kill) delete_buffer (buf);
}

/******************************************************************************
* Projects
******************************************************************************/

void
project_attach (string prj_name) {
  int i;
  tm_buffer buf= get_buffer ();
  buf->data->project= prj_name;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    vw->ed->notify_change (THE_DECORATIONS);
    vw->ed->require_save ();
  }
  if (prj_name == "") buf->prj= NULL;
  else {
    url full_name= head (buf->buf->name) * prj_name;
    buf->prj= load_passive_buffer (full_name);
  }
}

bool
project_attached () {
  tm_buffer buf= get_buffer ();
  return buf->data->project != "";
}

url
project_get () {
  tm_buffer buf= get_buffer ();
  if (buf->data->project == "") return url_none ();
  return buf->prj->buf->name;
}

/******************************************************************************
* Window management
******************************************************************************/

static int  last_window= 1;
static path the_windows;

int
create_window_id () {
  the_windows= path (last_window, the_windows);
  return last_window++;
}

static path
reset (path p, int i) {
  if (is_nil (p)) return p;
  else if (p->item == i) return p->next;
  else return path (p->item, reset (p->next, i));
}

void
destroy_window_id (int i) {
  the_windows= reset (the_windows, i);
}

int
window_current () {
  tm_window win= get_window ();
  return win->id;
}

path
windows_list () {
  return the_windows;
}

path
buffer_to_windows (url name) {
  path p;
  int nr= find_buffer (name);
  if (nr == -1) return path ();
  tm_buffer buf= bufs[nr];
  for (int i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->win != NULL)
      p= path (buf->vws[i]->win->id, p);
  return p;
}

url
window_to_buffer (int id) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (bufs[i]->vws[j]->win->id == id)
	  return bufs[i]->buf->name;
  return url_none ();
}

tm_view
window_find_view (int id) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (bufs[i]->vws[j]->win->id == id)
	  return bufs[i]->vws[j];
  return NULL;
}

void
window_set_buffer (int id, url name) {
  tm_view old_vw= window_find_view (id);
  if (old_vw == NULL || old_vw->buf->buf->name == name) return;
  tm_window win= old_vw->win;
  int nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf   = bufs[nr];
  tm_view   new_vw= get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
}

void
window_focus (int id) {
  if (id == window_current ()) return;
  tm_view vw= window_find_view (id);
  if (vw == NULL) return;
  set_view (vw);
  vw->buf->buf->last_visit= texmacs_time ();
}
