
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
array<tm_buffer> bufs;

/******************************************************************************
* Low level functions for maintaining the buffer menu
******************************************************************************/

int
find_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return i;
  return -1;
}

int
find_buffer (url name) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->buf->name == name)
      return i;
  return -1;
}

string
new_menu_name (url u) {
  string name= as_string (tail (u));
  if (starts (name, "no_name_") && ends (name, ".tm")) {
    string no_name= translate ("No name");
    for (int i=0; i<N(no_name); i++)
      if (((unsigned char) (no_name[i])) >= (unsigned char) 128)
	{ no_name= "No name"; break; }
    name= no_name * " [" * name (8, N(name) - 3) * "]";
  }
  if ((name == "") || (name == "."))
    name= as_string (tail (u * url_parent ()));
  if ((name == "") || (name == "."))
    name= as_string (u);
  if (is_rooted_tmfs (u))
    name= as_string (call ("tmfs-name", as_string (u)));

  int i, j;
  for (j=1; true; j++) {
    bool flag= true;
    string ret (name);
    if (j>1) ret= name * " (" * as_string (j) * ")";
    for (i=0; i<N(bufs); i++)
      if (bufs[i]->buf->abbr == ret) flag= false;
    if (flag) return ret;
  }
}

static void
menu_append_buffer (string& s, tm_buffer buf) {
  if (buf->buf->in_menu) {
    string name= copy (buf->buf->abbr);
    if (buf->needs_to_be_saved ()) name << " *"; 
    string mname= scm_quote (name);
    if (!starts (name, "Help")) mname= "(verbatim " * mname * ")";
    s << " (" << mname;
    s << " (switch-to-buffer " * scm_quote (as_string (buf->buf->name)) * "))";
  }
}

object
get_buffer_menu () {
  int i, count;
  bool two_types= false;;
  string s ("(menu-dynamic");
  for (i=0, count=0; i<N(bufs); i++) {
    if (is_none (bufs[i]->buf->extra) == is_none (bufs[0]->buf->extra)) {
      menu_append_buffer (s, bufs[i]);
      count++;
    }
    else if (bufs[i]->buf->in_menu) two_types= true;
    if (count == 10) break;
  }
  if (two_types) {
    s << " ---";
    for (i=0, count=0; i<N(bufs); i++) {
      if (is_none (bufs[i]->buf->extra) != is_none (bufs[0]->buf->extra)) {
	menu_append_buffer (s, bufs[i]);
	count++;
      }
      if (count == 10) break;
    }
  }
  s << ")";
  return eval (s);
}

bool
buffer_in_menu (url u, bool flag) {
  int nr= find_buffer (u);
  if (nr == -1) return false;
  tm_buffer buf= bufs[nr];
  bool old= buf->buf->in_menu;
  buf->buf->in_menu= flag;
  return old;
}

void
menu_insert_buffer (tm_buffer buf) {
  // bufs << ((pointer) buf); // causes compilation error
  bufs << buf; // WARNING: that one compile, what was the use of the cast?
}

void
menu_delete_buffer (tm_buffer buf) {
  int i, nr, n=N(bufs);
  for (nr=0; nr<n; nr++)
    if (bufs[nr] == ((pointer) buf)) break;
  if (nr==N(bufs)) return;

  for (i=nr; i<(n-1); i++) bufs[i]= bufs[i+1];
  bufs->resize (n-1);
}

void
menu_focus_buffer (tm_buffer buf) {
  int i, nr;
  for (nr=0; nr<N(bufs); nr++)
    if (bufs[nr] == ((pointer) buf)) break;
  if (nr==N(bufs)) return;

  for (i=nr; i>=1; i--) bufs[i]= bufs[i-1];
  bufs[0]= buf;
}

/******************************************************************************
* Low level buffer routines
******************************************************************************/

tm_buffer
create_buffer (url name) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  buf->buf->abbr= new_menu_name (name);
  menu_insert_buffer (buf);
  return buf;
}

tm_buffer
create_buffer (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= create_buffer (name);
  tree body= detach_data (doc, buf->data);
  set_document (buf->rp, body);
  if (buf->data->project != "") {
    url prj_name= head (name) * as_string (buf->data->project);
    buf->prj= load_passive_buffer (prj_name);
  }
  return buf;
}

void
revert_buffer (url name, tree doc) {
  int i, nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf= bufs[nr];
  tree body= detach_data (doc, buf->data);
  if (N(buf->vws)==0) set_document (buf->rp, body);
  else for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    if (i==0) assign (vw->ed->rp, body);
    vw->ed->set_data (buf->data);
    vw->ed->notify_save ();
  }
}

void
delete_buffer (tm_buffer buf) {
  int i;
  menu_delete_buffer (buf);
  for (i=0; i<N(buf->vws); i++)
    delete_view (buf->vws[i]);
  tm_delete (buf);
}

void
set_name_buffer (url name) {
  tm_buffer buf= get_buffer ();
  if (buf->buf->name == name) return;
  buf->buf->name= name;
  set_abbr_buffer (new_menu_name (name));
}

string
get_abbr_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->buf->abbr;
}

void
set_abbr_buffer (string abbr) {
  int i;
  tm_buffer buf= get_buffer ();
  if (buf->buf->abbr == abbr) return;
  buf->buf->abbr= abbr;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw2= buf->vws[i];
    if (vw2->win != NULL) {
      vw2->win->set_window_name (buf->buf->abbr);
      vw2->win->set_window_url (is_none (buf->buf->extra)? buf->buf->name: buf->buf->extra);
    }
  }
}

url
get_name_buffer () {
  tm_buffer buf= get_buffer ();
  if (!is_none (buf->buf->extra)) return buf->buf->extra;
  return buf->buf->name;
}

url
get_name_buffer (path p) {
  int nr= find_buffer (p);
  if (nr == -1) return url_none ();
  return bufs[nr]->buf->name;
}

url
get_all_buffers () {
  url u= url_none ();
  for (int i=N(bufs)-1; i>=0; i--)
    u= bufs[i]->buf->name | u;
  return u;
}

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
  menu_focus_buffer (buf);
}

/******************************************************************************
* Exported routines
******************************************************************************/

int
nr_bufs () {
  return N(bufs);
}

tm_buffer
get_buf (int i) {
  return (tm_buffer) bufs[i];
}

tm_buffer
get_buf (path p) {
  int nr= find_buffer (p);
  if (nr >= 0) return bufs[nr];
  else return NULL;
}

url
create_buffer () {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    int nr= find_buffer (name);
    if (nr == -1) {
      new_buffer_in_this_window (name, tree (DOCUMENT));
      return name;
    }
    else i++;
  }
}

void
switch_to_buffer (int nr) {
  // cout << "Switching to buffer " << nr << "\n";
  tm_window win    = get_window ();
  tm_buffer buf    = bufs[nr];
  tm_view   old_vw = get_view ();
  tm_view   new_vw = get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
  set_view (new_vw);
  menu_focus_buffer (buf);
  tm_window nwin= new_vw->win;
  nwin->set_shrinking_factor (nwin->get_shrinking_factor ());
  // cout << "Switched to buffer " << nr << "\n";
}

bool
switch_to_buffer (path p) {
  int nr= find_buffer (p);
  if (nr != -1) switch_to_buffer (nr);
  return nr != -1;
}

void
switch_to_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) {
    load_passive_buffer (name);
    nr= find_buffer (name);
  }
  if (nr != -1) switch_to_buffer (nr);
}

void
switch_to_active_buffer (url name) {
  // This function is a temporary hack for coq
  // Switching to buffers in other windows should be completely rewritten

  int nr= find_buffer (name);
  if (nr == -1) {
    load_passive_buffer (name);
    nr= find_buffer (name);
  }
  if (nr != -1) {
    int i;
    tm_buffer buf= bufs[nr];
    for (i=0; i<N(buf->vws); i++) // search active view
      if (buf->vws[i]->win != NULL) {
        tm_view vw= buf->vws[i];
        set_view (vw);
        menu_focus_buffer (buf);
        return;
      }
  }
  switch_to_buffer (name);
}

void
revert_buffer () {
  tm_buffer buf= get_buffer ();
  web_cache_invalidate (buf->buf->name);
  tree doc= load_tree (buf->buf->name, buf->buf->fm);
  if (doc == "error") set_message ("Error: file not found", "revert buffer");
  else revert_buffer (buf->buf->name, doc);
}

void
kill_buffer () {
  int i, nr;
  if (N(bufs) <= 1) get_server () -> quit();
  tm_buffer buf= get_buffer();
  for (nr=0; nr<N(bufs); nr++) if (buf == bufs[nr]) break;
  ASSERT (nr != N(bufs), "buffer not found");
  for (nr=0; nr<N(bufs); nr++) if (buf != bufs[nr]) break;
  ASSERT (nr != N(bufs), "no suitable new buffer");
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
  delete_buffer (buf);
}

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
  menu_focus_buffer (buf);
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
	menu_focus_buffer (vw->buf);
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

bool
no_bufs () {
  return N(bufs) == 0;
}

void
set_aux (string aux, url name) {
  int i, nr= find_buffer (aux);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    buf->buf->extra= name;
    if (starts (aux, "Help - ")) {
      buf->buf->fm= "help";
      buf->buf->read_only= true;
    }
    for (i=0; i<N(buf->vws); i++) {
      tm_view vw= buf->vws[i];
      vw->ed->set_base_name (name);
    }
  }
}

void
set_aux_buffer (string aux, url name, tree doc) {
  int nr= find_buffer (aux);
  if (nr == -1) create_buffer (aux, doc);
  else revert_buffer (aux, doc);
  nr= find_buffer (aux);
  if (nr != -1) {
    set_aux (aux, name);
    switch_to_buffer (nr);
  }
}

static string
get_doc_title (tree t) {
  if (is_atomic (t)) return "";
  if (is_compound (t, "doc-title") ||
      is_compound (t, "tmdoc-title") ||
      is_compound (t, "tmdoc-title*") ||
      is_compound (t, "tmweb-title"))
    return tree_to_verbatim (t[0]);
  else {
    for (int i=0; i<N(t); i++) {
      string r= get_doc_title (t[i]);
      if (r != "") return r;
    }
    return "";
  }
}

string
get_help_title (url name, tree t) {
  string s= get_doc_title (t);
  if (s == "") return "Help - " * as_string (tail (name));
  else return "Help - " * s;
}

void
set_help_buffer (url name, tree doc) {
  set_aux_buffer (get_help_title (name, doc), name, doc);
}

void
set_buffer_tree (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr == -1) create_buffer (name, tree (DOCUMENT));
  nr= find_buffer (name);
  tm_buffer buf= bufs[nr];
  assign (buf->rp, doc);
}

tree
get_buffer_tree (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return "";
  tm_buffer buf= bufs[nr];
  return subtree (the_et, buf->rp);
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

object
get_project_buffer_menu () {
  tm_buffer buf= get_buffer ();
  if (buf->prj == NULL) return eval ("(menu-dynamic)");
  string s ("(menu-dynamic ");
  s << "(" << scm_quote (buf->prj->buf->abbr) << " ";
  s << "(switch-to-buffer " * scm_quote (as_string (buf->prj->buf->name)) * "))";

  tree t= subtree (the_et, buf->prj->rp);
  int i, j, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], INCLUDE, 1) && is_atomic (t[i][0])) {
      string name= as_string (head (buf->prj->buf->name) * as_string (t[i][0]));
      for (j=N(name)-1; j>=0; j--)
	if (name[j]=='/') break;
      s << " ((verbatim " << scm_quote (name (j+1, N(name))) << ") ";
      s << "(switch-to-buffer " * scm_quote (name) * "))";
    }

  s << ")";
  return eval (s);
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
  menu_focus_buffer (vw->buf);
}

/******************************************************************************
* Check for changes in the buffer
******************************************************************************/

bool
tm_buffer_rep::needs_to_be_saved () {
  if (!buf->in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (!buf->in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}
