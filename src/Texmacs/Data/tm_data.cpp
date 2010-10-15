
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

url tm_init_buffer_file= url_none ();
url my_init_buffer_file= url_none ();

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_data_rep::tm_data_rep () {}
tm_data_rep::~tm_data_rep () {}

/******************************************************************************
* Low level functions for maintaining the buffer menu
******************************************************************************/

int
tm_data_rep::find_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return i;
  return -1;
}

int
tm_data_rep::find_buffer (url name) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->name == name)
      return i;
  return -1;
}

string
tm_data_rep::new_menu_name (url u) {
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
      if (bufs[i]->abbr == ret) flag= false;
    if (flag) return ret;
  }
}

static void
menu_append_buffer (string& s, tm_buffer buf) {
  if (buf->in_menu) {
    string name= copy (buf->abbr);
    if (buf->needs_to_be_saved ()) name << " *"; 
    s << " (" << scm_quote (name);
    s << " (switch-to-buffer " * scm_quote (as_string (buf->name)) * "))";
  }
}

object
tm_data_rep::get_buffer_menu () {
  int i, count;
  bool two_types= false;;
  string s ("(menu-dynamic");
  for (i=0, count=0; i<N(bufs); i++) {
    if (is_none (bufs[i]->extra) == is_none (bufs[0]->extra)) {
      menu_append_buffer (s, bufs[i]);
      count++;
    }
    else two_types= true;
    if (count == 10) break;
  }
  if (two_types) {
    s << " ---";
    for (i=0, count=0; i<N(bufs); i++) {
      if (is_none (bufs[i]->extra) != is_none (bufs[0]->extra)) {
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
tm_data_rep::buffer_in_menu (url u, bool flag) {
  int nr= find_buffer (u);
  if (nr == -1) return false;
  tm_buffer buf= bufs[nr];
  bool old= buf->in_menu;
  buf->in_menu= flag;
  return old;
}

void
tm_data_rep::menu_insert_buffer (tm_buffer buf) {
  // bufs << ((pointer) buf); // causes compilation error
  bufs << buf; // WARNING: that one compile, what was the use of the cast?
}

void
tm_data_rep::menu_delete_buffer (tm_buffer buf) {
  int i, nr, n=N(bufs);
  for (nr=0; nr<n; nr++)
    if (bufs[nr] == ((pointer) buf)) break;
  if (nr==N(bufs)) return;

  for (i=nr; i<(n-1); i++) bufs[i]= bufs[i+1];
  bufs->resize (n-1);
}

void
tm_data_rep::menu_focus_buffer (tm_buffer buf) {
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
tm_data_rep::new_buffer (url name) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  buf->abbr= new_menu_name (name);
  menu_insert_buffer (buf);
  return buf;
}

tm_buffer
tm_data_rep::new_buffer (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr != -1) return bufs[nr];
  tm_buffer buf= new_buffer (name);
  set_document (buf->rp, extract (doc, "body"));
  buf->project= extract (doc, "project");
  buf->style  = extract (doc, "style");
  buf->init   = hashmap<string,tree> (UNINIT, extract (doc, "initial"));
  buf->fin    = hashmap<string,tree> (UNINIT, extract (doc, "final"));
  buf->ref    = hashmap<string,tree> (UNINIT, extract (doc, "references"));
  buf->aux    = hashmap<string,tree> (UNINIT, extract (doc, "auxiliary"));
  if (buf->project != "") {
    url prj_name= head (name) * as_string (buf->project);
    buf->prj= load_passive_buffer (prj_name);
  }
  return buf;
}

void
tm_data_rep::revert_buffer (url name, tree doc) {
  int i, nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf= bufs[nr];
  buf->project= extract (doc, "project");
  buf->style  = extract (doc, "style");
  buf->init   = hashmap<string,tree> (UNINIT, extract (doc, "initial"));
  buf->fin    = hashmap<string,tree> (UNINIT, extract (doc, "final"));
  buf->ref    = hashmap<string,tree> (UNINIT, extract (doc, "references"));
  buf->aux    = hashmap<string,tree> (UNINIT, extract (doc, "auxiliary"));
  if (N(buf->vws)==0) set_document (buf->rp, extract (doc, "body"));
  else for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    if (i==0) assign (vw->ed->rp, extract (doc, "body"));
    vw->ed->set_style (buf->style);
    vw->ed->set_init  (buf->init);
    vw->ed->set_fin   (buf->fin);
    vw->ed->notify_page_change ();
    vw->ed->add_init (buf->init);
    vw->ed->notify_change (THE_DECORATIONS);
    vw->ed->typeset_invalidate_env ();
    vw->ed->notify_save ();
  }
}

void
tm_data_rep::delete_buffer (tm_buffer buf) {
  int i;
  menu_delete_buffer (buf);
  for (i=0; i<N(buf->vws); i++)
    delete_view (buf->vws[i]);
  tm_delete (buf);
}

void
tm_data_rep::set_name_buffer (url name) {
  tm_buffer buf= get_buffer ();
  if (buf->name == name) return;
  buf->name= name;
  set_abbr_buffer (new_menu_name (name));
}

string
tm_data_rep::get_abbr_buffer () {
  tm_buffer buf= get_buffer ();
  return buf->abbr;
}

void
tm_data_rep::set_abbr_buffer (string abbr) {
  int i;
  tm_buffer buf= get_buffer ();
  if (buf->abbr == abbr) return;
  buf->abbr= abbr;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw2= buf->vws[i];
    if (vw2->win != NULL) {
      vw2->win->set_window_name (buf->abbr);
      vw2->win->set_window_url (is_none (buf->extra)? buf->name: buf->extra);
    }
  }
}

url
tm_data_rep::get_name_buffer () {
  tm_buffer buf= get_buffer ();
  if (!is_none (buf->extra)) return buf->extra;
  return buf->name;
}

url
tm_data_rep::get_name_buffer (path p) {
  int nr= find_buffer (p);
  if (nr == -1) return url_none ();
  return bufs[nr]->name;
}

url
tm_data_rep::get_all_buffers () {
  url u= url_none ();
  for (int i=N(bufs)-1; i>=0; i--)
    u= bufs[i]->name | u;
  return u;
}

/******************************************************************************
* Low level view routines
******************************************************************************/

tm_view
tm_data_rep::new_view (url name) {
  // cout << "Creating new view\n";

  tm_buffer buf= new_buffer (name);
  editor    ed = new_editor (get_server (), buf);
  tm_view   vw = tm_new<tm_view_rep> (buf, ed);
  buf->vws << vw;

  ed->set_style (buf->style);
  ed->set_init (buf->init);
  ed->set_fin (buf->fin);
  ed->notify_page_change ();
  ed->add_init (buf->init);
  ed->notify_change (THE_DECORATIONS);
  ed->typeset_invalidate_env ();

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
tm_data_rep::get_passive_view (tm_buffer buf) {
  int i;
  for (i=0; i<N(buf->vws); i++)
    if (buf->vws[i]->win == NULL)
      return buf->vws[i];
  return new_view (buf->name);
}

void
tm_data_rep::delete_view (tm_view vw) {
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
tm_data_rep::attach_view (tm_window win, tm_view vw) {
  // cout << "Attach view " << vw->buf->name << "\n";
  vw->win= win;
  widget wid= win->wid;
  set_canvas (wid, vw->ed);
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->resume ();
  win->set_window_name (vw->buf->abbr);
  win->set_window_url (is_none (vw->buf->extra)? vw->buf->name: vw->buf->extra);
  // cout << "View attached\n";
}

void
tm_data_rep::detach_view (tm_view vw) {
  // cout << "Detach view " << vw->buf->name << "\n";
  tm_window win= vw->win;
  if (win == NULL) return;
  vw->win= NULL;
  widget wid= win->wid;
  ASSERT (is_attached (wid), "widget should be attached");
  vw->ed->suspend ();
  set_canvas (wid, glue_widget ());
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
tm_data_rep::new_window (bool map_flag, tree geom) {
  int mask= 0;
  if (get_preference ("header") == "on") mask += 1;
  if (get_preference ("main icon bar") == "on") mask += 2;
  if (get_preference ("context dependent icons") == "on") mask += 4;
  if (get_preference ("user provided icons") == "on") mask += 8;
  if (get_preference ("status bar") == "on") mask += 16;
  command quit= tm_new<kill_window_command_rep> ();
  tm_window win= tm_new<tm_window_rep> (texmacs_widget (mask, quit), geom);
  if (map_flag) win->map ();
  return win;
}

bool
tm_data_rep::delete_view_from_window (tm_window win) {
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
tm_data_rep::delete_window (tm_window win) {
  while (delete_view_from_window (win)) {}
  win->unmap ();
  destroy_window_widget (win->win);
  tm_delete (win);
}

/******************************************************************************
* Other subroutines
******************************************************************************/

void
tm_data_rep::new_buffer_in_this_window (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr != -1) switch_to_buffer (nr);
  else {
    (void) new_buffer (name, doc);
    switch_to_buffer (name);
  }
}

void
tm_data_rep::new_buffer_in_new_window (url name, tree doc, tree geom) {
  tm_window win= new_window (true, geom);
  tm_buffer buf= new_buffer (name, doc);
  tm_view   vw = get_passive_view (buf);
  attach_view (win, vw);
  set_view (vw);
  menu_focus_buffer (buf);
}

/******************************************************************************
* Exported routines
******************************************************************************/

int
tm_data_rep::nr_bufs () {
  return N(bufs);
}

tm_buffer
tm_data_rep::get_buf (int i) {
  return (tm_buffer) bufs[i];
}

tm_buffer
tm_data_rep::get_buf (path p) {
  int nr= find_buffer (p);
  if (nr >= 0) return bufs[nr];
  else return NULL;
}

url
tm_data_rep::new_buffer () {
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
tm_data_rep::switch_to_buffer (int nr) {
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
tm_data_rep::switch_to_buffer (path p) {
  int nr= find_buffer (p);
  if (nr != -1) switch_to_buffer (nr);
  return nr != -1;
}

void
tm_data_rep::switch_to_buffer (url name) {
  int nr= find_buffer (name);
  if (nr == -1) {
    load_passive_buffer (name);
    nr= find_buffer (name);
  }
  if (nr != -1) switch_to_buffer (nr);
}

void
tm_data_rep::switch_to_active_buffer (url name) {
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
tm_data_rep::revert_buffer () {
  tm_buffer buf= get_buffer ();
  web_cache_invalidate (buf->name);
  tree doc= load_tree (buf->name, buf->fm);
  if (doc == "error") set_message ("Error: file not found", "revert buffer");
  else revert_buffer (buf->name, doc);
}

void
tm_data_rep::kill_buffer () {
  int i, nr;
  if (N(bufs) <= 1) quit();
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
tm_data_rep::open_window (tree geom) {
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
tm_data_rep::clone_window () {
  tm_window win= new_window ();
  tm_buffer buf= get_buffer ();
  tm_view   vw = get_passive_view (buf);
  attach_view (win, vw);
  set_view (vw);
  menu_focus_buffer (buf);
}

void
tm_data_rep::kill_window () {
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
  if (number_of_servers () == 0) quit ();
  else delete_window (win);
}

void
tm_data_rep::kill_window_and_buffer () {
  if (N(bufs) <= 1) quit();
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
tm_data_rep::no_bufs () {
  return N(bufs) == 0;
}

void
tm_data_rep::set_aux (string aux, url name) {
  int i, nr= find_buffer (aux);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    buf->extra= name;
    if (starts (aux, "Help - ")) {
      buf->fm= "help";
      buf->read_only= true;
    }
    for (i=0; i<N(buf->vws); i++) {
      tm_view vw= buf->vws[i];
      vw->ed->set_base_name (name);
    }
  }
}

void
tm_data_rep::set_aux_buffer (string aux, url name, tree doc) {
  int nr= find_buffer (aux);
  if (nr == -1) new_buffer (aux, doc);
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
tm_data_rep::set_help_buffer (url name, tree doc) {
  set_aux_buffer (get_help_title (name, doc), name, doc);
}

void
tm_data_rep::set_buffer_tree (url name, tree doc) {
  int nr= find_buffer (name);
  if (nr == -1) new_buffer (name, tree (DOCUMENT));
  nr= find_buffer (name);
  tm_buffer buf= bufs[nr];
  assign (buf->rp, doc);
}

tree
tm_data_rep::get_buffer_tree (url name) {
  int nr= find_buffer (name);
  if (nr == -1) return "";
  tm_buffer buf= bufs[nr];
  return subtree (the_et, buf->rp);
}

/******************************************************************************
* Projects
******************************************************************************/

void
tm_data_rep::project_attach (string prj_name) {
  int i;
  tm_buffer buf= get_buffer ();
  buf->project= prj_name;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    vw->ed->notify_change (THE_DECORATIONS);
    vw->ed->require_save ();
  }
  if (prj_name == "") buf->prj= NULL;
  else {
    url full_name= head (buf->name) * prj_name;
    buf->prj= load_passive_buffer (full_name);
  }
}

bool
tm_data_rep::project_attached () {
  tm_buffer buf= get_buffer ();
  return buf->project != "";
}

object
tm_data_rep::get_project_buffer_menu () {
  tm_buffer buf= get_buffer ();
  if (buf->prj == NULL) return eval ("(menu-dynamic)");
  string s ("(menu-dynamic ");
  s << "(" << scm_quote (buf->prj->abbr) << " ";
  s << "(switch-to-buffer " * scm_quote (as_string (buf->prj->name)) * "))";

  tree t= subtree (the_et, buf->prj->rp);
  int i, j, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], INCLUDE, 1) && is_atomic (t[i][0])) {
      string name= as_string (head (buf->prj->name) * as_string (t[i][0]));
      for (j=N(name)-1; j>=0; j--)
	if (name[j]=='/') break;
      s << " (" << scm_quote (name (j+1, N(name))) << " ";
      s << "(switch-to-buffer " * scm_quote (name) * "))";
    }

  s << ")";
  return eval (s);
}

/******************************************************************************
* Management of all edit trees
******************************************************************************/

tree the_et;

path
new_document () {
  int i, n= N(the_et);
  for (i=0; i<n; i++)
    if (the_et[i] == UNINIT) {
      assign (the_et[i], tree (DOCUMENT, ""));
      return path (i); // obtain_ip (the_et[i]);
    }
  insert (the_et, n, tuple (tree (DOCUMENT, "")));
  return path (n); // obtain_ip (the_et[n]);
}

void
delete_document (path rp) {
  assign (subtree (the_et, rp), UNINIT);
  clean_observers (subtree (the_et, rp));
}

void
set_document (path rp, tree t) {
  //assign (subtree (the_et, rp), t);
  assign (subtree (the_et, rp), copy (t));
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
tm_data_rep::window_current () {
  tm_window win= get_window ();
  return win->id;
}

path
tm_data_rep::windows_list () {
  return the_windows;
}

path
tm_data_rep::buffer_to_windows (url name) {
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
tm_data_rep::window_to_buffer (int id) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (bufs[i]->vws[j]->win->id == id)
	  return bufs[i]->name;
  return url_none ();
}

tm_view
tm_data_rep::window_find_view (int id) {
  for (int i=0; i<N(bufs); i++)
    for (int j=0; j<N(bufs[i]->vws); j++)
      if (bufs[i]->vws[j]->win != NULL)
	if (bufs[i]->vws[j]->win->id == id)
	  return bufs[i]->vws[j];
  return NULL;
}

void
tm_data_rep::window_set_buffer (int id, url name) {
  tm_view old_vw= window_find_view (id);
  if (old_vw == NULL || old_vw->buf->name == name) return;
  tm_window win= old_vw->win;
  int nr= find_buffer (name);
  if (nr == -1) return;
  tm_buffer buf   = bufs[nr];
  tm_view   new_vw= get_passive_view (buf);
  detach_view (old_vw);
  attach_view (win, new_vw);
}

void
tm_data_rep::window_focus (int id) {
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
  if (!in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (!in_menu) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}
