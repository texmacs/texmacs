
/******************************************************************************
* MODULE     : tm_data.cpp
* DESCRIPTION: Buffer management for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_data.hpp"
#include "convert.hpp"
#include "file.hpp"

url tm_init_buffer_file= url_none ();
url my_init_buffer_file= url_none ();
int max_undo_depth= 100; // should actually be part of tm_data_rep

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_data_rep::tm_data_rep (): history (0), hist_pos (-1) {}
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
  if ((name == "") || (name == "."))
    name= as_string (tail (u * url_parent ()));
  if ((name == "") || (name == "."))
    name= as_string (u);

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

object
tm_data_rep::get_buffer_menu () {
  int i;
  string s ("(menu-dynamic ");
  for (i=0; i<N(bufs); i++) {
    if (i>0) s << " ";
    s << "(\"" << bufs[i]->abbr;
    if (bufs[i]->needs_to_be_saved ()) s << " *"; 
    s << "\" (switch-to-buffer \"" * as_string (bufs[i]->name) * "\"))";
  }
  s << ")";
  return eval (s);
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
  tm_buffer buf= new tm_buffer_rep (name);
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
    if (i==0) vw->ed->assign (vw->ed->rp, extract (doc, "body"));
    vw->ed->set_style (buf->style);
    vw->ed->set_init  (buf->init);
    vw->ed->set_fin   (buf->fin);
    vw->ed->notify_page_change ();
    vw->ed->add_init (buf->init);
    vw->ed->notify_change (THE_DECORATIONS);
    vw->ed->typeset_invalidate_env ();
  }
  buf->mark_undo_block ();
  buf->need_save= buf->need_autosave= false;
  buf->last_save= buf->last_autosave= buf->undo_depth- 1;
}

void
tm_data_rep::delete_buffer (tm_buffer buf) {
  int i;
  menu_delete_buffer (buf);
  for (i=0; i<N(buf->vws); i++)
    delete_view (buf->vws[i]);
  delete buf;
}

void
tm_data_rep::set_name_buffer (url name) {
  tm_buffer buf= get_buffer ();
  if (buf->name == name) return;
  buf->name= name;
  set_abbr_buffer (new_menu_name (name));
}

void
tm_data_rep::set_abbr_buffer (string abbr) {
  int i;
  tm_buffer buf= get_buffer ();
  if (buf->abbr == abbr) return;
  buf->abbr= abbr;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw2= buf->vws[i];
    if (vw2->win != NULL)
      vw2->win->win->set_name (buf->abbr);
  }
}

url
tm_data_rep::get_name_buffer () {
  tm_buffer buf= get_buffer ();
  if (!is_none (buf->extra)) return buf->extra;
  return buf->name;
}

/******************************************************************************
* Low level view routines
******************************************************************************/

tm_view
tm_data_rep::new_view (url name) {
  // cout << "Creating new view\n";

  tm_buffer buf= new_buffer (name);
  editor    ed = new_editor (get_server (), buf);
  tm_view   vw = new tm_view_rep (buf, ed);
  buf->vws << vw;

  ed->set_style (buf->style);
  ed->set_init (buf->init);
  ed->set_fin (buf->fin);
  ed->notify_page_change ();
  ed->add_init (buf->init);
  ed->notify_change (THE_DECORATIONS);
  ed->notify_change (THE_AUTOMATIC_SIZE);
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
  delete vw;
}

void
tm_data_rep::attach_view (tm_window win, tm_view vw) {
  // cout << "Attach view " << vw->buf->name << "\n";
  vw->win= win;
  widget wid= (widget) win->wid;
  wid ["canvas"] << set_widget ("scrollable", vw->ed);
  if (wid->attached ()) {
    vw->ed->resume ();
    win->wid->set_window_name (vw->buf->abbr);
    wid ["canvas"] << emit_update ();
  }
  // cout << "View attached\n";
}

void
tm_data_rep::detach_view (tm_view vw) {
  // cout << "Detach view " << vw->buf->name << "\n";
  tm_window win= vw->win;
  if (win == NULL) return;
  vw->win= NULL;
  widget wid= (widget) win->wid;
  wid ["canvas"] << set_widget ("scrollable", glue_widget ());
  if (wid->attached ()) {
    vw->ed->suspend ();
    vw->ed << emit_attach_window (NULL);
    win->wid->set_window_name ("TeXmacs");
    wid ["canvas"] << emit_update ();
  }
  // cout << "View detached\n";
}

/******************************************************************************
* Low level window routines
******************************************************************************/

tm_window
tm_data_rep::new_window (display dis, bool map_flag) {
  tm_window win= new tm_window_rep (new tm_widget_rep (this, dis));
  if (map_flag) win->win->map ();
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
  while (delete_view_from_window (win));
  win->win->unmap ();
  delete win->win;
  delete win;
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
tm_data_rep::new_buffer_in_new_window (url name, tree doc) {
  tm_window win= new_window (get_display ());
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

void
tm_data_rep::new_buffer () {
  int i;
  for (i=1; true; i++) {
    string name= "no name " * as_string (i);
    if (i==1) name= "no name";
    if (find_buffer (name) != -1) continue;
    new_buffer_in_this_window (name, tree (DOCUMENT));
    return;
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
  tm_widget meta= new_vw->win->wid;
  meta->set_shrinking_factor (meta->get_shrinking_factor ());
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
  if (nr == N(bufs))
    fatal_error ("Buffer not found", "tm_data_rep::kill_buffer");
  for (nr=0; nr<N(bufs); nr++) if (buf != bufs[nr]) break;
  if (nr == N(bufs))
    fatal_error ("No suitable new buffer", "tm_data_rep::kill_buffer");
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

void
tm_data_rep::open_window () {
  int i;
  for (i=1; true; i++) {
    string name= "no name " * as_string (i);
    if (i==1) name= "no name";
    if (find_buffer (name) != -1) continue;
    new_buffer_in_new_window (name, tree (DOCUMENT));
    return;
  }
}

void
tm_data_rep::clone_window () {
  tm_window win= new_window (get_display ());
  tm_buffer buf= get_buffer ();
  tm_view   vw = get_passive_view (buf);
  attach_view (win, vw);
  set_view (vw);
  menu_focus_buffer (buf);
}

void
tm_data_rep::kill_window () {
  int i, j;
  tm_window win    = get_window ();
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
  quit ();
}

void
tm_data_rep::set_max_undo_depth (int i) {
  if (i <  0) i= -1;
  if (i == 0) i= 1;
  max_undo_depth= i;
}

int
tm_data_rep::get_max_undo_depth () {
  return max_undo_depth;
}

bool
tm_data_rep::no_bufs () {
  return N(bufs) == 0;
}

void
tm_data_rep::set_aux_buffer (string aux, url name, tree doc) {
  int i, nr= find_buffer (aux);
  if (nr == -1) new_buffer (aux, doc);
  else revert_buffer (aux, doc);
  nr= find_buffer (aux);
  if (nr != -1) {
    tm_buffer buf= bufs[nr];
    buf->extra= name;
    if (aux == "* Help *") {
      buf->fm= "help";
      buf->read_only= true;
    }
    for (i=0; i<N(buf->vws); i++) {
      tm_view vw= buf->vws[i];
      vw->ed->set_base_name (name);
    }
    switch_to_buffer (nr);
  }
}

void
tm_data_rep::set_help_buffer (url name, tree doc) {
  set_aux_buffer ("* Help *", name, doc);
  tree t= tuple (name->t, doc);
  hist_pos++;
  if ((hist_pos>=0) && (hist_pos<N(history)) && (history[hist_pos]!=t))
    history->resize (hist_pos);
  if (hist_pos == N(history))
    history << tuple (name->t, doc);
}

void
tm_data_rep::browse_help (int delta) {
  if (hist_pos + delta < 0) return;
  if (hist_pos + delta >= N(history)) return;
  hist_pos += delta-1;
  tree t= history[hist_pos+1];
  set_help_buffer (as_url (t[0]), t[1]);
}

/******************************************************************************
* Projects
******************************************************************************/

void
tm_data_rep::project_attach (string prj_name) {
  int i;
  tm_buffer buf= get_buffer ();
  buf->project= prj_name;
  buf->need_save= buf->need_autosave= true;
  for (i=0; i<N(buf->vws); i++) {
    tm_view vw= buf->vws[i];
    vw->ed->notify_change (THE_DECORATIONS);
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
  s << "(\"" << buf->prj->abbr << "\" ";
  s << "(switch-to-buffer \"" * as_string (buf->prj->name) * "\"))";

  tree t= subtree (the_et, buf->prj->rp);
  int i, j, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], INCLUDE, 1) && is_atomic (t[i][0])) {
      string name= as_string (head (buf->prj->name) * as_string (t[i][0]));
      for (j=N(name)-1; j>=0; j--)
	if (name[j]=='/') break;
      s << " (\"" << name (j+1, N(name)) << "\" ";
      s << "(switch-to-buffer \"" * name * "\"))";
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
}

void
set_document (path rp, tree t) {
  assign (subtree (the_et, rp), t);
  //assign (subtree (the_et, rp), copy (t));
}

/******************************************************************************
* Undo/redo routines for buffers as well as tests for (auto)saves
******************************************************************************/

void
tm_buffer_rep::mark_undo_block () {
  if ((undo == "nil") || (undo[0] != "")) {
    undo= tree (BACKUP, "", undo);
    undo_depth++;
  }
}

void
tm_buffer_rep::mark_redo_block () {
  if ((redo == "nil") || (redo[0] != "")) {
    redo= tree (BACKUP, "", redo);
    exdo= tree (BACKUP, "", exdo);
    redo_depth++;
  }
}

void
tm_buffer_rep::unmark_undo_block () {
  if ((undo != "nil") && undo[0] == "") {
    undo= undo[1];
    undo_depth--;
  }
}

void
tm_buffer_rep::unmark_redo_block () {
  if ((redo != "nil") && redo[0] == "") {
    redo= redo[1];
    exdo= exdo[1];
    redo_depth--;
  }
}

void
tm_buffer_rep::redo_to_undo () {
  if (redo == "nil") mark_undo_block ();
  else {
    tree re= redo[0];
    tree ex= exdo[0];
    redo= redo[1];
    exdo= exdo[1];
    if (re == "") redo_depth--;
    if (ex == "") mark_undo_block ();
    else undo= tree (BACKUP, ex, undo);
    redo_to_undo ();
    if (re == "") mark_undo_block ();
    else undo= tree (BACKUP, re, undo);
  }
}

void
tm_buffer_rep::truncate_undos (int nr) {
  int i;
  tree rev= "nil";
  for (i=0; i<nr; i++) {
    while (undo[0] != "") {
      rev= tree (BACKUP, undo[0], rev);
      undo= undo[1];
    }
    rev= tree (BACKUP, undo[0], rev);
    undo= undo[1];
  }

  undo= "nil";
  for (i=0; i<nr; i++) {
    undo= tree (BACKUP, rev[0], undo);
    rev= rev[1];
    while ((rev != "nil") && (rev[0] != "")) {
      undo= tree (BACKUP, rev[0], undo);
      rev= rev[1];
    }
  }

  int del= undo_depth- nr;
  undo_depth    -= del;
  last_save     -= del;
  last_autosave -= del;
}

bool
tm_buffer_rep::needs_to_be_saved () {
  if (need_save) return true;
  if ((undo == "nil") || (undo[0] != ""))
    return (last_save != undo_depth);
  else return (last_save != (undo_depth-1));
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (!needs_to_be_saved ()) return false;
  if (need_autosave) return true;
  if ((undo == "nil") || (undo[0] != ""))
    return (last_autosave != undo_depth);
  else return (last_autosave != (undo_depth-1));
}
