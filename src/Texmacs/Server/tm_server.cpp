
/******************************************************************************
* MODULE     : tm_server.cpp
* DESCRIPTION: The TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "tm_server.hpp"
#include "drd_std.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "sys_utils.hpp"
#include "file.hpp"
#include "tm_link.hpp"
#include "socket_link.hpp"
#include "socket_server.hpp"
#include "dictionary.hpp"
#include "socket_notifier.hpp"

server* the_server= NULL;
bool texmacs_started= false;
url tm_init_file= url_none ();
url my_init_file= url_none ();
string my_init_cmds= "";

/******************************************************************************
* Execution of commands
******************************************************************************/

void reset_inclusions ();
extern string printing_dpi;
extern string printing_cmd;
extern string printing_on;
extern int nr_windows;

/******************************************************************************
* Texmacs server constructor and destructor
******************************************************************************/

void
texmacs_interpose_handler () {
  if (the_server != NULL)
    (*the_server)->interpose_handler ();
}

void
texmacs_wait_handler (string message, string arg, int level) {
  (void) level;
  if (texmacs_started && the_server != NULL)
    (*the_server)->wait_handler (message, arg);
}

server
get_server () {
  ASSERT (the_server != NULL, "TeXmacs server not yet started");
  return *the_server;
}

tree
get_subtree (path p) {
  return get_server()->get_editor()->the_subtree (p);
}

void
gui_set_output_language (string lan) {
  set_output_language (lan);
  gui_refresh ();
}

server_rep::server_rep () {}
server_rep::~server_rep () {}

tm_server_rep::tm_server_rep ():
  vw (NULL), def_sfactor (5),
  style_cache (hashmap<string,tree> (UNINIT)),
  style_drd (tree (COLLECTION))
{
  the_server= tm_new<server> (this);
  initialize_guile ();
  gui_interpose (texmacs_interpose_handler);
  set_wait_handler (texmacs_wait_handler);
  if (is_none (tm_init_file))
    tm_init_file= "$TEXMACS_PATH/progs/init-texmacs.scm";
  if (is_none (my_init_file))
    my_init_file= "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm";
  bench_start ("initialize scheme");
  if (exists (tm_init_file)) exec_file (tm_init_file);
  if (exists (my_init_file)) exec_file (my_init_file);
  bench_cumul ("initialize scheme");
  if (my_init_cmds != "") {
    my_init_cmds= "(dialogue" * my_init_cmds * ")";
    exec_delayed (scheme_cmd (my_init_cmds));
  }
#ifdef OS_GNU_LINUX
  return; // in order to avoid segmentation faults
#elif defined OS_POWERPC_GNU_LINUX
  return; // in order to avoid segmentation faults
#endif
}

tm_server_rep::~tm_server_rep () {}
server::server (): rep (tm_new<tm_server_rep> ()) {}

/******************************************************************************
* Get and set objects associated to server
******************************************************************************/

server_rep*
tm_server_rep::get_server () {
  return this;
}

bool
tm_server_rep::has_view () {
  return vw != NULL;
}

bool
tm_server_rep::has_window () {
  return vw != NULL && vw->win != NULL;
}

tm_view
tm_server_rep::get_view (bool must_be_valid) {
  ASSERT (!must_be_valid || vw != NULL, "no active view");
  return vw;
}

void
tm_server_rep::set_view (tm_view vw2) {
  vw= vw2;
  if (vw != NULL)
    the_drd= vw->ed->drd;
}

tm_buffer
tm_server_rep::get_buffer () {
  tm_view vw= get_view ();
  return vw->buf;
}

editor
tm_server_rep::get_editor () {
  tm_view vw= get_view ();
  // cout << "Get editor" << vw->ed << "\n";
  return vw->ed;
}

tm_window
tm_server_rep::get_window () {
  tm_view vw= get_view ();
  ASSERT (vw->win != NULL, "no window attached to view");
  return vw->win;
}

int
tm_server_rep::get_nr_windows () {
  return nr_windows;
}

/******************************************************************************
* The style and package menus
******************************************************************************/

static string
compute_style_menu (url u, int kind) {
  if (is_or (u)) {
    string sep= "\n";
    if (is_atomic (u[1]) &&
	((is_concat (u[2]) && (u[2][1] != "CVS") && (u[2][1] != ".svn")) ||
	 (is_or (u[2]) && is_concat (u[2][1]))))
      sep= "\n---\n";
    return
      compute_style_menu (u[1], kind) * sep *
      compute_style_menu (u[2], kind);
  }
  if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    string sub= compute_style_menu (u[2], kind);
    if ((dir == "Test") || (dir == "Obsolete") ||
	(dir == "CVS") || (dir == ".svn")) return "";
    return "(-> \"" * dir * "\" " * sub * ")";
  }
  if (is_atomic (u)) {
    string l  = as_string (u);
    if (!ends (l, ".ts")) return "";
    l= l(0, N(l)-3);
    string cmd ("init-style");
    if (kind == 1) cmd= "init-add-package";
    if (kind == 2) cmd= "init-remove-package";
    return "(\"" * l * "\" (" * cmd * " \"" * l * "\"))";
  }
  return "";
}

object
tm_server_rep::get_style_menu () {
  url sty_u= descendance ("$TEXMACS_STYLE_ROOT");
  string sty= compute_style_menu (sty_u, 0);
  return eval ("(menu-dynamic " * sty * ")");
}

object
tm_server_rep::get_add_package_menu () {
  url pck_u= descendance ("$TEXMACS_PACKAGE_ROOT");
  string pck= compute_style_menu (pck_u, 1);
  return eval ("(menu-dynamic " * pck * ")");
}

object
tm_server_rep::get_remove_package_menu () {
  url pck_u= descendance ("$TEXMACS_PACKAGE_ROOT");
  string pck= compute_style_menu (pck_u, 2);
  return eval ("(menu-dynamic " * pck * ")");
}

/******************************************************************************
* Caching style files
******************************************************************************/

static string
cache_file_name (tree t) {
  if (is_atomic (t)) return t->label;
  else {
    string s;
    int i, n= N(t);
    for (i=0; i<n; i++)
      s << "__" << cache_file_name (t[i]);
    return s * "__";
  }
}

void
tm_server_rep::style_clear_cache () {
  style_cache=
    hashmap<tree,hashmap<string,tree> > (hashmap<string,tree> (UNINIT));
  remove ("$TEXMACS_HOME_PATH/system/cache" * url_wildcard ("__*"));

  int i, j, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= ((tm_buffer) bufs[i]);
    for (j=0; j<N(buf->vws); j++)
      ((tm_view) (buf->vws[j]))->ed->init_style ();
  }
}

void
tm_server_rep::style_set_cache (tree style, hashmap<string,tree> H, tree t) {
  // cout << "set cache " << style << LF;
  style_cache (copy (style))= H;
  style_drd   (copy (style))= t;
  url name ("$TEXMACS_HOME_PATH/system/cache", cache_file_name (style));
  if (!exists (name)) {
    save_string (name, tree_to_scheme (tuple ((tree) H, t)));
    // cout << "saved " << name << LF;
  }
}

void
tm_server_rep::style_get_cache (
  tree style, hashmap<string,tree>& H, tree& t, bool& f)
{
  // cout << "get cache " << style << LF;
  if ((style == "") || (style == tree (TUPLE))) { f= false; return; }
  f= style_cache->contains (style);
  if (f) {
    H= style_cache [style];
    t= style_drd   [style];
  }
  else {
    string s;
    url name ("$TEXMACS_HOME_PATH/system/cache", cache_file_name (style));
    if (exists (name) && (!load_string (name, s, false))) {
      // cout << "loaded " << name << LF;
      tree p= scheme_to_tree (s);
      H= hashmap<string,tree> (UNINIT, p[0]);
      t= p[1];
      style_cache (copy (style))= H;
      style_drd   (copy (style))= t;
      f= true;
    }
  }
}

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

void
tm_server_rep::interpose_handler () {
#ifdef QTTEXMACS
  // TeXmacs/Qt handles delayed messages and socket notification
  // in its own runloop 
#else
  perform_select ();
  exec_pending_commands ();
#endif

  int i,j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= (tm_buffer) bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      if (vw->win != NULL) vw->ed->apply_changes ();
    }
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      if (vw->win != NULL) vw->ed->animate ();
    }
  }
}

void
tm_server_rep::wait_handler (string message, string arg) {
  show_wait_indicator (get_window () -> win, message, arg);
}

void
tm_server_rep::set_script_status (int i) {
  script_status= i;
}

void
tm_server_rep::focus_on_editor (editor ed) {
  int i,j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= (tm_buffer) bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      if (vw->ed == ed) {
	set_view (vw);
	return;
      }
    }
  }
  FAILED ("invalid situation");
}

void
tm_server_rep::set_printing_command (string cmd) {
  printing_cmd= cmd;
}

void
tm_server_rep::set_printer_page_type (string type) {
  printing_on= type;
}

string
tm_server_rep::get_printer_page_type () {
  return printing_on;
}

void
tm_server_rep::set_printer_dpi (string dpi) {
  printing_dpi= dpi;
}

void
tm_server_rep::set_default_shrinking_factor (int sf) {
  def_sfactor= sf;
}

int
tm_server_rep::get_default_shrinking_factor () {
  return def_sfactor;
}

void
tm_server_rep::image_gc (string which) {
  ::image_gc (which);
  typeset_update_all ();
}

void
tm_server_rep::inclusions_gc (string which) {
  (void) which;
  reset_inclusions ();
  typeset_update_all ();
}

void
tm_server_rep::typeset_update (path p) {
  int i, j, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= ((tm_buffer) bufs[i]);
    for (j=0; j<N(buf->vws); j++)
      ((tm_view) (buf->vws[j]))->ed->typeset_invalidate (p);
  }
}

void
tm_server_rep::typeset_update_all () {
  int i, j, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= ((tm_buffer) bufs[i]);
    for (j=0; j<N(buf->vws); j++)
      ((tm_view) (buf->vws[j]))->ed->typeset_invalidate_all ();
  }
}

bool
tm_server_rep::is_yes (string s) {
  s= locase_all (s);
  return
    (s == "ano") || (s == "a") ||
    (s == "yes") || (s == "y") ||
    (s == "oui") || (s == "o") ||
    (s == "ja") || (s == "j") ||
    (s == "si") || (s == "s") ||
    (s == "sim") || (s == "s") ||
    (s == "tak") || (s == "t");
}

void
tm_server_rep::quit () {
  close_all_pipes ();
  call ("quit-TeXmacs-scheme");
  clear_pending_commands ();
  exit (0);
}

/******************************************************************************
* System commands
******************************************************************************/

void
tm_server_rep::shell (string s) {
  system (s);
}
