
/******************************************************************************
* MODULE     : tm_server.cpp
* DESCRIPTION: The TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tm_server.hpp"
#include "drd_std.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "sys_utils.hpp"
#include "file.hpp"
#include "pipe_link.hpp"
#include "socket_link.hpp"

server* the_server= NULL;
url tm_init_file= url_none ();
url my_init_file= url_none ();
string my_init_cmds= "";

void reset_inclusions ();
extern string printing_dpi;
extern string printing_cmd;
extern string printing_on;
extern int nr_windows;
extern window (*get_current_window) (void);

/******************************************************************************
* Creation of TeXmacs window
******************************************************************************/

int geometry_w= 800, geometry_h= 600;
int geometry_x= 0  , geometry_y= 0;

window
texmacs_window (widget wid) {
  int W, H;
  int w= geometry_w, h= geometry_h;
  int x= geometry_x, y= geometry_y;
  wid->dis->get_extents (W, H); W /= PIXEL; H /= PIXEL;
  if (x < 0) x= W + x + 1 - w;
  if (y < 0) y= H + y + 1 - h;
  return plain_window (wid, "TeXmacs", w*PIXEL, h*PIXEL, x*PIXEL, (-y)*PIXEL);
}

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
  if (the_server != NULL)
    (*the_server)->wait_handler (message, arg);
}

window
texmacs_current_window () {
  if (the_server == NULL)
    fatal_error ("No server", "texmacs_current_window", "tm_server.cpp");
  return (*the_server)->get_window()->win;
}

server
get_server () {
  if (the_server == NULL)
    fatal_error ("TeXmacs server not yet started", "get_server");
  return *the_server;
}

server_rep::server_rep () {}
server_rep::~server_rep () {}

tm_server_rep::tm_server_rep (display dis2):
  dis (dis2), vw (NULL),
  full_screen (false), full_screen_edit (false), def_sfactor (5),
  style_cache (hashmap<string,tree> (UNINIT)),
  style_drd (tree (COLLECTION))
{
  the_server= new server (this);
  initialize_guile ();
  set_interpose_handler (texmacs_interpose_handler);
  set_wait_handler (texmacs_wait_handler);
  get_current_window= texmacs_current_window;
  out_lan= dis->get_output_language ();
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
server::server (display dis): rep (new tm_server_rep (dis)) {}

/******************************************************************************
* Get and set objects associated to server
******************************************************************************/

server_rep*
tm_server_rep::get_server () {
  return this;
}

display
tm_server_rep::get_display () {
  return dis;
}

bool
tm_server_rep::has_view () {
  return vw != NULL;
}

tm_view
tm_server_rep::get_view (bool must_be_valid) {
  if (must_be_valid && (vw==NULL))
    fatal_error ("No active view", "tm_server_rep::get_meta");
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
  if (vw->win==NULL)
    fatal_error ("No window attached to view", "tm_server_rep::get_meta");
  return vw->win;
}

tm_widget
tm_server_rep::get_meta () {
  tm_window win= get_window ();
  return win->wid;
}

color
tm_server_rep::get_color (string s) {
  return get_display () -> get_color (s);
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
	((is_concat (u[2]) && (u[2][1] != "CVS")) ||
	 (is_or (u[2]) && is_concat (u[2][1]))))
      sep= "\n---\n";
    return
      compute_style_menu (u[1], kind) * sep *
      compute_style_menu (u[2], kind);
  }
  if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    string sub= compute_style_menu (u[2], kind);
    if ((dir == "Test") || (dir == "Obsolete") || (dir == "CVS")) return "";
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
  remove ("$TEXMACS_HOME_PATH/system/cache" * url_wildcard ("*"));

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
    if (exists (name) && (!load_string (name, s))) {
      // cout << "loaded " << name << LF;
      tree pair= scheme_to_tree (s);
      H= hashmap<string,tree> (UNINIT, pair[0]);
      t= pair[1];
      style_cache (copy (style))= H;
      style_drd   (copy (style))= t;
      f= true;
    }
  }
}

/******************************************************************************
* Routines concerning the widget
******************************************************************************/

void
tm_server_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  widget meta= (widget) get_meta ();
  meta["canvas"] << ::get_visible (x1, y1, x2, y2);
}

void
tm_server_rep::scroll_where (SI& x, SI& y) {
  widget meta= (widget) get_meta ();
  meta["canvas"] << get_coord2 ("scroll position", x, y);
}

void
tm_server_rep::scroll_to (SI x, SI y) {
  widget meta= (widget) get_meta ();
  meta["canvas"] << set_scroll_pos (x, y);
}

void
tm_server_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  widget meta= (widget) get_meta ();
  meta["canvas"] << ::set_extents (x1, y1, x2, y2);
}

void
tm_server_rep::set_left_footer (string s) {
  if ((vw == NULL) || (vw->win == NULL)) return;
  get_meta()->set_left_footer (s);
}

void
tm_server_rep::set_right_footer (string s) {
  if ((vw == NULL) || (vw->win == NULL)) return;
  get_meta()->set_right_footer (s);
}

void
tm_server_rep::set_message (string left, string right, bool temp) {
  if ((vw == NULL) || (vw->win == NULL)) return;
  get_editor()->set_message (left, right, temp);
}

void
tm_server_rep::recall_message () {
  if ((vw == NULL) || (vw->win == NULL)) return;
  get_editor()->recall_message ();
}

void
tm_server_rep::full_screen_mode (bool on, bool edit) {
  widget meta= (widget) get_meta ();
  if (on && !edit) {
    show_header (false);
    show_footer (false);
    meta ["canvas"] << set_integer ("scrollbars", false);
  }
  else {
    show_header (true);
    show_footer (true);
    meta ["canvas"] << set_integer ("scrollbars", true);
  }
  meta->win->full_screen (on);
  get_editor()->full_screen_mode (on && !edit);
  full_screen = on;
  full_screen_edit = on && edit;
}

bool
tm_server_rep::in_full_screen_mode () {
  return full_screen && !full_screen_edit;
}

bool
tm_server_rep::in_full_screen_edit_mode () {
  return full_screen && full_screen_edit;
}

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

void
tm_server_rep::interpose_handler () {
  listen_to_pipes ();
  listen_to_sockets ();
  listen_to_connections ();
  exec_pending_commands ();

  int i,j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= (tm_buffer) bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      if (vw->win != NULL) vw->ed->process_mutators ();
    }
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
  dis->set_wait_indicator (message, arg);
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
  fatal_error ("Invalid situation", "tm_server_rep::focus_on_editor");
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
  dis->image_gc (which);
  int i,j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= (tm_buffer) bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      vw->ed->typeset_invalidate_all ();
    }
  }  
}

void
tm_server_rep::inclusions_gc (string which) {
  (void) which;
  reset_inclusions ();
  int i,j;
  for (i=0; i<N(bufs); i++) {
    tm_buffer buf= (tm_buffer) bufs[i];
    for (j=0; j<N(buf->vws); j++) {
      tm_view vw= (tm_view) buf->vws[j];
      vw->ed->typeset_invalidate_all ();
    }
  }  
}

string
tm_server_rep::translate (string which, string from, string to) {
  display dis= get_display ();
  return dis->translate (which, from, to);
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
  exit (0);
}

/******************************************************************************
* Extern packages
******************************************************************************/

tree
tm_server_rep::evaluate (string name, string session, tree expr) {
  if (name == "scheme") {
    string s= tree_to_verbatim (expr);
    string r= object_to_string (::eval (s));
    if (r == "#<unspecified>") r= "";
    return verbatim_to_tree (r);
  }
  if (!connection_declared (name)) {
    set_message ("Package#'" * name * "'#not declared",
		 "Evaluate#'" * name * "'#expression");
    return "";
  }
  if (connection_status (name, session) == CONNECTION_DEAD) {
    string r= connection_start (name, session);
    set_message (r, "Started#'" * name * "'");
    if (connection_status (name, session) == CONNECTION_DEAD) return "";
  }
  return connection_eval (name, session, expr);
}

/******************************************************************************
* System commands
******************************************************************************/

void
tm_server_rep::shell (string s) {
  system (s);
}
