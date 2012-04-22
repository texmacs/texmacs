
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
#include "dictionary.hpp"
#include "tm_link.hpp"
#include "socket_notifier.hpp"
#include "new_style.hpp"

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

bool
in_presentation_mode () {
  return get_server () -> in_full_screen_mode ();
}

tree
get_subtree (path p) {
  return get_editor () -> the_subtree (p);
}

void
gui_set_output_language (string lan) {
  set_output_language (lan);
  get_server () -> refresh ();
  gui_refresh ();
}

server_rep::server_rep () {}
server_rep::~server_rep () {}

tm_server_rep::tm_server_rep (): def_sfactor (5) {
  the_server= tm_new<server> (this);
  initialize_scheme ();
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
    my_init_cmds= "(begin" * my_init_cmds * ")";
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
server_rep* tm_server_rep::get_server () { return this; }

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

void
tm_server_rep::style_clear_cache () {
  style_invalidate_cache ();

  int i, j, n= N(bufs);
  for (i=0; i<n; i++) {
    tm_buffer buf= ((tm_buffer) bufs[i]);
    for (j=0; j<N(buf->vws); j++)
      ((tm_view) (buf->vws[j]))->ed->init_style ();
  }
}

void
tm_server_rep::refresh () {
  array<url> l= windows_list ();
  for (int i=0; i<N(l); i++) {
    url u= get_window_view (l[i]);
    if (!is_none (u)) search_view (u)->win->refresh ();
  }
}

void
tm_server_rep::interpose_handler () {
#ifdef QTTEXMACS
  // TeXmacs/Qt handles delayed messages and socket notification
  // in its own runloop
#ifndef QTPIPES
  perform_select ();
#endif
  process_all_pipes ();
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

  windows_refresh ();
}

void
tm_server_rep::wait_handler (string message, string arg) {
  show_wait_indicator (access_window () -> win, translate (message), arg);
}

void
tm_server_rep::set_script_status (int i) {
  script_status= i;
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
