
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
#include "boot.hpp"
#include "tm_server.hpp"
#include "drd_std.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "sys_utils.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "dictionary.hpp"
#include "tm_link.hpp"
#include "socket_notifier.hpp"
#include "new_style.hpp"
#include "Database/database.hpp"

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
extern string printing_on;
extern int nr_windows;

#ifdef QTTEXMACS
void del_obj_qt_renderer(void);
#endif

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
  else
    cout << "TeXmacs] Please wait: " << message << " " << arg << "\n";
}

bool is_server_started () {
  return the_server != NULL;
}

server
get_server () {
  ASSERT (is_server_started (), "TeXmacs server not yet started");
  return *the_server;
}

bool
in_presentation_mode () {
  return get_server () -> in_full_screen_mode ();
}

tree
get_subtree (path p) {
  return get_current_editor () -> the_subtree (p);
}

void
gui_set_output_language (string lan) {
  set_output_language (lan);
  get_server () -> refresh ();
  gui_refresh ();
}

server_rep::server_rep () {}
server_rep::~server_rep () {}

tm_server_rep::tm_server_rep ()
  : def_zoomf (1.0),
    idle_last_cpu_ms (0), idle_last_check_ms (0), idle_acc (0) {
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

  array<url> vs= get_all_views ();
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> init_style ();
}

void
tm_server_rep::refresh () {
  array<url> l= windows_list ();
  for (int i=0; i<N(l); i++) {
    url u= window_to_view (l[i]);
    if (!is_none (u)) concrete_view (u)->win->refresh ();
  }
}

// CPU usage below this many ms per second of wall time is considered idle
static const long   IDLE_CPU_THRESHOLD_MS   = 150;
// Number of consecutive idle polls (each ~1 s apart) before firing the hook
static const int    IDLE_CONSECUTIVE_NEEDED = 30;
// Minimum wall-clock interval between idle polls (milliseconds)
static const time_t IDLE_CHECK_INTERVAL_MS  = 1000;

void
tm_server_rep::idle_monitor_tick () {
  time_t now_wall = texmacs_time ();
  if ((now_wall - idle_last_check_ms) < IDLE_CHECK_INTERVAL_MS) return;
  long   now_cpu  = cpu_time_ms ();
  time_t dt_wall  = now_wall - idle_last_check_ms;
  long   dt_cpu   = now_cpu  - idle_last_cpu_ms;
  // Normalise to ms of CPU per second of wall time
  long   cpu_rate = (dt_wall > 0) ? (dt_cpu * 1000L / dt_wall) : dt_cpu;
  idle_last_cpu_ms   = now_cpu;
  idle_last_check_ms = now_wall;
  idle_acc = cpu_rate < IDLE_CPU_THRESHOLD_MS ? idle_acc+1 : 0;
  if (DEBUG_IO)
    debug_io << "idle: cpu_rate=" << cpu_rate
             << " acc=" << idle_acc << "/" << IDLE_CONSECUTIVE_NEEDED
             << " dt_cpu=" << dt_cpu << " dt_wall=" << dt_wall << LF;
}

void
tm_server_rep::interpose_handler () {
#ifdef QTTEXMACS
  // TeXmacs/Qt handles delayed messages and socket notification
  // in its own runloop
  //server_listen_connections (0);
  //client_listen_connections (0);
#ifndef QTPIPES
  perform_select ();
#endif
  process_all_pipes ();
#else
  perform_select ();
  exec_pending_commands ();
#endif

  if (!headless_mode) {
    int i, j;
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
  sync_databases ();
  idle_monitor_tick ();
  schedule_tick ();
}

/******************************************************************************
* Periodic task scheduler
******************************************************************************/

void
tm_server_rep::add_on_idle_task (string cmd, long interval_ms) {
  scheduled_task t;
  t.cmd         = cmd;
  t.interval_ms = interval_ms;
  t.next_run    = texmacs_time () + interval_ms;
  task_schedule << t;
}

void
tm_server_rep::schedule_tick () {
  if (idle_acc < IDLE_CONSECUTIVE_NEEDED) return;

  time_t now= texmacs_time ();
  for (int i = 0; i < N(task_schedule); i++) {
    scheduled_task& t= task_schedule[i];
    if ((now - t.next_run) < 0) continue; // not yet due
    t.next_run= now + t.interval_ms;
    idle_acc= 0; // reset so we don't launch another immediately
    exec_delayed (scheme_cmd (t.cmd));
    return; // one task at a time
  }
}

void
tm_add_on_idle_task (string cmd, int interval_ms) {
  if (is_server_started ())
    get_server () -> add_on_idle_task (cmd, (long) interval_ms);
}

void
tm_server_rep::wait_handler (string message, string arg) {
  if (has_current_window ())
    show_wait_indicator (concrete_window () -> win, translate (message), arg);
  else
    cout << "TeXmacs] Please wait: " << message << " " << arg << "\n";
}

void
tm_server_rep::set_script_status (int i) {
  script_status= i;
}

void
tm_server_rep::set_printing_command (string cmd) {
  ::set_printing_cmd (cmd);
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
tm_server_rep::set_default_zoom_factor (double zoom) {
  if (zoom >= 25.0 ) zoom= 25.0;
  if (zoom <=  0.04) zoom=  0.04;
  zoom= normal_zoom (zoom);
  def_zoomf= zoom;
}

double
tm_server_rep::get_default_zoom_factor () {
  return def_zoomf;
}

void
tm_server_rep::inclusions_gc (string which) {
  (void) which;
  reset_inclusions ();
  typeset_update_all ();
}

void
tm_server_rep::typeset_update (path p) {
  array<url> vs= get_all_views ();
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> typeset_invalidate (p);
}

void
tm_server_rep::typeset_update_all () {
  array<url> vs= get_all_views ();
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> typeset_invalidate_all ();
}

bool
tm_server_rep::is_yes (string s) {
  s= locase_all (s);
  string st= locase_all (translate ("yes"));
  return tm_forward_access (s, 0) == tm_forward_access (st, 0) || s == st;
}

static void
quit_texmacs_internal (int code) {
  close_all_pipes ();
  call ("quit-TeXmacs-scheme");
  clear_pending_commands ();
#ifdef QTTEXMACS
  del_obj_qt_renderer ();
#endif

#ifdef ADVANCED_DEVELOPER_MODE
  // Crashes sometimes occur when destructing Qt objects at exit.
  // Developers are invited to investigate this issue.
  // An example where it crashes with macOS SDK 14 and qt-6.8.2:
  //   open texmacs, write something in the buffer, close texmacs,
  //   and confirm exit in the lower status bar.
  exit (code);
#else
  _exit (code);
#endif
}

void
tm_server_rep::quit () {
  quit_texmacs_internal (0);
}

void
quit_TeXmacs_code (int code) {
  quit_texmacs_internal (code);
}

/******************************************************************************
* System commands
******************************************************************************/

void
tm_server_rep::shell (string s) {
  system (s);
}
