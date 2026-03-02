
/******************************************************************************
* MODULE     : tm_server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_SERVER_H
#define TM_SERVER_H
#include "tm_timer.hpp"
#include "tm_config.hpp"
#include "tm_buffer.hpp"
#include "tm_frame.hpp"
#include "tm_data.hpp"
bool is_server_started ();

/******************************************************************************
* Scheduled task descriptor
******************************************************************************/

struct scheduled_task {
  string  cmd;         // Scheme expression to evaluate
  long    interval_ms; // repeat interval in milliseconds
  time_t  next_run;    // texmacs_time() value at which the task next fires
};

/******************************************************************************
* tm_server_rep
******************************************************************************/

class tm_server_rep:
  public tm_config_rep,
  public tm_frame_rep
{
protected:
  double def_zoomf;          // default zoom factor
  long   idle_last_cpu_ms;   // cumulative CPU time at last idle check (ms)
  time_t idle_last_check_ms; // wall time of last idle check (ms since start)
  int    idle_acc;           // consecutive idle polls so far

  array<scheduled_task> task_schedule; // registered periodic tasks

public:
  tm_server_rep ();
  ~tm_server_rep ();
  server_rep* get_server ();

  /* Miscellaneous routines */
  void   style_clear_cache ();
  void   refresh ();
  void   interpose_handler ();
  void   idle_monitor_tick ();
  void   schedule_tick ();
  void   add_on_idle_task (string cmd, long interval_ms);
  void   wait_handler (string message, string arg);
  void   set_script_status (int i);
  void   set_printing_command (string s);
  void   set_printer_page_type (string s);
  string get_printer_page_type ();
  void   set_printer_dpi (string dpi);
  void   set_default_zoom_factor (double zoom);
  double get_default_zoom_factor ();
  void   inclusions_gc (string which);
  void   typeset_update (path p);
  void   typeset_update_all ();
  bool   is_yes (string s);
  void   quit ();
  void   shell    (string s);
};

// Global wrapper callable from Scheme glue
void tm_add_on_idle_task (string cmd, int interval_ms);

#endif // defined TM_SERVER_H
