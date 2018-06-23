
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

class tm_server_rep:
  public tm_config_rep,
  public tm_frame_rep
{
protected:
  double def_zoomf; // default zoom factor

public:
  tm_server_rep ();
  ~tm_server_rep ();
  server_rep* get_server ();

  /* Miscellaneous routines */
  void   style_clear_cache ();
  void   refresh ();
  void   interpose_handler ();
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

#endif // defined TM_SERVER_H
