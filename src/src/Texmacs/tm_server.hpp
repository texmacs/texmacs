
/******************************************************************************
* MODULE     : tm_server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_SERVER_H
#define TM_SERVER_H
#include "timer.hpp"
#include "tm_config.hpp"
#include "tm_buffer.hpp"
#include "tm_layout.hpp"
#include "tm_scheme.hpp"
#include "tm_data.hpp"

class tm_server_rep:
  public tm_config_rep,
  public tm_data_rep,
  public tm_layout_rep,
  public tm_scheme_rep
{
protected:
  display dis;       // current display
  tm_view vw;        // current editor
  int banner_nr;     // how far in banner
  bool full_screen;  // full screen mode
  int def_sfactor;   // default shrinking factor

  hashmap<tree,hashmap<string,tree> > style_cache; // style environments cache
  hashmap<tree,tree> style_drd;                    // style drd cache

public:
  tm_server_rep (display dis);
  ~tm_server_rep ();
  void advance_banner ();

  /* Get and set objects associated to server */
  server_rep* get_server ();
  display     get_display ();
  bool        has_view ();
  tm_view     get_view (bool must_be_valid= true);
  void        set_view (tm_view vw);
  tm_buffer   get_buffer ();
  editor      get_editor ();
  tm_window   get_window ();
  tm_widget   get_meta ();
  color       get_color (string s);
  int         get_nr_windows ();

  /* Caching style files */
  void style_update_menu ();
  void style_clear_cache ();
  void style_set_cache (tree st, hashmap<string,tree> H, tree t);
  void style_get_cache (tree st, hashmap<string,tree>& H, tree& t, bool& flag);

  /* Routines concerning the current editor widget */
  void get_visible (SI& x1, SI& y1, SI& x2, SI& y2);
  void scroll_where (SI& x, SI& y);
  void scroll_to (SI x, SI y);
  void set_extents (SI x1, SI y1, SI x2, SI y2);
  void set_left_footer (string s);
  void set_right_footer (string s);
  void set_message (string left, string right);
  void interactive (string name, string& s, command call_back);
  void full_screen_mode (bool on);
  bool in_full_screen_mode ();

  /* Miscellaneous routines */
  void   interpose_handler ();
  void   wait_handler (string message, string arg);
  void   set_script_status (int i);
  void   focus_on_editor (editor ed);
  void   set_printing_command (string s);
  void   set_printer_page_type (string s);
  void   set_printer_dpi (string dpi);
  void   set_default_shrinking_factor (int sf);
  int    get_default_shrinking_factor ();
  void   postscript_gc (string which);
  void   inclusions_gc (string which);
  string translate (string which, string from, string to);
  bool   is_yes (string s);
  void   quit ();
  tree   evaluate (string name, string session, tree expr);
  void   shell    (string s);
};

#endif // defined TM_SERVER_H
