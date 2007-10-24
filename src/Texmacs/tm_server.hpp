
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
#include "tm_frame.hpp"
#include "tm_data.hpp"

class tm_server_rep:
  public tm_config_rep,
  public tm_data_rep,
  public tm_frame_rep
{
protected:
  tm_view vw;             // current editor
  int def_sfactor;        // default shrinking factor

  hashmap<tree,hashmap<string,tree> > style_cache; // style environments cache
  hashmap<tree,tree> style_drd;                    // style drd cache

public:
  tm_server_rep ();
  ~tm_server_rep ();

  /* Get and set objects associated to server */
  server_rep* get_server ();
  bool        has_view ();
  bool        has_window ();
  tm_view     get_view (bool must_be_valid= true);
  void        set_view (tm_view vw);
  tm_buffer   get_buffer ();
  editor      get_editor ();
  tm_window   get_window ();
  color       get_color (string s);
  int         get_nr_windows ();

  /* Caching style files */
  object get_style_menu ();
  object get_add_package_menu ();
  object get_remove_package_menu ();
  void style_clear_cache ();
  void style_set_cache (tree st, hashmap<string,tree> H, tree t);
  void style_get_cache (tree st, hashmap<string,tree>& H, tree& t, bool& flag);

  /* Miscellaneous routines */
  void   interpose_handler ();
  void   wait_handler (string message, string arg);
  void   set_script_status (int i);
  void   focus_on_editor (editor ed);
  void   set_printing_command (string s);
  void   set_printer_page_type (string s);
  string get_printer_page_type ();
  void   set_printer_dpi (string dpi);
  void   set_default_shrinking_factor (int sf);
  int    get_default_shrinking_factor ();
  void   image_gc (string which);
  void   inclusions_gc (string which);
  void   typeset_update (path p);
  void   typeset_update_all ();
  string translate (string which, string from, string to);
  bool   is_yes (string s);
  void   quit ();
  tree   evaluate (string name, string session, tree expr);
  void   shell    (string s);
};

#endif // defined TM_SERVER_H
