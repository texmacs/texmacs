
/******************************************************************************
* MODULE     : tm_data.hpp
* DESCRIPTION: Buffer management for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_DATA_H
#define TM_DATA_H
#include "server.hpp"
#include "tm_buffer.hpp"

class tm_data_rep: virtual public server_rep {
protected:
  array<tm_buffer> bufs;      // the buffers
  array<tree>      history;   // history for browsing
  int              hist_pos;  // position in history

  /* Low level buffer menu manipulation */
  int       find_buffer (url name);
  string    new_menu_name (url name);
  void      update_menu ();
  void      menu_insert_buffer (tm_buffer buf);
  void      menu_delete_buffer (tm_buffer buf);
  void      menu_focus_buffer (tm_buffer buf);

  /* Low level buffer manipulation */
  tm_buffer new_buffer (url name);
  tm_buffer new_buffer (url name, tree t);
  void      revert_buffer (url name, tree doc);
  void      delete_buffer (tm_buffer buf);
  void      set_name_buffer (url name);
  url       get_name_buffer ();

  /* Low level view manipulation */
  tm_view   new_view (url name);
  tm_view   get_passive_view (tm_buffer buf);
  void      delete_view (tm_view vw);
  void      attach_view (tm_window win, tm_view vw);
  void      detach_view (tm_view vw);

  /* Low level window manipulation */
  tm_window new_window (display dis, bool map_flag= true);
  bool      delete_view_from_window (tm_window win);
  void      delete_window (tm_window win);

  /* Other subroutines */
  void new_buffer_in_this_window (url name, tree t);
  void new_buffer_in_new_window (url name, tree t);
  tree make_document (tm_view vw, string fm= "texmacs");
  tm_buffer load_passive_buffer (url name);

public:
  tm_data_rep ();
  ~tm_data_rep ();

  /* Buffer management */
  void new_buffer ();
  void switch_to_buffer (int nr);
  void switch_to_buffer (url name);
  void switch_to_active_buffer (url name);
  void revert_buffer ();
  void kill_buffer ();
  void open_window ();
  void clone_window ();
  void kill_window ();
  void set_max_undo_depth (int i);
  int  get_max_undo_depth ();
  bool no_bufs ();
  bool no_name ();
  bool help_buffer ();
  void set_aux_buffer (string aux, url name, tree doc);
  void set_help_buffer (url name, tree doc);
  void browse_help (int delta);

  /* Project management */
  void project_attach (string prj_name);
  bool project_attached ();
  void project_update_menu ();

  /* File management */
  tree load_tree (url name, string fm);
  void load_buffer (url name, string fm, int where= 0, bool asf= false);
  void save_buffer (url name, string fm);
  void auto_save ();
  bool buffer_unsaved ();
  bool exists_unsaved_buffer ();
  void pretend_save_buffer ();
};

#endif // defined TM_DATA_H
