
/******************************************************************************
* MODULE     : tm_data.hpp
* DESCRIPTION: Buffer management for TeXmacs server
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_DATA_H
#define TM_DATA_H
#include "server.hpp"
#include "tm_window.hpp"

class tm_data_rep: virtual public server_rep {
protected:
  array<tm_buffer> bufs;      // the buffers

  /* Low level buffer menu manipulation */
  int       find_buffer (path p);
  int       find_buffer (url name);
  string    new_menu_name (url name);
  void      menu_insert_buffer (tm_buffer buf);
  void      menu_delete_buffer (tm_buffer buf);
  void      menu_focus_buffer (tm_buffer buf);

  /* Low level buffer manipulation */
  tm_buffer new_buffer (url name);
  tm_buffer new_buffer (url name, tree t);
  void      delete_buffer (tm_buffer buf);
  void      set_name_buffer (url name);
  url       get_name_buffer ();
  url       get_name_buffer (path p);
  void      set_abbr_buffer (string abbr);
  string    get_abbr_buffer ();

  /* Low level view manipulation */
  tm_view   new_view (url name);
  tm_view   get_passive_view (tm_buffer buf);
  void      delete_view (tm_view vw);
  void      attach_view (tm_window win, tm_view vw);
  void      detach_view (tm_view vw);

  /* Low level window manipulation */
  tm_window new_window (bool map_flag= true, tree geom= "");
  bool      delete_view_from_window (tm_window win);
  void      delete_window (tm_window win);

  /* Other subroutines */
  void new_buffer_in_this_window (url name, tree t);
  void new_buffer_in_new_window (url name, tree t, tree geom= "");
  tree make_document (tm_view vw, string fm= "texmacs");
  tm_buffer load_passive_buffer (url name);

public:
  tm_data_rep ();
  ~tm_data_rep ();

  /* Buffer management */
  int  nr_bufs ();
  tm_buffer get_buf (int i);
  tm_buffer get_buf (path p);
  url  new_buffer ();
  void switch_to_buffer (int nr);
  bool switch_to_buffer (path p);
  void switch_to_buffer (url name);
  void switch_to_active_buffer (url name);
  void revert_buffer ();
  void kill_buffer ();
  url  open_window (tree geom= "");
  void clone_window ();
  void kill_window ();
  void kill_window_and_buffer ();
  bool no_bufs ();
  bool no_name ();
  bool help_buffer ();
  void revert_buffer (url name, tree doc);
  void set_aux (string aux, url name);
  void set_aux_buffer (string aux, url name, tree doc);
  void set_help_buffer (url name, tree doc);
  void browse_help (int delta);
  void set_buffer_tree (url name, tree doc);
  tree get_buffer_tree (url name);
  url  get_all_buffers ();
  object get_buffer_menu ();
  bool buffer_in_menu (url name, bool flag);

  /* Project management */
  void project_attach (string prj_name);
  bool project_attached ();
  object get_project_buffer_menu ();

  /* Window management */
  int  window_current ();
  path windows_list ();
  path buffer_to_windows (url name);
  url  window_to_buffer (int id);
  tm_view window_find_view (int id);
  void window_set_buffer (int id, url name);
  void window_focus (int id);

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
