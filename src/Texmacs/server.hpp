
/******************************************************************************
* MODULE     : server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SERVER_H
#define SERVER_H
#include "editor.hpp"
#include "url.hpp"
#include "scheme.hpp"

class tm_buffer_rep;
class tm_view_rep;
class tm_window_rep;
typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;
typedef tm_window_rep* tm_window;
class editor;

class server_rep: public abstract_struct {
public:
  server_rep ();
  virtual ~server_rep ();

  /* Get and set objects associated to server */
  virtual server_rep* get_server () = 0;
  virtual bool        has_view () = 0;
  virtual bool        has_window () = 0;
  virtual tm_view     get_view (bool must_be_valid= true) = 0;
  virtual void        set_view (tm_view vw) = 0;
  virtual tm_buffer   get_buffer () = 0;
  virtual editor      get_editor () = 0;
  virtual tm_window   get_window () = 0;
  virtual int         get_nr_windows () = 0;

  /* Control global server parameters */
  virtual void   set_font_rules (scheme_tree rules) = 0;
  virtual bool   kbd_get_command (string s, string& help, command& cmd) = 0;
  virtual void   insert_kbd_wildcard (string key, string im,
				      bool post, bool l, bool r) = 0;
  virtual string kbd_pre_rewrite (string l) = 0;
  virtual string kbd_post_rewrite (string l, bool var_flag= true) = 0;
  virtual tree   kbd_system_rewrite (string l) = 0;
  virtual void   set_variant_keys (string var, string unvar) = 0;
  virtual void   get_keycomb (string& s, int& status,
			      command& cmd, string& sh, string& help) = 0;

  /* TeXmacs frames */
  virtual int  get_window_id () = 0;
  virtual void set_window_property (scheme_tree what, scheme_tree val) = 0;
  virtual void set_bool_window_property (string what, bool val) = 0;
  virtual void set_int_window_property (string what, int val) = 0;
  virtual void set_string_window_property (string what, string val) = 0;
  virtual scheme_tree get_window_property (scheme_tree what) = 0;
  virtual bool get_bool_window_property (string what) = 0;
  virtual int get_int_window_property (string what) = 0;
  virtual string get_string_window_property (string what) = 0;

  virtual void show_header (bool flag) = 0;
  virtual void show_icon_bar (int which, bool flag) = 0;
  virtual void show_side_tools (int which, bool flag) = 0;
  virtual bool visible_header () = 0;
  virtual bool visible_icon_bar (int which) = 0;
  virtual bool visible_side_tools (int which) = 0;
  virtual void menu_widget (string menu, widget& w) = 0;
  virtual void menu_main (string menu) = 0;
  virtual void menu_icons (int which, string menu) = 0;
  virtual void side_tools (int which, string menu) = 0;

  virtual void set_shrinking_factor (int sf) = 0;
  virtual int  get_shrinking_factor () = 0;
  virtual void set_scrollbars (int sb) = 0;
  virtual void get_visible (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void scroll_where (SI& x, SI& y) = 0;
  virtual void scroll_to (SI x, SI y) = 0;
  virtual void set_extents (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void get_extents (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void full_screen_mode (bool on, bool edit) = 0;
  virtual bool in_full_screen_mode () = 0;
  virtual bool in_full_screen_edit_mode () = 0;

  virtual void show_footer   (bool flag) = 0;
  virtual bool visible_footer () = 0;
  virtual void set_left_footer (string s) = 0;
  virtual void set_right_footer (string s) = 0;
  virtual void set_message (tree left, tree right, bool temp= false) = 0;
  virtual void recall_message () = 0;
  virtual void dialogue_start (string name, widget wid) = 0;
  virtual void dialogue_inquire (int i, string& arg) = 0;
  virtual void dialogue_end () = 0;
  virtual void choose_file (object fun, string title, string type) = 0;
  virtual void interactive (object fun, scheme_tree p) = 0;

  /* Miscellaneous routines */
  virtual void   style_clear_cache () = 0;
  virtual void   refresh () = 0;
  virtual void   interpose_handler () = 0;
  virtual void   wait_handler (string message, string arg) = 0;
  virtual void   set_script_status (int i) = 0;
  virtual void   focus_on_editor (editor ed) = 0;
  virtual void   set_printing_command (string s) = 0;
  virtual void   set_printer_page_type (string s) = 0;
  virtual string get_printer_page_type () = 0;
  virtual void   set_printer_dpi (string dpi) = 0;
  virtual void   set_default_shrinking_factor (int sf) = 0;
  virtual int    get_default_shrinking_factor () = 0;
  virtual void   image_gc (string which= "*") = 0;
  virtual void   inclusions_gc (string which= "*") = 0;
  virtual void   typeset_update (path p) = 0;
  virtual void   typeset_update_all () = 0;
  virtual bool   is_yes (string s) = 0;
  virtual void   quit () = 0;
  virtual void   shell (string s) = 0;
};

class server {
  ABSTRACT(server);
  server ();
};
ABSTRACT_CODE(server);

extern bool rescue_mode;
scheme_tree menu_merge (scheme_tree m1, scheme_tree m2);
server get_server ();
void gui_set_output_language (string lan);

/* low level */
tm_buffer create_buffer (url name, tree doc);
int find_buffer (path p);
int find_buffer (url name);
void attach_view (tm_window win, tm_view vw);
void detach_view (tm_view vw);
tm_view get_passive_view (tm_buffer buf);
void delete_view (tm_view vw);
void new_buffer_in_this_window (url name, tree t);
tm_window new_window (bool map_flag= true, tree geom= "");
tm_buffer load_passive_buffer (url name);
tree make_document (tm_view vw, string fm= "texmacs");

/* Buffer management */
int  number_buffers ();
url  get_all_buffers ();
url  get_this_buffer ();
void remove_buffer (url name);
url  get_name_buffer (path p);
void rename_buffer (url name, url new_name);
url get_master_buffer (url name);
void set_master_buffer (url name, url master);
void set_title_buffer (url name, string title);
string get_title_buffer (url name);
void set_buffer_tree (url name, tree doc);
tree get_buffer_tree (url name);
void set_buffer_body (url name, tree body);
tree get_buffer_body (url name);
void new_buffer_in_new_window (url name, tree t, tree geom= "");
double last_visited (url name);
bool buffer_modified (url name);
void pretend_buffer_saved (url name);
bool buffer_has_name (url name);

/* Buffer management */
url  create_buffer ();
void switch_to_buffer (int nr);
bool switch_to_buffer (path p);
void switch_to_buffer (url name);
void revert_buffer ();
void kill_buffer ();
url  open_window (tree geom= "");
void clone_window ();
void kill_window ();
void kill_window_and_buffer ();
bool is_aux_buffer (url name);

/* Project management */
void project_attach (string prj_name= "");
bool project_attached ();
url  project_get ();

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

#endif // defined SERVER_H
