
/******************************************************************************
* MODULE     : server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SERVER_H
#define SERVER_H
#include "editor.hpp"
#include "tm_widget.hpp"
#include "url.hpp"

class tm_buffer_rep;
class tm_view_rep;
class tm_window_rep;
typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;
typedef tm_window_rep* tm_window;
class tm_widget;
class editor;

class server_rep: public abstract_struct {
public:
  server_rep ();
  virtual ~server_rep ();

  /* Control global server parameters */
  virtual void   set_input_language (string s) = 0;
  virtual void   set_output_language (string s) = 0;
  virtual string get_input_language () = 0;
  virtual string get_output_language () = 0;
  virtual void   set_font_rules (scheme_tree rules) = 0;
  virtual bool   kbd_get_command (string s, string& help, command& cmd) = 0;
  virtual void   insert_kbd_wildcard (string key, string im,
				      bool post, bool l, bool r) = 0;
  virtual string kbd_pre_rewrite (string l) = 0;
  virtual string kbd_post_rewrite (string l) = 0;
  virtual void   set_variant_keys (string var, string unvar) = 0;
  virtual void   get_keycomb (string& s, int& status,
			      command& cmd, string& sh, string& help) = 0;

  /* Guile scheme motor */
  virtual bool exec_file (url u) = 0;
  virtual void exec_delayed (object cmd) = 0;
  virtual string preference (string var) = 0;

  /* TeXmacs layout */
  virtual void menu_widget (string menu, widget& w) = 0;
  virtual void menu_main (string menu) = 0;
  virtual void menu_icons (int which, string menu) = 0;
  virtual void show_header (bool flag) = 0;
  virtual void show_icon_bar (int which, bool flag) = 0;
  virtual void show_footer   (bool flag) = 0;
  virtual bool visible_header () = 0;
  virtual bool visible_icon_bar (int which) = 0;
  virtual bool visible_footer () = 0;
  virtual void set_shrinking_factor (int sf) = 0;
  virtual int  get_shrinking_factor () = 0;

  /* Buffer management */
  virtual int  nr_bufs () = 0;
  virtual tm_buffer get_buf (int i) = 0;
  virtual tm_buffer get_buf (path p) = 0;
  virtual void set_name_buffer (url name) = 0;
  virtual url  get_name_buffer () = 0;
  virtual url  get_name_buffer (path p) = 0;
  virtual void set_abbr_buffer (string abbr) = 0;
  virtual string get_abbr_buffer () = 0;
  virtual void new_buffer () = 0;
  virtual void switch_to_buffer (url name) = 0;
  virtual bool switch_to_buffer (path p) = 0;
  virtual void switch_to_active_buffer (url name) = 0;
  virtual void revert_buffer () = 0;
  virtual void kill_buffer () = 0;
  virtual void new_buffer_in_new_window (url name, tree t, tree geom= "") = 0;
  virtual void open_window (tree geom= "") = 0;
  virtual void clone_window () = 0;
  virtual void kill_window () = 0;
  virtual void kill_window_and_buffer () = 0;
  virtual void set_max_undo_depth (int i) = 0;
  virtual int  get_max_undo_depth () = 0;
  virtual bool no_bufs () = 0;
  virtual bool no_name () = 0;
  virtual bool help_buffer () = 0;
  virtual void revert_buffer (url name, tree doc) = 0;
  virtual void set_aux (string aux, url name) = 0;
  virtual void set_aux_buffer (string aux, url name, tree doc) = 0;
  virtual void set_help_buffer (url name, tree doc) = 0;
  virtual void set_buffer_tree (url name, tree doc) = 0;
  virtual tree get_buffer_tree (url name) = 0;
  virtual object get_buffer_menu () = 0;
  virtual bool buffer_in_menu (url name, bool flag) = 0;

  /* Projects */
  virtual void project_attach (string prj_name= "") = 0;
  virtual bool project_attached () = 0;
  virtual object get_project_buffer_menu () = 0;

  /* Loading and saving files */
  virtual tree load_tree (url name, string f) = 0;
  virtual void load_buffer (url name, string f, int w=0, bool a=false)=0;
  virtual void save_buffer (url name, string fm) = 0;
  virtual void auto_save () = 0;
  //virtual void delayed_autosave () = 0;
  virtual bool buffer_unsaved () = 0;
  virtual bool exists_unsaved_buffer () = 0;
  virtual void pretend_save_buffer () = 0;

  /* Get and set objects associated to server */
  virtual server_rep* get_server () = 0;
  virtual display     get_display () = 0;
  virtual bool        has_view () = 0;
  virtual tm_view     get_view (bool must_be_valid= true) = 0;
  virtual void        set_view (tm_view vw) = 0;
  virtual tm_buffer   get_buffer () = 0;
  virtual editor      get_editor () = 0;
  virtual tm_window   get_window () = 0;
  virtual tm_widget   get_meta () = 0;
  virtual color       get_color (string s) = 0;
  virtual int         get_nr_windows () = 0;

  virtual object get_style_menu () = 0;
  virtual object get_add_package_menu () = 0;
  virtual object get_remove_package_menu () = 0;
  virtual void style_clear_cache () = 0;
  virtual void style_set_cache (
            tree style, hashmap<string,tree> H, tree drd) = 0;
  virtual void style_get_cache (
	    tree style, hashmap<string,tree>& H, tree& drd, bool& flag) = 0;

  /* getting window properties */
  virtual int  get_window_id () = 0;
  virtual void set_window_property (scheme_tree what, scheme_tree val) = 0;
  virtual void set_bool_window_property (string what, bool val) = 0;
  virtual void set_int_window_property (string what, int val) = 0;
  virtual void set_string_window_property (string what, string val) = 0;
  virtual scheme_tree get_window_property (scheme_tree what) = 0;
  virtual bool get_bool_window_property (string what) = 0;
  virtual int get_int_window_property (string what) = 0;
  virtual string get_string_window_property (string what) = 0;

  /* Routines concerning the current editor widget */
  virtual void get_visible (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void scroll_where (SI& x, SI& y) = 0;
  virtual void scroll_to (SI x, SI y) = 0;
  virtual void set_extents (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void set_left_footer (string s) = 0;
  virtual void set_right_footer (string s) = 0;
  virtual void set_message (string left, string right, bool temp= false) = 0;
  virtual void recall_message () = 0;
  virtual void dialogue_start (string name, widget wid) = 0;
  virtual void dialogue_inquire (int i, string& arg) = 0;
  virtual void dialogue_end () = 0;
  virtual void choose_file (object fun, string title, string type) = 0;
  virtual void interactive (object fun, scheme_tree p) = 0;
  virtual void full_screen_mode (bool on, bool edit) = 0;
  virtual bool in_full_screen_mode () = 0;
  virtual bool in_full_screen_edit_mode () = 0;

  /* Misscelaneous routines */
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
  virtual string translate (string which, string from, string to) = 0;
  virtual bool   is_yes (string s) = 0;
  virtual void   quit () = 0;
  virtual tree   evaluate (string name, string session, tree expr) = 0;
  virtual void   shell    (string s) = 0;
};

class server {
  ABSTRACT(server);
  server (display dis);
};
ABSTRACT_CODE(server);

scheme_tree menu_merge (scheme_tree m1, scheme_tree m2);
server get_server ();

#endif // defined SERVER_H
