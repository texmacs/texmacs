
/******************************************************************************
* MODULE     : edit_interface.hpp
* DESCRIPTION: the interface for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_INTERFACE_H
#define EDIT_INTERFACE_H
#include "editor.hpp"
#include "timer.hpp"
#include "window.hpp"
#include "Event/attribute_event.hpp"

#define INPUT_NORMAL      0
#define INPUT_SEARCH      1
#define INPUT_REPLACE     2
#define INPUT_SPELL       3
#define INPUT_COMPLETE    4

string MODE_LANGUAGE (string mode);

class edit_interface_rep: virtual public editor_rep {
protected:
  int           env_change;    // which things have been changed ?
  time_t        last_change;   // time of last processed change
  time_t        last_update;   // time of last update of menu, icons and footer
  int           con_status;    // 0: inexistent, 1: waiting, 2: active
  string        con_name;      // name of connected application
  string        con_session;   // name of computer algebra session
  bool          full_screen;   // full screen mode ?
  bool          got_focus;     // do we have keyboard focus ?
  string        sh_s;          // current string for shorthands
  int           sh_len;        // length of translation
  window        popup_win;     // the current popup window
  string        message_l;     // a left message to display
  string        message_r;     // a right message to display
  int           sfactor;       // the current shrinking factor
  SI            pixel;         // sfactor*PIXEL
  rectangles    copy_always;   // for wiping out cursor
  int           input_mode;    // INPUT_NORMAL, INPUT_SEARCH, INPUT_REPLACE

protected:
  time_t        last_click;    // last click on left mouse button
  SI            last_x, last_y;
  bool          start_drag;
  bool          dragging;
  SI            start_x, start_y;
  SI            end_x, end_y;
  bool          made_selection;
  bool          table_selection;
  rectangles    selection_rects;
  rectangles    env_rects;
  cursor        oc;
  array<string> completions;
  string        completion_prefix;
  int           completion_pos;
  int           nr_mutators;

public:
  edit_interface_rep ();
  ~edit_interface_rep ();
  operator tree ();
  void suspend ();
  void resume ();
  display get_display ();
  widget  get_widget ();

  /* extern connections */
  void update_connection ();
  void connect ();
  void process_extern_input ();
  void process_mutators ();
  path get_mutator_path ();
  void feed_input (tree t);
  bool busy_connection ();
  void interrupt_connection ();
  void stop_connection ();

  /* routines for dealing with shrinked coordinates */
  void set_shrinking_factor (int sf);
  void invalidate (SI x1, SI y1, SI x2, SI y2);
  void invalidate (rectangles rs);
  void get_visible (SI& x1, SI& y1, SI& x2, SI& y2);
  SI   get_window_height ();
  void scroll_to (SI x, SI y1);
  void set_extents (SI x1, SI y1, SI x2, SI y2);

  /* repainting the window */
  void draw_text (repaint_event ev);
  void draw_surround (ps_device dev, SI X1, SI Y1, SI X2, SI Y2);
  void draw_context (repaint_event ev);
  void draw_env (ps_device dev);
  void draw_cursor (ps_device dev);
  void draw_selection (ps_device dev);

  /* handle changes */
  void notify_change (int changed);
  bool has_changed (int question);
  void cursor_visible ();
  void selection_visible ();
  void apply_changes ();

  /* miscellaneous */
  bool kbd_get_command (string which, string& help, command& cmd);
  void full_screen_mode (bool flag);
  void compute_env_rects (path p, rectangles& rs, bool recurse);
  void before_menu_action ();
  void after_menu_action ();

  /* keyboard handling */
  int  get_input_mode ();
  void set_input_mode (int mode);
  void set_input_normal ();
  bool in_normal_mode ();
  bool in_search_mode ();
  bool in_replace_mode ();
  bool in_spell_mode ();
  bool try_shortcut (string comb);
  void key_press (string key);
  void emulate_keyboard (string keys, string action= "");
  void show_keymaps ();
  bool complete_try ();
  void complete_message ();
  void complete_start (string prefix, array<string> compls);
  bool complete_keypress (string key);

  /* mouse handling */
  void mouse_any (string s, SI x, SI y, time_t t);
  void mouse_click (SI x, SI y);
  bool mouse_extra_click (SI x, SI y);
  void mouse_drag (SI x, SI y);
  void mouse_select (SI x, SI y);
  void mouse_paste (SI x, SI y);
  void mouse_adjust (SI x, SI y);
  void mouse_scroll (SI x, SI y, bool up);
  cursor get_cursor ();

  /* the footer */
  string compute_text_footer (tree st);
  string compute_operation_footer (tree st);
  string compute_compound_footer (tree t, path p);
  bool   set_latex_footer (tree st);
  bool   set_hybrid_footer (tree st);
  void   set_left_footer (string l);
  void   append_left_footer (string& s, string env_var);
  void   set_left_footer ();
  void   set_right_footer (string r);
  void   set_right_footer ();
  void   set_footer ();
  void   set_message (string l, string r= "");
  void   interactive (scheme_tree args, object cmd);

  /* event handlers */
  void handle_get_size (get_size_event ev);
  void handle_attach_window (attach_window_event ev);
  void handle_clear (clear_event ev);
  void handle_repaint (repaint_event ev);
  void handle_keypress (keypress_event ev);
  void handle_mouse (mouse_event ev);
  void handle_keyboard_focus (keyboard_focus_event ev);
  void handle_resize (resize_event ev);
  void handle_set_integer (set_integer_event ev);

  friend class interactive_command_rep;
  friend class tm_window_rep;
  friend class tm_project_rep;
};

#endif // defined EDIT_INTERFACE_H
