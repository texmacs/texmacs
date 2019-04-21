
/******************************************************************************
* MODULE     : edit_interface.hpp
* DESCRIPTION: the interface for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_INTERFACE_H
#define EDIT_INTERFACE_H
#include "editor.hpp"
#include "tm_timer.hpp"
#include "widget.hpp"

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
  double        anim_next;     // time for next animation
  bool          full_screen;   // full screen mode ?
  bool          got_focus;     // do we have keyboard focus ?
  string        sh_s;          // current string for shortcuts
  double        sh_mark;       // 0 or mark for undoing shortcut
  bool          pre_edit_skip; // temporarily disabled pre-edit mechanism
  string        pre_edit_s;    // pre-edit string
  double        pre_edit_mark; // 0 or mark for undoing pre-edit
  widget        popup_win;     // the current popup window
  tree          message_l;     // a left message to display
  tree          message_r;     // a right message to display
  tree          last_l;        // last displayed left message
  tree          last_r;        // last displayed right message
  double        zoomf;         // the current zoom factor
  double        magf;          // the current magnification factor
  SI            pixel;         // current size of a pixel on the screen
  rectangles    copy_always;   // for wiping out cursor
  int           input_mode;    // INPUT_NORMAL, INPUT_SEARCH, INPUT_REPLACE

protected:
  SI            last_x, last_y;
  time_t        last_t;
  SI            start_x, start_y;
  SI            end_x, end_y;
  bool          table_selection;
  int           mouse_adjusting;  // mask with active key modifiers upon click
  rectangles    selection_rects;
  array<rectangles> alt_selection_rects;
  rectangle     last_visible;
  rectangles    env_rects;
  rectangles    foc_rects;
  rectangles    sem_rects;
  bool          sem_correct;
  cursor        oc;
  bool          temp_invalid_cursor;
  array<string> completions;
  string        completion_prefix;
  int           completion_pos;
  renderer      shadow;
  SI            vx1, vy1, vx2, vy2;
  rectangles    stored_rects;
  renderer      stored;
  rectangles    locus_new_rects;
  rectangles    locus_rects;
  list<string>  mouse_ids;
  list<string>  focus_ids;
  int           cur_sb, cur_wb;
  SI            cur_wx, cur_wy;
  rectangles    keys_rects;

public:
  edit_interface_rep ();
  ~edit_interface_rep ();
  operator tree ();
  void suspend ();
  void resume ();
  void keyboard_focus_on (string field);
  void get_size (SI& wx, SI& wy);

  /* routines for dealing with shrinked coordinates */
  int  get_pixel_size ();
  SI   get_visible_width ();
  SI   get_visible_height ();
  SI   get_window_width ();
  SI   get_window_height ();
  void set_zoom_factor (double zoom);
  void invalidate (SI x1, SI y1, SI x2, SI y2);
  void invalidate (rectangles rs);
  void invalidate_all ();
  void update_visible ();
  void scroll_to (SI x, SI y1);
  void set_extents (SI x1, SI y1, SI x2, SI y2);

  /* repainting the window */
  void draw_background (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void draw_text (renderer ren, rectangles& l);
  void draw_surround (renderer ren, rectangle r);
  void draw_context (renderer ren, rectangle r);
  void draw_env (renderer ren);
  void draw_cursor (renderer ren);
  void draw_selection (renderer ren, rectangle r);
  void draw_graphics (renderer ren);
  void draw_keys (renderer ren);
  void draw_pre (renderer win, renderer ren, rectangle r);
  void draw_post (renderer win, renderer ren, rectangle r);
  void draw_with_shadow (renderer win, rectangle r);
  void draw_with_stored (renderer win, rectangle r);

  /* handle changes */
  void notify_change (int changed);
  bool has_changed (int question);
  int  idle_time (int event_type= ANY_EVENT);
  int  change_time ();
  void update_menus ();
  int  find_alt_selection_index (range_set alt_sel, SI y, int b, int e);
  void apply_changes ();
  void animate ();

  /* miscellaneous */
  void compute_env_rects (path p, rectangles& rs, bool recurse);
  void cursor_visible ();
  void selection_visible ();
  void full_screen_mode (bool flag);
  void before_menu_action ();
  void after_menu_action ();
  void cancel_menu_action ();
  cursor search_cursor (path p);
  selection search_selection (path start, path end);
  rectangle get_window_extents ();

  /* keyboard handling */
  int  get_input_mode ();
  void set_input_mode (int mode);
  void set_input_normal ();
  bool in_normal_mode ();
  bool in_search_mode ();
  bool in_replace_mode ();
  bool in_spell_mode ();
  bool kbd_get_command (string which, string& help, command& cmd);
  void interrupt_shortcut ();
  bool try_shortcut (string comb);
  tree kbd (string s);
  tree kbd_shortcut (string s);
  void key_press (string key);
  void emulate_keyboard (string keys, string action= "");
  bool complete_try ();
  void complete_message ();
  void complete_start (string prefix, array<string> compls);
  bool complete_keypress (string key);
  string session_complete_command (tree t);
  void custom_complete (tree t);

  /* mouse handling */
  void mouse_any (string s, SI x, SI y, int mods, time_t t);
  void mouse_click (SI x, SI y);
  bool mouse_extra_click (SI x, SI y);
  void mouse_drag (SI x, SI y);
  void mouse_select (SI x, SI y, int mods, bool drag);
  void mouse_paste (SI x, SI y);
  void mouse_adjust (SI x, SI y, int mods);
  void mouse_adjust_selection (SI x, SI y, int mods);
  void mouse_scroll (SI x, SI y, bool up);
  cursor get_cursor ();
  array<SI> get_mouse_position ();
  void set_pointer (string name);
  void set_pointer (string curs_name, string mask_name);
  void update_mouse_loci ();
  void update_focus_loci ();

  /* the footer */
  tree compute_text_footer (tree st);
  tree compute_operation_footer (tree st);
  tree compute_compound_footer (tree t, path p);
  bool set_latex_footer (tree st);
  bool set_hybrid_footer (tree st);
  void set_left_footer (tree l);
  void append_left_footer (tree& l, string env_var);
  void set_left_footer ();
  void set_right_footer (tree r);
  void set_right_footer ();
  void set_footer ();
  void set_message (tree l, tree r= "", bool temp= false);
  void recall_message ();

  /* event handlers */
  bool is_editor_widget ();
  void handle_get_size_hint (SI& w, SI& h);
  void handle_notify_resize (SI w, SI h);
  void handle_keypress (string key, time_t t);
  void handle_keyboard_focus (bool has_focus, time_t t);
  void handle_mouse (string kind, SI x, SI y, int mods, time_t t);
  void handle_set_zoom_factor (double zoomf);
  void handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2);
  void handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2);

  friend class interactive_command_rep;
  friend class tm_window_rep;
};

#endif // defined EDIT_INTERFACE_H
