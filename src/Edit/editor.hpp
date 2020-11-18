
/******************************************************************************
* MODULE     : editor.hpp
* DESCRIPTION: abstract TeXmacs editors
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDITOR_H
#define EDITOR_H
#include "typesetter.hpp"
#include "tree_select.hpp"
#ifdef AQUATEXMACS
#  include "Cocoa/aqua_simple_widget.h"
#else
#  ifdef QTTEXMACS
#    include "Qt/qt_simple_widget.hpp"
#  else
#    include "Widkit/simple_wk_widget.hpp"
#  endif
#endif
#include "server.hpp"
#include "drd_info.hpp"
#ifdef EXPERIMENTAL
#  include "../Style/Environment/environment.hpp"
#  include "../Style/Memorizer/memorizer.hpp"
#endif
#include "new_data.hpp"
#define TEXMACS_COPYRIGHT (string("(c) 1999-2020 by Joris van der Hoeven and others"))

#define THE_CURSOR 1
#define THE_FOCUS 2
#define THE_TREE 4
#define THE_ENVIRONMENT 8
#define THE_SELECTION 16
#define THE_DECORATIONS 32
#define THE_EXTENTS 64
#define THE_LOCUS 128
#define THE_MENUS 256
#define THE_FREEZE 512

class tm_buffer_rep;
class tm_view_rep;
class server_rep;
typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep* tm_view;
class modification;
class editor;
extern bool enable_fastenv;

class editor_rep: public simple_widget_rep {
public:
  server_rep*  sv;   // the underlying texmacs server
  widget_rep*  cvw;  // non reference counted canvas widget
  tm_view_rep* mvw;  // master view

protected:
  tm_buffer    buf;  // the underlying buffer
  drd_info     drd;  // the drd for the buffer
  tree&        et;   // all TeXmacs trees
  box          eb;   // box translation of tree
  path         rp;   // path to the root of the document in et
  path         tp;   // path of cursor in tree
#ifdef EXPERIMENTAL
  environment  ste;  // environment for style rewriting
  tree         cct;  // clean copy of the document tree
  memorizer    mem;  // style converted document tree
#endif

  /* exchanging information with the interface */
  virtual void      get_selection (path& start, path& end) = 0;
  virtual void      set_selection (path start, path end) = 0;
  virtual cursor&   the_cursor () = 0;
  virtual cursor&   the_ghost_cursor () = 0;

  /* exchanging information with the typesetter */
  virtual typesetter           get_typesetter () = 0;
  virtual hashmap<string,tree> get_init () = 0;
  virtual hashmap<string,tree> get_fin () = 0;
  virtual hashmap<string,tree> get_ref () = 0;
  virtual hashmap<string,tree> get_aux () = 0;
  virtual hashmap<string,tree> get_att () = 0;
  virtual void                 set_init (hashmap<string,tree> H= tree ("?"))=0;
  virtual void                 add_init (hashmap<string,tree> H) = 0;
  virtual void                 set_fin (hashmap<string,tree> H) = 0;
  virtual void                 set_ref (hashmap<string,tree> H) = 0;
  virtual void                 set_aux (hashmap<string,tree> H) = 0;
  virtual void                 set_att (hashmap<string,tree> H) = 0;

  /* exchanging property information */
  virtual void   set_bool_property (string what, bool val) = 0;
  virtual void   set_int_property (string what, int val) = 0;
  virtual void   set_string_property (string what, string val) = 0;
  virtual bool   get_bool_property (string what) = 0;
  virtual int    get_int_property (string what) = 0;
  virtual string get_string_property (string what) = 0;

protected:
  /* protected routines from edit_typeset */
  virtual void   typeset_preamble () = 0;
  virtual void   typeset_invalidate_env () = 0;
  virtual void   typeset (SI& x1, SI& y1, SI& x2, SI& y2) = 0;

  /* protected subroutines for deletion of content */
  virtual void back_around (tree t, path p, bool forward) = 0;
  virtual void back_prime (tree t, path p, bool forward) = 0;
  virtual void back_in_around (tree t, path p, bool forward) = 0;
  virtual void back_in_long_arrow (tree t, path p, bool forward) = 0;
  virtual void back_in_wide (tree t, path p, bool forward) = 0;
  virtual void back_in_tree (tree t, path p, bool forward) = 0;
  virtual void pre_remove_around (path p) = 0;
  virtual void back_table (path p, bool forward) = 0;
  virtual void back_in_table (tree t, path p, bool forward) = 0;
  virtual void back_monolithic (path p) = 0;
  virtual void back_general (path p, bool forward) = 0;
  virtual void back_in_with (tree t, path p, bool forward) = 0;
  virtual void back_in_general (tree t, path p, bool forward) = 0;
  virtual void back_in_text_at (tree t, path p, bool forward) = 0;

  /* other protected subroutines */
  virtual tree kbd (string s) = 0;
  virtual tree kbd_shortcut (string s) = 0;
  virtual path tree_path (path sp, SI x, SI y, SI delta) = 0;
  virtual void apply_changes () = 0;
  virtual void animate () = 0;
  virtual path search_format () = 0;
  virtual path search_format (int& row, int& col) = 0;
  virtual void table_get_extents (path fp, int& nr_rows, int& nr_cols) = 0;
  virtual void table_bound (path fp, int& i1, int& j1, int& i2, int& j2) = 0;
  virtual tree table_get_subtable (path p, int i1, int j1, int i2, int j2) = 0;
  virtual void table_write_subtable (path fp, int row, int col, tree subt) = 0;
  virtual void table_del_format (path fp, int I1, int J1,
				 int I2, int J2, string var) = 0;
  virtual void table_insert (path fp, int row, int col,
                             int nr_rows, int nr_cols) = 0;
  virtual void table_remove (path fp, int row, int col,
                             int nr_rows, int nr_cols) = 0;

public:
  editor_rep ();
  editor_rep (server_rep* sv, tm_buffer buf);
  inline virtual ~editor_rep () {}

  /* public routines from edit_interface */
  virtual void suspend () = 0;
  virtual void resume () = 0;
  virtual void keyboard_focus_on (string field) = 0;
  virtual void update_menus () = 0;
  virtual int  get_pixel_size () = 0;
  virtual SI   get_visible_width () = 0;
  virtual SI   get_visible_height () = 0;
  virtual SI   get_window_width () = 0;
  virtual SI   get_window_height () = 0;
  virtual SI   get_window_x () = 0;
  virtual SI   get_window_y () = 0;
  virtual SI   get_canvas_x () = 0;
  virtual SI   get_canvas_y () = 0;
  virtual SI   get_scroll_x () = 0;
  virtual SI   get_scroll_y () = 0;
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void invalidate (rectangles rs) = 0;
  virtual void invalidate_all () = 0;
  virtual void notify_change (int changed) = 0;
  virtual bool has_changed (int question) = 0;
  virtual int  idle_time (int event_type= ANY_EVENT) = 0;
  virtual int  change_time () = 0;
  virtual void full_screen_mode (bool flag) = 0;
  virtual void before_menu_action () = 0;
  virtual void after_menu_action () = 0;
  virtual void cancel_menu_action () = 0;
  virtual rectangle get_window_extents () = 0;
  virtual cursor search_cursor (path p) = 0;
  virtual selection search_selection (path start, path end) = 0;
  virtual int  get_input_mode () = 0;
  virtual void set_input_mode (int mode) = 0;
  virtual void set_input_normal () = 0;
  virtual bool in_normal_mode () = 0;
  virtual bool in_search_mode () = 0;
  virtual bool in_replace_mode () = 0;
  virtual bool in_spell_mode () = 0;
  virtual void interrupt_shortcut () = 0;
  virtual bool kbd_get_command (string cmd_s, string& help, command& cmd) = 0;
  virtual void key_press (string key) = 0;
  virtual void emulate_keyboard (string keys, string action= "") = 0;
  virtual bool complete_try () = 0;
  virtual void complete_start (string prefix, array<string> compls) = 0;
  virtual bool complete_keypress (string key) = 0;
  virtual string session_complete_command (tree t) = 0;
  virtual void custom_complete (tree t) = 0;
  virtual void mouse_any (string s, SI x, SI y, int mods, time_t t) = 0;
  virtual void mouse_click (SI x, SI y) = 0;
  virtual bool mouse_extra_click (SI x, SI y) = 0;
  virtual void mouse_drag (SI x, SI y) = 0;
  virtual void mouse_select (SI x, SI y, int mods, bool drag) = 0;
  virtual void mouse_paste (SI x, SI y) = 0;
  virtual void mouse_adjust (SI x, SI y, int mods) = 0;
  virtual void mouse_adjust_selection (SI x, SI y, int mods) = 0;
  virtual void mouse_scroll (SI x, SI y, bool up) = 0;
  virtual cursor get_cursor () = 0;
  virtual array<SI> get_mouse_position () = 0;
  virtual void set_pointer (string name) = 0;
  virtual void set_pointer (string curs_name, string mask_name) = 0;
  virtual void set_message (tree l, tree r= "", bool temp= false) = 0;
  virtual void recall_message () = 0;

  /* public routines from edit_cursor */
  virtual path make_cursor_accessible (path p, bool forwards) = 0;
  virtual bool cursor_is_accessible () = 0;
  virtual void show_cursor_if_hidden () = 0;
  virtual void go_to (SI x, SI y, bool absolute= true) = 0;
  virtual void go_left_physical () = 0;
  virtual void go_right_physical () = 0;
  virtual void go_left () = 0;
  virtual void go_right () = 0;
  virtual void go_up () = 0;
  virtual void go_down () = 0;
  virtual void go_start_line () = 0;
  virtual void go_end_line () = 0;
  virtual void go_page_up () = 0;
  virtual void go_page_down () = 0;
  virtual void go_to (path p) = 0;
  virtual void go_to_correct (path p) = 0;
  virtual void go_to_start (path p) = 0;
  virtual void go_to_end (path p) = 0;
  virtual void go_to_border (path p, bool at_start) = 0;
  virtual void go_to_here () = 0;
  virtual void go_start () = 0;
  virtual void go_end () = 0;
  virtual void go_start_of (tree_label what) = 0;
  virtual void go_end_of (tree_label what) = 0;
  virtual void go_start_with (string var, string val) = 0;
  virtual void go_end_with (string var, string val) = 0;
  virtual void go_start_paragraph () = 0;
  virtual void go_end_paragraph () = 0;
  virtual path search_label (string s, bool local= false) = 0;
  virtual void go_to_label (string s) = 0;
  virtual tree get_labels () = 0;

  /* public routines from edit_graphics */
  virtual bool   inside_graphics (bool b=true) = 0;
  virtual bool   inside_active_graphics (bool b=true) = 0;
  virtual bool   over_graphics (SI x, SI y) = 0;
  virtual tree   get_graphics () = 0;
  virtual double get_x () = 0;
  virtual double get_y () = 0;
  virtual frame  find_frame (bool last= false) = 0;
  virtual grid   find_grid () = 0;
  virtual void   find_limits (point& lim1, point& lim2) = 0;
  virtual bool   find_graphical_region (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual point  adjust (point p) = 0;
  virtual tree   find_point (point p) = 0;
  virtual tree   graphical_select (double x, double y) = 0;
  virtual tree   graphical_select (double x1, double y1,
				   double x2, double y2) = 0;
  virtual tree   get_graphical_object () = 0;
  virtual void   set_graphical_object (tree t) = 0;
  virtual void   invalidate_graphical_object () = 0;
  virtual void   draw_graphical_object (renderer ren) = 0;
  virtual bool   mouse_graphics (string s, SI x, SI y, int mods, time_t t) = 0;

  /* public routines from edit_typeset */
  virtual void     clear_local_info () = 0;
  virtual void     set_data (new_data data) = 0;
  virtual void     get_data (new_data& data) = 0;
  virtual SI       as_length (string l) = 0;
  virtual string   add_lengths (string l1, string l2) = 0;
  virtual string   sub_lengths (string l1, string l2) = 0;
  virtual string   max_lengths (string l1, string l2) = 0;
  virtual string   min_lengths (string l1, string l2) = 0;
  virtual string   multiply_length (double x, string l) = 0;
  virtual bool     is_length (string s) = 0;
  virtual double   divide_lengths (string l1, string l2) = 0;
  virtual void     init_update () = 0;
  virtual void     drd_update () = 0;
#ifdef EXPERIMENTAL
  virtual void     environment_update () = 0;
#endif
  virtual tree     get_full_env () = 0;
  virtual bool     defined_at_cursor (string var_name) = 0;
  virtual bool     defined_at_init (string var_name) = 0;
  virtual bool     defined_in_init (string var_name) = 0;
  virtual tree     get_env_value (string var_name, path p) = 0;
  virtual tree     get_env_value (string var_name) = 0;
  virtual tree     get_init_value (string var_name) = 0;
  virtual string   get_env_string (string var_name) = 0;
  virtual string   get_init_string (string var_name) = 0;
  virtual int      get_env_int (string var_name) = 0;
  virtual int      get_init_int (string var_name) = 0;
  virtual double   get_env_double (string var_name) = 0;
  virtual double   get_init_double (string var_name) = 0;
  virtual language get_env_language () = 0;
  virtual int      get_page_count () = 0;
  virtual SI       get_page_width (bool deco) = 0;
  virtual SI       get_pages_width (bool deco) = 0;
  virtual SI       get_page_height (bool deco) = 0;
  virtual SI       get_total_width (bool deco) = 0;
  virtual SI       get_total_height (bool deco) = 0;
  virtual tree     exec_texmacs (tree t, path p) = 0;
  virtual tree     exec_texmacs (tree t) = 0;
  virtual tree     exec_verbatim (tree t, path p) = 0;
  virtual tree     exec_verbatim (tree t) = 0;
  virtual tree     exec_html (tree t, path p) = 0;
  virtual tree     exec_html (tree t) = 0;
  virtual tree     exec_latex (tree t, path p) = 0;
  virtual tree     exec_latex (tree t) = 0;
  virtual tree     texmacs_exec (tree t) = 0;
  virtual tree     var_texmacs_exec (tree t) = 0;
  virtual tree     checkout_animation (tree t) = 0;
  virtual tree     commit_animation (tree t) = 0;
  virtual tree     get_style () = 0;
  virtual void     set_style (tree t) = 0;
  virtual void     init_style () = 0;
  virtual void     init_style (string style) = 0;
  virtual void     change_style (tree style) = 0;
  virtual tree     get_init_all () = 0;
  virtual void     init_env (string var, tree by) = 0;
  virtual void     init_default (string var) = 0;
  virtual tree     get_ref (string key) = 0;
  virtual tree     get_aux (string key) = 0;
  virtual tree     get_att (string key) = 0;
  virtual void     set_ref (string key, tree im) = 0;
  virtual void     set_aux (string key, tree im) = 0;
  virtual void     set_att (string key, tree im) = 0;
  virtual void     reset_ref (string key) = 0;
  virtual void     reset_aux (string key) = 0;
  virtual void     reset_att (string key) = 0;
  virtual array<string> find_refs (string val, bool global= false) = 0;
  virtual array<string> list_refs (bool global= false) = 0;
  virtual array<string> list_auxs (bool global= false) = 0;
  virtual array<string> list_atts (bool global= false) = 0;
  virtual void     typeset_forced () = 0;
  virtual void     typeset_invalidate (path p) = 0;
  virtual void     typeset_invalidate_all () = 0;
  virtual void     typeset_invalidate_players (path p, bool reattach) = 0;

  /* public routines from edit_modify */
  virtual void notify_assign (path p, tree u) = 0;
  virtual void notify_insert (path p, tree u) = 0;
  virtual void notify_remove (path p, int nr) = 0;
  virtual void notify_split (path p) = 0;
  virtual void notify_join (path p) = 0;
  virtual void notify_assign_node (path p, tree_label op) = 0;
  virtual void notify_insert_node (path p, tree t) = 0;
  virtual void notify_remove_node (path p) = 0;
  virtual void notify_set_cursor (path p, tree data) = 0;
  virtual void post_notify (path p) = 0;
  virtual void clear_undo_history () = 0;
  virtual double this_author () = 0;
  virtual void archive_state () = 0;
  virtual void start_editing () = 0;
  virtual void end_editing () = 0;
  virtual void cancel_editing () = 0;
  virtual void start_slave (double a) = 0;
  virtual void mark_start (double a) = 0;
  virtual bool mark_cancel (double a) = 0;
  virtual void mark_end (double a) = 0;
  virtual void add_undo_mark () = 0;
  virtual void remove_undo_mark () = 0;
  virtual int  undo_possibilities () = 0;
  virtual void unredoable_undo () = 0;
  virtual void undo (int i=0) = 0;
  virtual int  redo_possibilities () = 0;
  virtual void redo (int i=0) = 0;
  virtual void require_save () = 0;
  virtual void notify_save (bool real_save= true) = 0;
  virtual bool need_save (bool real_save= true) = 0;
  virtual void show_history () = 0;
  virtual observer position_new (path p) = 0;
  virtual void position_delete (observer o) = 0;
  virtual void position_set (observer o, path p) = 0;
  virtual path position_get (observer o) = 0;

  /* public routines from edit_text */
  virtual void correct (path p) = 0;
  virtual void correct_concat (path p, int done=0) = 0;
  virtual bool insert_return () = 0;
  virtual void remove_return (path p) = 0;
  virtual void insert_tree (tree t, path p_in_t) = 0;
  virtual void var_insert_tree (tree t, path p_in_t) = 0;
  virtual void insert_tree (tree t) = 0;
  virtual void remove_text (bool forward) = 0;
  virtual void remove_structure (bool forward) = 0;
  virtual void remove_structure_upwards () = 0;

  virtual void make_space (tree t) = 0;
  virtual void make_space (string w) = 0;
  virtual void make_space (string w, string y1, string y2) = 0;
  virtual void make_hspace (string s) = 0;
  virtual void make_hspace (string smin, string sdef, string smax) = 0;
  virtual void make_vspace_before (string s) = 0;
  virtual void make_vspace_before (string smin, string sdef, string smax) = 0;
  virtual void make_vspace_after (string s) = 0;
  virtual void make_vspace_after (string smin, string sdef, string smax) = 0;
  virtual void make_htab (string spc) = 0;
  virtual void make_image (string file_name, bool link,
			   string w, string h, string x, string y) = 0;

  /* public routines from edit_math */
  virtual void make_rigid () = 0;
  virtual void make_lprime (string s) = 0;
  virtual void make_rprime (string s) = 0;
  virtual void make_below () = 0;
  virtual void make_above () = 0;
  virtual void make_script (bool sup, bool right) = 0;
  virtual void make_fraction () = 0;
  virtual void make_sqrt () = 0;
  virtual void make_var_sqrt () = 0;
  virtual void make_wide (string wide) = 0;
  virtual void make_wide_under (string wide) = 0;
  virtual void make_neg () = 0;
  virtual void make_tree () = 0;

  /* public routines from edit_table */
  virtual void   make_table (int nr_rows=1, int nr_cols=1) = 0;
  virtual void   make_subtable (int nr_rows=1, int nr_cols=1) = 0;
  virtual void   table_deactivate () = 0;
  virtual void   table_extract_format () = 0;
  virtual void   table_insert_row (bool forward) = 0;
  virtual void   table_insert_column (bool forward) = 0;
  virtual void   table_remove_row (bool forward, bool flag= false) = 0;
  virtual void   table_remove_column (bool forward, bool flag= false) = 0;
  virtual int    table_nr_rows () = 0;
  virtual int    table_nr_columns () = 0;
  virtual array<int> table_get_extents () = 0;
  virtual void   table_set_extents (int rows, int cols) = 0;
  virtual int    table_which_row () = 0;
  virtual int    table_which_column () = 0;
  virtual array<int> table_which_cells () = 0;
  virtual path   table_search_cell (int row, int col) = 0;
  virtual void   table_go_to (int row, int col) = 0;
  virtual void   table_set_format (string var, tree val) = 0;
  virtual tree   table_get_format () = 0;
  virtual string table_get_format (string var) = 0;
  virtual void   table_del_format (string var= "") = 0;
  virtual void   table_format_center () = 0;
  virtual void   table_row_decoration (bool forward) = 0;
  virtual void   table_column_decoration (bool forward) = 0;
  virtual void   table_correct_block_content () = 0;
  virtual void   table_resize_notify () = 0;
  virtual void   set_cell_mode (string mode) = 0;
  virtual string get_cell_mode () = 0;
  virtual void   cell_set_format (string var, tree val) = 0;
  virtual string cell_get_format (string var) = 0;
  virtual void   cell_del_format (string var= "") = 0;
  virtual void   table_test () = 0;

  /* public routines from edit_dynamic */
  virtual bool in_source () = 0;
  virtual path find_dynamic (path p) = 0;
  virtual void make_compound (tree_label l, int n=-1) = 0;
  virtual void activate () = 0;
  virtual void go_to_argument (path p, bool start_flag) = 0;
  virtual void insert_argument (path p, bool forward) = 0;
  virtual void insert_argument (bool forward) = 0;
  virtual void remove_empty_argument (path p, bool forward) = 0;
  virtual void remove_argument (path p, bool forward) = 0;
  virtual void remove_argument (bool forward) = 0;
  virtual void make_with (string var, string val) = 0;
  virtual void insert_with (path p, string var, tree val) = 0;
  virtual void remove_with (path p, string var) = 0;
  virtual void make_mod_active (tree_label l) = 0;
  virtual void make_style_with (string var, string val) = 0;
  virtual void make_hybrid () = 0;
  virtual bool activate_latex () = 0;
  virtual void activate_hybrid (bool with_args_hint) = 0;
  virtual void activate_symbol () = 0;
  virtual bool make_return_before () = 0;
  virtual bool make_return_after () = 0;
  virtual void temp_proof_fix () = 0;

  /* public routines from edit_process */
  virtual void generate_bibliography (string bib, string sty, string fname)= 0;
  virtual void generate_table_of_contents (string toc) = 0;
  virtual void generate_index (string idx) = 0;
  virtual void generate_glossary (string glo) = 0;
  virtual void generate_aux (string which= "") = 0;
  virtual bool get_save_aux () = 0;

  /* public routines from edit_select */
  virtual path semantic_root (path p) = 0;
  virtual bool semantic_active (path p) = 0;
  virtual bool semantic_select (path p, path& q1, path& q2, int mode) = 0;

  virtual void select (path p) = 0;
  virtual void select (path start, path end) = 0;
  virtual void select_all () = 0;
  virtual void select_line () = 0;
  virtual void select_from_cursor () = 0;
  virtual void select_from_cursor_if_active () = 0;
  virtual void select_from_keyboard (bool flag) = 0;
  virtual void select_from_shift_keyboard () = 0;
  virtual void select_enlarge_text () = 0;
  virtual void select_enlarge () = 0;
  virtual void select_enlarge_environmental () = 0;

  virtual bool selection_active_any () = 0;
  virtual bool selection_active_normal () = 0;
  virtual bool selection_active_table (bool strict= true) = 0;
  virtual bool selection_active_small () = 0;
  virtual bool selection_active_enlarging () = 0;

  virtual void selection_raw_set (string key, tree t) = 0;
  virtual tree selection_raw_get (string key) = 0;
  virtual void selection_correct (path i1, path i2, path& o1, path& o2) = 0;
  virtual path selection_get_subtable (int& i1, int& j1, int& i2, int& j2) = 0;
  virtual selection compute_selection (path p1, path p2) = 0;
  virtual selection compute_selection (range_set sel) = 0;
  virtual void selection_get (selection& sel) = 0;
  virtual void selection_get (path& start, path& end) = 0;
  virtual path selection_get_start () = 0;
  virtual path selection_get_end () = 0;
  virtual path selection_var_get_start () = 0;
  virtual path selection_var_get_end () = 0;
  virtual path selection_get_path () = 0;
  virtual path selection_get_cursor_path () = 0;
  virtual tree selection_get_env_value (string var) = 0;
  virtual tree selection_get (string key) = 0;
  virtual void selection_set (string key, tree t, bool persistant= false) = 0;
  virtual void selection_set (tree t) = 0;
  virtual void selection_set_start (path p= path()) = 0;
  virtual void selection_set_end (path p= path()) = 0;
  virtual void selection_set_paths (path start, path end) = 0;
  virtual void selection_set_range_set (range_set sel) = 0;
  virtual void selection_copy (string key= "primary") = 0;
  virtual void selection_paste (string key= "primary") = 0;
  virtual void selection_clear (string key= "primary") = 0;
  virtual void selection_cancel () = 0;
  virtual void selection_set_import (string fm) = 0;
  virtual void selection_set_export (string fm) = 0;
  virtual string selection_get_import () = 0;
  virtual string selection_get_export () = 0;

  virtual tree selection_get () = 0;
  virtual void selection_cut (string key= "primary") = 0;
  virtual tree selection_get_cut () = 0;
  virtual void selection_move () = 0;
  virtual void cut (path p) = 0;
  virtual void cut (path start, path end) = 0;
  virtual path manual_focus_get () = 0;
  virtual void manual_focus_set (path p, bool force= true) = 0;
  virtual void manual_focus_release () = 0;
  virtual path focus_get (bool skip_flag= true) = 0;

  virtual void set_alt_selection (string s, range_set sel) = 0;
  virtual range_set get_alt_selection (string s) = 0;
  virtual void cancel_alt_selection (string s) = 0;
  virtual void cancel_alt_selections () = 0;

  /* public routines from edit_replace */
  virtual bool inside (string what) = 0;
  virtual bool inside (tree_label l) = 0;
  virtual bool inside_with (string var, string val) = 0;
  virtual string inside_which (tree t) = 0;
  virtual path search_upwards (string what) = 0;
  virtual path search_upwards (tree_label l) = 0;
  virtual path search_parent_upwards (tree_label l) = 0;
  virtual path search_parent_upwards (tree_label l, int& last) = 0;
  virtual path search_upwards_with (string var, string val) = 0;
  virtual path search_upwards_in_set (tree t) = 0;
  virtual path search_previous_compound (path init, string which) = 0;
  virtual path search_next_compound (path init, string which) = 0;
  virtual void search_start (bool forward= true) = 0;
  virtual void search_button_next () = 0;
  virtual bool search_keypress (string s) = 0;
  virtual void replace_start (tree what, tree by, bool forward= true) = 0;
  virtual bool replace_keypress (string s) = 0;
  virtual void spell_start () = 0;
  virtual void spell_replace (string by) = 0;
  virtual bool spell_keypress (string s) = 0;

  /* public routines from edit_main */
  virtual void set_property (scheme_tree what, scheme_tree val) = 0;
  virtual scheme_tree get_property (scheme_tree what) = 0;
  virtual void clear_buffer () = 0;
  virtual void new_window () = 0;
  virtual void clone_window () = 0;
  virtual void tex_buffer () = 0;
  virtual url  get_name () = 0;
  virtual void focus_on_this_editor () = 0;
  virtual void notify_page_change () = 0;
  virtual string get_metadata (string kind) = 0;
  virtual int  nr_pages () = 0;
  virtual void print_doc (url ps_name, bool to_file, int first, int last) = 0;
  virtual void print_to_file (url ps_name,
			      string first="1", string last="1000000") = 0;
  virtual void print_buffer (string first="1", string last="1000000") = 0;
  virtual void export_ps (url ps_name,
			  string first="1", string last="1000000") = 0;
  virtual array<int> print_snippet (url u, tree t, bool conserve_preamble) = 0;
  virtual bool graphics_file_to_clipboard (url output) = 0;
  virtual void footer_eval (string s) = 0;
  virtual tree the_line () = 0;
  virtual tree the_root () = 0;
  virtual tree the_buffer () = 0;
  virtual tree the_subtree (path p) = 0;
  virtual path the_path () = 0;
  virtual path the_shifted_path () = 0;
  virtual path the_buffer_path () = 0;
  virtual void show_tree () = 0;
  virtual void show_env () = 0;
  virtual void show_path () = 0;
  virtual void show_cursor () = 0;
  virtual void show_selection () = 0;
  virtual void show_meminfo () = 0;
  virtual void edit_special () = 0;
  virtual void edit_test () = 0;

  friend class tm_window_rep;
  friend class tm_server_rep;
  friend class server_command_rep;
  friend void   edit_announce (editor_rep* ed, modification mod);
  friend void   edit_done (editor_rep* ed, modification mod);
  friend string get_editor_status_report ();
  friend void   tm_failure (const char* msg);
  friend void   set_buffer_tree (url name, tree doc);
  friend void   set_current_view (url u);
  friend void   set_current_drd (url name);
  friend void   focus_on_editor (editor ed);
  friend void   delete_view (url u);
};

template<> void tm_delete<editor_rep> (editor_rep* ptr);

class editor {
EXTEND_NULL(widget,editor);
public:
  inline bool operator == (editor w) { return rep == w.rep; }
  inline bool operator != (editor w) { return rep != w.rep; }
};
EXTEND_NULL_CODE(widget,editor);

editor new_editor (server_rep* sv, tm_buffer buf);

#define SERVER(cmd) {                 \
  url temp= get_current_view_safe (); \
  focus_on_this_editor ();            \
  sv->cmd;                            \
  set_current_view (temp);            \
}

// global variables for showing keypresses
extern bool          kbd_show_keys;
extern array<string> kbd_shown_keys;
extern array<string> kbd_last_keys;
extern array<time_t> kbd_last_times;
extern int           kbd_erase_delay;
extern int           kbd_hide_delay;

bool get_show_kbd ();
void set_show_kbd (bool flag);

#endif // defined EDITOR_H
