
/******************************************************************************
* MODULE     : editor.cpp
* DESCRIPTION: routines for the editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_main.hpp"
#include "tm_buffer.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "printer.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "typesetter.hpp"
#include "drd_std.hpp"
#include "message.hpp"
#include <setjmp.h>
#ifdef EXPERIMENTAL
#include "../../Style/Memorizer/clean_copy.hpp"
#endif

/******************************************************************************
* Constructors and destructor
******************************************************************************/

editor_rep::editor_rep ():
  simple_widget_rep (),
  drd (buf->abbr, std_drd), et (the_et), rp (buf->rp) {}

editor_rep::editor_rep (server_rep* sv2, tm_buffer buf2):
  simple_widget_rep (),
  sv (sv2), buf (buf2), drd (buf->abbr, std_drd),
  et (the_et), rp (buf2->rp) {}

edit_main_rep::edit_main_rep (server_rep* sv, tm_buffer buf):
  editor_rep (sv, buf), props (UNKNOWN), ed_obs (edit_observer (this))
{
#ifdef EXPERIMENTAL
  cct= copy (subtree (et, rp));
  copy_ip (subtree (et, rp), cct);
#endif
  attach_observer (subtree (et, rp), ed_obs);
  notify_change (THE_TREE);
  tp= correct_cursor (et, rp * 0);
}

edit_main_rep::~edit_main_rep () {
  detach_observer (subtree (et, rp), ed_obs);
#ifdef EXPERIMENTAL
  mem= memorizer ();
#endif
}

editor
new_editor (server_rep* sv, tm_buffer buf) {
  return tm_new<edit_main_rep> (sv, buf);
}

/******************************************************************************
* Properties
******************************************************************************/

void
edit_main_rep::set_property (scheme_tree what, scheme_tree val) {
  props (what)= val;
}

void
edit_main_rep::set_bool_property (string what, bool val) {
  props (what)= (val? string ("true"): string ("false"));
}

void
edit_main_rep::set_int_property (string what, int val) {
  props (what)= as_tree (val);
}

void
edit_main_rep::set_string_property (string what, string val) {
  props (what)= val;
}

scheme_tree
edit_main_rep::get_property (scheme_tree what) {
  return props [what];
}

bool
edit_main_rep::get_bool_property (string what) {
  return as_bool (props [what]);
}

int
edit_main_rep::get_int_property (string what) {
  return as_int (props [what]);
}

string
edit_main_rep::get_string_property (string what) {
  return as_string (props [what]);
}

/******************************************************************************
* Global routines
******************************************************************************/

void
edit_main_rep::clear_buffer () {
  assign (rp, tree (DOCUMENT, tree ("")));
}

void
edit_main_rep::new_window () {
}

void
edit_main_rep::clone_window () {
}

void
edit_main_rep::tex_buffer () {
}

url
edit_main_rep::get_name () {
  return buf->name;
}

void
edit_main_rep::focus_on_this_editor () {
  sv->focus_on_editor (this);
}

void
edit_main_rep::notify_page_change () {
  if (is_attached (this)) send_invalidate_all (this);
}

/******************************************************************************
* Printing
******************************************************************************/

string printing_dpi ("600");
string printing_cmd ("lpr");
string printing_on ("a4");

void
edit_main_rep::print (url name, bool conform, int first, int last) {
  bool pdf= (suffix (name) == "pdf");
  url orig= resolve (name, "");
  if (pdf) name= url_temp (".ps");

  string medium = env->get_string (PAGE_MEDIUM);
  if (conform && (medium != "paper")) conform= false;
  // FIXME: better command for conform printing

  // Set environment variables for printing

  typeset_prepare ();
  env->write (DPI, printing_dpi);
  env->write (PAGE_SHOW_HF, "true");
  env->write (PAGE_SCREEN_MARGIN, "false");
  if (!conform) {
    env->write (PAGE_MEDIUM, "paper");
    env->write (PAGE_PRINTED, "true");
  }

  // Typeset pages for printing

  box the_box= typeset_as_document (env, subtree (et, rp), reverse (rp));

  // Determine parameters for printer

  string page_type = env->get_string (PAGE_TYPE);
  double w         = env->page_width;
  double h         = env->page_height;
  double cm        = env->as_length (string ("1cm"));
  bool   landsc    = env->page_landscape;
  int    dpi       = as_int (printing_dpi);
  int    start     = max (0, first-1);
  int    end       = min (N(the_box[0]), last);
  int    pages     = end-start;
  if (conform) {
    page_type= "user";
    SI bw= the_box[0][0]->w();
    SI bh= the_box[0][0]->h();
    string bws= as_string (bw) * "tmpt";
    string bhs= as_string (bh) * "tmpt";
    w= env->as_length (bws);
    h= env->as_length (bhs);
  }

  // Print pages

  int i;
  renderer ren=
    printer (name, dpi, pages, page_type, landsc, w/cm, h/cm);
  for (i=start; i<end; i++) {
    tree bg= env->read (BG_COLOR);
    ren->set_background_pattern (bg);
    if (bg != "white")
      ren->clear_pattern (0, (SI) -h, (SI) w, 0);

    rectangles rs;
    the_box[0]->sx(i)= 0;
    the_box[0]->sy(i)= 0;
    the_box[0][i]->redraw (ren, path (0), rs);
    if (i<end-1) ren->next_page ();
  }
  tm_delete (ren);

  if (pdf) {
    ps2pdf (name, orig);
    ::remove (name);
  }
}

void
edit_main_rep::print_to_file (url name, string first, string last) {
  print (name, false, as_int (first), as_int (last));
}

void
edit_main_rep::print_buffer (string first, string last) {
  url temp= url_temp (".ps");
  print (temp, false, as_int (first), as_int (last));
  system (printing_cmd, temp);
  ::remove (temp);
}

void
edit_main_rep::export_ps (url name, string first, string last) {
  print (name, true, as_int (first), as_int (last));
}

array<int>
edit_main_rep::print_snippet (url name, tree t) {
  bool ps= suffix (name) == "ps" || suffix (name) == "eps";
  typeset_prepare ();
  int dpi= as_int (printing_dpi);
  //if (!ps) t= tree (WITH, MAGNIFICATION, "2", PAGE_WIDTH, "40cm", t);
  if (!ps) t= tree (WITH, MAGNIFICATION, "1.6", PAGE_WIDTH, "40cm", t);
  box b= typeset_as_box (env, t, path ());
  if (b->x4 - b->x3 >= 5*PIXEL && b->y4 - b->y3 >= 5*PIXEL) {
    if (ps) make_eps (name, b, dpi);
    else {
      url temp= url_temp ("eps");
      make_eps (temp, b, dpi);
      system ("convert", temp, name);
      ::remove (temp);
    }
  }
  array<int> a;
  a << b->x3 << b->y3 << b->x4 << b->y4;
  return a;
}

/******************************************************************************
* Evaluation of expressions
******************************************************************************/

void
edit_main_rep::footer_eval (string s) {
  // s= unslash (s); // FIXME: dirty fix; should not be necessary
  string r= object_to_string (eval (s));
  set_message (r, "evaluate expression");
}

tree
edit_main_rep::the_line () {
  path p= search_parent_upwards (DOCUMENT);
  return copy (subtree (et, p));
}

tree
edit_main_rep::the_root () {
  return et;
}

tree
edit_main_rep::the_buffer () {
  return subtree (et, rp);
}

tree
edit_main_rep::the_subtree (path p) {
  return subtree (et, p);
}

path
edit_main_rep::the_buffer_path () {
  return copy (rp);
}

path
edit_main_rep::the_path () {
  return copy (tp);
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
edit_main_rep::show_tree () {
  stretched_print (et, true);
  // cout << et << "\n";
}

void
edit_main_rep::show_env () {
  cout << env << "\n";
}

void
edit_main_rep::show_path () {
  cout << tp << "\n";
}

void
edit_main_rep::show_cursor () {
  cout << "Principal cursor: "
       << cu->ox << ", " << cu->oy << " [" << cu->delta << "]\n";
  cout << "Ghost cursor    : "
       << mv->ox << ", " << mv->oy << " [" << mv->delta << "]\n";
}

void
edit_main_rep::show_selection () {
  selection sel; selection_get (sel);
  cout << "physical  selection: " << start_p << " --- " << end_p << "\n";
  cout << "logical   selection: " << sel->start << " --- " << sel->end << "\n";
}

void
edit_main_rep::show_meminfo () {
  mem_info ();
}

void
edit_main_rep::edit_special () {
}

void
edit_main_rep::edit_test () {
  cout << "Test !\n";
}
