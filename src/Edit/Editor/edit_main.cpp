
/******************************************************************************
* MODULE     : editor.cpp
* DESCRIPTION: routines for the editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_main.hpp"
#include "tm_widget.hpp"
#include "tm_buffer.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "PsDevice/printer.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "typesetter.hpp"
#include "drd_std.hpp"
#include <dlfcn.h>
#include <setjmp.h>

/******************************************************************************
* Constructors and destructor
******************************************************************************/

editor_rep::editor_rep ():
  attribute_widget_rep (dis),
  drd (buf->abbr, std_drd), et (the_et), rp (buf->rp) {}

editor_rep::editor_rep (server_rep* sv2, display dis, tm_buffer buf2):
  attribute_widget_rep (dis),
  sv (sv2), buf (buf2), drd (buf->abbr, std_drd),
  et (the_et), rp (buf2->rp) {}

edit_main_rep::edit_main_rep (server_rep* sv, display dis, tm_buffer buf):
  editor_rep (sv, dis, buf), props (UNKNOWN)
{
  notify_change (THE_TREE);
  tp= correct_cursor (et, rp * 0);
}

edit_main_rep::~edit_main_rep () {}

editor
new_editor (server_rep* sv, tm_buffer buf) {
  return new edit_main_rep (sv, current_display (), buf);
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
  if (attached ()) this << emit_invalidate_all ();
  if (get_init_string (PAGE_MEDIUM) == "automatic")
    notify_change (THE_AUTOMATIC_SIZE);
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
  url orig= name;
  if (pdf) name= url_temp (".ps");

  string medium = env->get_string (PAGE_MEDIUM);
  if (conform && (medium == "papyrus")) conform= false;
  if ((!conform) && (medium == "automatic")) {
    set_message (
      "Error: you should switch to ``paper'' or ``papyrus'' page type",
      "print");
    return;
  }

  // Set environment variables for printing

  typeset_prepare ();
  env->write (DPI, printing_dpi);
  env->write (PAGE_SHOW_HF, "true");
  env->write (PAGE_SCREEN_MARGIN, "false");
  if (!conform) env->write (PAGE_MEDIUM, "paper");

  // Typeset pages for printing

  box the_box= typeset_as_document (env, subtree (et, rp), reverse (rp));

  // Determine parameters for printer device

  string page_type = env->get_string (PAGE_TYPE);
  double w         = env->page_width;
  double h         = env->page_height;
  double cm        = env->decode_length (string ("1cm"));
  bool   landsc    = env->page_landscape;
  int    dpi       = as_int (printing_dpi);
  int    start     = max (0, first-1);
  int    end       = min (N(the_box[0]), last);
  int    pages     = end-start;
  if (conform) {
    page_type= "user";
    SI bw= the_box[0][0]->w();
    SI bh= the_box[0][0]->h();
    string bws= as_string (bw) * "unit";
    string bhs= as_string (bh) * "unit";
    w= env->decode_length (bws);
    h= env->decode_length (bhs);
  }

  // Print pages

  int i;
  ps_device dev=
    printer (dis, name, dpi, pages, page_type, landsc, w/cm, h/cm);
  for (i=start; i<end; i++) {
    string col_name= env->get_string (BG_COLOR);
    dev->set_background (dis->get_color (col_name));
    if (col_name != "white")
      dev->clear (0, -h, w, 0);

    rectangles rs;
    the_box[0]->sx(i)= 0;
    the_box[0]->sy(i)= 0;
    the_box[0][i]->redraw (dev, path (0), rs);
    if (i<end-1) dev->next_page ();
  }
  delete dev;

  if (pdf) {
    system ("ps2pdf", name, orig);
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

/******************************************************************************
* Evaluation of expressions
******************************************************************************/

void
edit_main_rep::footer_eval (string s) {
  s= unslash (s); // FIXME: dirty fix; should not be necessary
  string r= object_to_string (eval (s));
  set_message (r, "evaluate expression");
}

tree
edit_main_rep::the_line () {
  path p= search_parent_upwards (DOCUMENT);
  return copy (subtree (et, p));
}

tree
edit_main_rep::the_buffer () {
  return copy (et);
}

tree
edit_main_rep::the_subtree (path p) {
  return subtree (et, p);
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
