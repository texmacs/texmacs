
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
#include "PsDevice/page_type.hpp"
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

/******************************************************************************
* Printing
******************************************************************************/

void
edit_main_rep::set_page_parameters () {
  if (attached ()) this << emit_invalidate_all ();

  string medium     = get_init_string (PAGE_MEDIUM);
  string type       = get_init_string (PAGE_TYPE);
  string orientation= get_init_string (PAGE_ORIENTATION);
  bool   landscape  = (orientation == "landscape");

  if (medium == "automatic") {
    init_env (PAGE_ODD, "5mm");
    init_env (PAGE_EVEN, "5mm");
    init_env (PAGE_RIGHT, "5mm");
    init_env (PAGE_TOP, "5mm");
    init_env (PAGE_BOT, "5mm");
    notify_change (THE_AUTOMATIC_SIZE);
    return;
  }
  if (type == "user") return;

#define PAGE_INIT(feature) \
  init_env (feature, page_get_feature (type, feature, landscape))
  PAGE_INIT (PAR_WIDTH);
  PAGE_INIT (PAGE_ODD);
  PAGE_INIT (PAGE_EVEN);
  PAGE_INIT (PAGE_RIGHT);
  PAGE_INIT (PAGE_TOP);
  PAGE_INIT (PAGE_BOT);
  PAGE_INIT (PAGE_REDUCE_LEFT);
  PAGE_INIT (PAGE_REDUCE_RIGHT);
  PAGE_INIT (PAGE_REDUCE_TOP);
  PAGE_INIT (PAGE_REDUCE_BOT);
#undef PAGE_INIT
}

void
edit_main_rep::set_page_medium (string medium) {
  init_env (PAGE_MEDIUM, medium);
  set_page_parameters ();
}

void
edit_main_rep::set_page_type (string type) {
  init_env (PAGE_TYPE, type);
  set_page_parameters ();
}

void
edit_main_rep::set_page_orientation (string orientation) {
  init_env (PAGE_ORIENTATION, orientation);
  set_page_parameters ();
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
  env->write (PAGE_REDUCE_LEFT, "0cm");
  env->write (PAGE_REDUCE_RIGHT, "0cm");
  env->write (PAGE_REDUCE_TOP, "0cm");
  env->write (PAGE_REDUCE_BOT, "0cm");
  env->write (PAGE_SHOW_HF, "true");
  if (!conform) env->write (PAGE_MEDIUM, "paper");

  // Typeset pages for printing

  box the_box= typeset_as_document (env, subtree (et, rp), reverse (rp));

  // Determine parameters for printer device

  string page_type = printing_on;
  double w         = env->get_length (PAGE_WIDTH);
  double h         = env->get_length (PAGE_HEIGHT);
  double cm        = env->decode_length (string ("1cm"));
  bool   landsc    = (env->get_string (PAGE_ORIENTATION) == "landscape");
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

path
edit_main_rep::the_path () {
  return copy (tp);
}

void
edit_main_rep::process_input () {
  path p= search_upwards ("input");
  if (nil (p) || (N (subtree (et, p)) != 2)) return;
  tree t= subtree (et, p) [1];
  string lan= get_env_string (PROG_LANGUAGE);

  if (lan == "scheme") {
    start_output ();
    tree u= sv->evaluate ("scheme", "default", t);
    if (!is_document (u)) u= tree (DOCUMENT, u);
    insert_tree (u);
    start_input ();
  }
  else if (connection_declared (lan)) {
    start_output ();
    feed_input (t);
  }
  else {
    set_message ("Package#'" * lan * "'#not declared",
		 "Evaluate#'" * lan * "'#expression");
  }
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
edit_main_rep::show_tree () {
  cout << et << "\n";
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
