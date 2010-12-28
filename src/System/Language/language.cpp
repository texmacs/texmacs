
/******************************************************************************
* MODULE     : language.cpp
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "impl_language.hpp"
#include "packrat.hpp"

RESOURCE_CODE(language);

text_property_rep global_tpr;

text_property_rep tp_normal_rep (TP_NORMAL);
text_property_rep tp_hyph_rep (TP_HYPH, SPC_NONE, SPC_NONE, 0, 0);
text_property_rep tp_space_rep (TP_SPACE, SPC_NONE, SPC_SPACE, 0,0);
text_property_rep tp_dspace_rep (TP_DSPACE, SPC_NONE, SPC_DSPACE, 0,0);
text_property_rep tp_period_rep (TP_PERIOD, SPC_NONE, SPC_PERIOD, 0, 0);
text_property_rep tp_blank_rep (
  TP_BLANK, SPC_NONE, SPC_SPACE, 0, HYPH_INVALID);
text_property_rep tp_operator_rep (
  TP_OPERATOR, SPC_NONE, SPC_OPERATOR, 0, HYPH_INVALID);
text_property_rep tp_shortop_rep (
  TP_SHORTOP, SPC_NONE, SPC_TINY, 0, HYPH_INVALID);

/******************************************************************************
* Text properties
******************************************************************************/

text_property_rep::text_property_rep (
  int type2, int spc_before2, int spc_after2,
  int pen_before2, int pen_after2,
  int op_type2, int priority2, int limits2):
    type (type2),
    spc_before (spc_before2), spc_after (spc_after2),
    pen_before (pen_before2), pen_after (pen_after2),
    op_type (op_type2), priority (priority2), limits (limits2) {}

bool
operator == (text_property_rep tpr1, text_property_rep tpr2) {
  return
    (tpr1.type == tpr2.type) &&
    (tpr1.spc_before == tpr2.spc_before) &&
    (tpr1.spc_after == tpr2.spc_after) &&
    (tpr1.pen_before == tpr2.pen_before) &&
    (tpr1.pen_after == tpr2.pen_after) &&
    (tpr1.op_type == tpr2.op_type) &&
    (tpr1.priority == tpr2.priority) &&
    (tpr1.limits == tpr2.limits);
}

bool
operator != (text_property_rep tpr1, text_property_rep tpr2) {
  return !(tpr1 == tpr2);
}

/******************************************************************************
* Initialized the allowed successions of mathematical operators
******************************************************************************/

int succession_status_table [OP_TOTAL * OP_TOTAL];

/*
int
succession_status (int op1, int op2) {
  cout << "Check " << op1 << ":" << op2 << " -> " <<
    succession_status_table [op1 * OP_TOTAL + op2] << "\n";
  return succession_status_table [op1 * OP_TOTAL + op2];
}
*/

static inline void set_status (int op1, int op2, int st) {
  succession_status_table [op1 * OP_TOTAL + op2]= st; }

static inline void init_could_end (int op) {
  set_status (op, OP_UNARY, REMOVE_CURRENT_SPACE);
  set_status (op, OP_N_ARY, REMOVE_CURRENT_SPACE);
  set_status (op, OP_PREFIX, REMOVE_CURRENT_SPACE);
}

static inline void init_expect_after (int op) {
  set_status (op, OP_TEXT, REMOVE_CURRENT_SPACE);
  set_status (op, OP_BINARY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_POSTFIX, REMOVE_CURRENT_SPACE);
  set_status (op, OP_INFIX, REMOVE_CURRENT_SPACE);
  set_status (op, OP_APPLY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_SEPARATOR, REMOVE_SPACE_BEFORE);
  set_status (op, OP_MIDDLE_BRACKET, REMOVE_SPACE_BEFORE);
  set_status (op, OP_CLOSING_BRACKET, REMOVE_SPACE_BEFORE);
}

static inline void init_expect_space (int op) {
  set_status (op, OP_TEXT, REMOVE_CURRENT_SPACE);
  set_status (op, OP_SYMBOL, REMOVE_SPACE_BEFORE);
  set_status (op, OP_UNARY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_BINARY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_N_ARY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_PREFIX, REMOVE_SPACE_BEFORE);
  set_status (op, OP_POSTFIX, REMOVE_SPACE_BEFORE);
  set_status (op, OP_INFIX, REMOVE_SPACE_BEFORE);
  set_status (op, OP_SEPARATOR, REMOVE_SPACE_BEFORE);
  set_status (op, OP_MIDDLE_BRACKET, REMOVE_SPACE_BEFORE);
  set_status (op, OP_CLOSING_BRACKET, REMOVE_SPACE_BEFORE);
  set_status (op, OP_BIG, REMOVE_SPACE_BEFORE);
}

void
init_succession_status_table () {
  for (int i=0; i < (OP_TOTAL * OP_TOTAL); i++)
    succession_status_table[i]= SUCCESSION_OK;

  for (int i=0; i<OP_TOTAL; i++) {
    set_status (OP_UNKNOWN, i, REMOVE_ALL_SPACE);
    set_status (i, OP_UNKNOWN, REMOVE_ALL_SPACE);
  }

  init_expect_after (OP_TEXT);
  init_could_end    (OP_SYMBOL);
  init_expect_space (OP_UNARY);
  init_expect_space (OP_BINARY);
  init_expect_space (OP_N_ARY);
  init_expect_after (OP_PREFIX);
  init_could_end    (OP_POSTFIX);
  init_expect_after (OP_INFIX);
  init_expect_after (OP_APPLY);
  init_expect_after (OP_SEPARATOR);
  init_expect_after (OP_OPENING_BRACKET);
  init_expect_after (OP_MIDDLE_BRACKET);
  init_could_end    (OP_CLOSING_BRACKET);
  init_expect_after (OP_BIG);
  set_status (OP_APPLY, OP_BINARY, SUCCESSION_OK);
  set_status (OP_OPENING_BRACKET, OP_CLOSING_BRACKET, SUCCESSION_OK);
}

/******************************************************************************
* Default group of a string
******************************************************************************/

language_rep::language_rep (string s):
  rep<language> (s), hl_lan (0) {}

string
language_rep::get_group (string s) {
  (void) s;
  return "default";
}

array<string>
language_rep::get_members (string s) {
  (void) s;
  return array<string> ();
}

void
language_rep::highlight (tree t) {
  if (hl_lan != 0 && !has_highlight (t, hl_lan))
    packrat_highlight (res_name, "Main", t);
}

string
language_rep::get_color (tree t, int start, int end) {
  (void) t; (void) start; (void) end;
  return "";
}
