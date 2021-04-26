
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
#include "analyze.hpp"
#include "hyphenate.hpp"
#include "iterator.hpp"
#include "universal.hpp"

RESOURCE_CODE(language);

text_property_rep global_tpr;

text_property_rep tp_normal_rep
  (TP_NORMAL);
text_property_rep tp_hyph_rep
  (TP_HYPH, SPC_NONE, SPC_NONE, 0, 0);
text_property_rep tp_thin_space_rep
  (TP_THIN_SPACE, SPC_NONE, SPC_THIN_SPACE, 0, 0);
text_property_rep tp_space_rep
  (TP_SPACE, SPC_NONE, SPC_SPACE, 0, 0);
text_property_rep tp_dspace_rep
  (TP_DSPACE, SPC_NONE, SPC_DSPACE, 0, 0);
text_property_rep tp_nb_thin_space_rep
  (TP_NB_THIN_SPACE, SPC_NONE, SPC_THIN_SPACE, 0, HYPH_INVALID);
text_property_rep tp_nb_space_rep
  (TP_NB_SPACE, SPC_NONE, SPC_SPACE, 0, HYPH_INVALID);
text_property_rep tp_nb_dspace_rep
  (TP_NB_DSPACE, SPC_NONE, SPC_DSPACE, 0, HYPH_INVALID);
text_property_rep tp_period_rep
  (TP_PERIOD, SPC_NONE, SPC_PERIOD, 0, 0);
text_property_rep tp_cjk_normal_rep
  (TP_CJK_NORMAL, SPC_NONE, SPC_CJK_NORMAL, 0, 0);
text_property_rep tp_cjk_no_break_rep
  (TP_CJK_NO_BREAK, SPC_NONE, SPC_CJK_NORMAL, 0, HYPH_INVALID);
text_property_rep tp_cjk_period_rep
  (TP_CJK_PERIOD, SPC_NONE, SPC_CJK_PERIOD, HYPH_INVALID, 0);
text_property_rep tp_cjk_wide_period_rep
  (TP_CJK_PERIOD, SPC_NONE, SPC_CJK_WIDE_PERIOD, HYPH_INVALID, 0);
text_property_rep tp_cjk_no_break_period_rep
  (TP_CJK_PERIOD, SPC_NONE, SPC_CJK_PERIOD, HYPH_INVALID, HYPH_INVALID);
text_property_rep tp_half_rep
  (TP_OPERATOR, SPC_NONE, SPC_HALF, 0, HYPH_INVALID);
text_property_rep tp_operator_rep
  (TP_OPERATOR, SPC_NONE, SPC_OPERATOR, 0, HYPH_INVALID);
text_property_rep tp_short_apply_rep
  (TP_OPERATOR, SPC_NONE, SPC_SHORT_APPLY, 0, HYPH_INVALID);
text_property_rep tp_apply_rep
  (TP_OPERATOR, SPC_NONE, SPC_APPLY, 0, HYPH_INVALID);

/******************************************************************************
* Text properties
******************************************************************************/

text_property_rep::text_property_rep (
  int type2, int spc_before2, int spc_after2,
  int pen_before2, int pen_after2,
  int op_type2, int priority2, int limits2, tree_label macro2):
    type (type2),
    spc_before (spc_before2), spc_after (spc_after2),
    pen_before (pen_before2), pen_after (pen_after2),
    op_type (op_type2), priority (priority2), limits (limits2),
    macro (macro2) {}

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
    (tpr1.limits == tpr2.limits) &&
    (tpr1.macro == tpr2.macro);
}

bool
operator != (text_property_rep tpr1, text_property_rep tpr2) {
  return !(tpr1 == tpr2);
}

text_property_rep
copy (text_property_rep tpr) {
  return text_property_rep (tpr.type,
                            tpr.spc_before, tpr.spc_after,
                            tpr.pen_before, tpr.pen_after,
                            tpr.op_type, tpr.priority, tpr.limits,
                            tpr.macro);
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
  set_status (op, OP_BIG, REMOVE_CURRENT_SPACE);
}

static inline void init_expect_after (int op) {
  set_status (op, OP_TEXT, REMOVE_CURRENT_SPACE);
  set_status (op, OP_BINARY, REMOVE_SPACE_BEFORE);
  set_status (op, OP_POSTFIX, REMOVE_CURRENT_SPACE);
  set_status (op, OP_INFIX, REMOVE_CURRENT_SPACE);
  set_status (op, OP_PREFIX_INFIX, REMOVE_CURRENT_SPACE);
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
  set_status (op, OP_PREFIX_INFIX, REMOVE_SPACE_BEFORE);
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
  init_expect_after (OP_PREFIX_INFIX);
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
  rep<language> (s), lan_name (s), hl_lan (0) {}

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

/******************************************************************************
 * Encode and decode colors for syntax highlighting
 ******************************************************************************/

hashmap<string,int>
language_rep::color_encoding(type_helper<int>::init, 32);

void
initialize_color_encodings () {
  language_rep::color_encoding ("comment")= 1;
  language_rep::color_encoding ("error")= 3;
  language_rep::color_encoding ("preprocessor")= 4;
  language_rep::color_encoding ("preprocessor_directive")= 5;
  language_rep::color_encoding ("constant")= 10;
  language_rep::color_encoding ("constant_identifier")= 11;
  language_rep::color_encoding ("constant_function")= 12;
  language_rep::color_encoding ("constant_type")= 13;
  language_rep::color_encoding ("constant_category")= 14;
  language_rep::color_encoding ("constant_module")= 15;
  language_rep::color_encoding ("constant_number")= 16;
  language_rep::color_encoding ("constant_string")= 17;
  language_rep::color_encoding ("constant_char")= 18;
  language_rep::color_encoding ("variable")= 20;
  language_rep::color_encoding ("variable_identifier")= 21;
  language_rep::color_encoding ("variable_function")= 22;
  language_rep::color_encoding ("variable_type")= 23;
  language_rep::color_encoding ("variable_category")= 24;
  language_rep::color_encoding ("variable_module")= 25;
  language_rep::color_encoding ("variable_ioarg")= 26;
  language_rep::color_encoding ("declare")= 30;
  language_rep::color_encoding ("declare_identifier")= 31;
  language_rep::color_encoding ("declare_function")= 32;
  language_rep::color_encoding ("declare_type")= 33;
  language_rep::color_encoding ("declare_category")= 34;
  language_rep::color_encoding ("declare_module")= 35;
  language_rep::color_encoding ("operator")= 40;
  language_rep::color_encoding ("operator_openclose")= 41;
  language_rep::color_encoding ("operator_field")= 42;
  language_rep::color_encoding ("operator_special")= 43;
  language_rep::color_encoding ("keyword")= 50;
  language_rep::color_encoding ("keyword_conditional")= 51;
  language_rep::color_encoding ("keyword_control")= 52;
}

void
initialize_color_decodings (string lan_name) {
  //debug_packrat << "Initialize color decodings for " << lan_name << LF;

  language lan= prog_language (lan_name);
  string pfx= "syntax:" * lan->lan_name * ":";
  lan->color_decoding (-1)= get_preference (pfx * "none", "red");
  lan->color_decoding (1) = get_preference (pfx * "comment", "brown");
  lan->color_decoding (3) = get_preference (pfx * "error", "dark red");
  lan->color_decoding (4) = get_preference (pfx * "preprocessor", "#004000");
  lan->color_decoding (5) = get_preference (pfx * "preprocessor_directive", "#20a000");
  lan->color_decoding (10)= get_preference (pfx * "constant", "#4040c0");
  lan->color_decoding (11)= get_preference (pfx * "constant_identifier", "#4040c0");
  lan->color_decoding (12)= get_preference (pfx * "constant_function", "#4040c0");
  lan->color_decoding (13)= get_preference (pfx * "constant_type", "#4040c0");
  lan->color_decoding (14)= get_preference (pfx * "constant_category", "#4040c0");
  lan->color_decoding (15)= get_preference (pfx * "constant_module", "#4040c0");
  lan->color_decoding (16)= get_preference (pfx * "constant_number", "#3030b0");
  lan->color_decoding (17)= get_preference (pfx * "constant_string", "dark grey");
  lan->color_decoding (18)= get_preference (pfx * "constant_char", "#333333");
  lan->color_decoding (20)= get_preference (pfx * "variable", "#606060");
  lan->color_decoding (21)= get_preference (pfx * "variable_identifier", "#204080");
  lan->color_decoding (22)= get_preference (pfx * "variable_function", "#606060");
  lan->color_decoding (23)= get_preference (pfx * "variable_type", "#00c000");
  lan->color_decoding (24)= get_preference (pfx * "variable_category", "#00c000");
  lan->color_decoding (25)= get_preference (pfx * "variable_module", "#00c000");
  lan->color_decoding (26)= get_preference (pfx * "variable_ioarg", "#00b000");
  lan->color_decoding (30)= get_preference (pfx * "declare", "#0000c0");
  lan->color_decoding (31)= get_preference (pfx * "declare_identifier", "#0000c0");
  lan->color_decoding (32)= get_preference (pfx * "declare_function", "#0000c0");
  lan->color_decoding (33)= get_preference (pfx * "declare_type", "#0000c0");
  lan->color_decoding (34)= get_preference (pfx * "declare_category", "#d030d0");
  lan->color_decoding (35)= get_preference (pfx * "declare_module", "#0000c0");
  lan->color_decoding (40)= get_preference (pfx * "operator", "#8b008b");
  lan->color_decoding (41)= get_preference (pfx * "operator_openclose", "#B02020");
  lan->color_decoding (42)= get_preference (pfx * "operator_field", "#888888");
  lan->color_decoding (43)= get_preference (pfx * "operator_special", "orange");
  lan->color_decoding (50)= get_preference (pfx * "keyword", "#309090");
  lan->color_decoding (51)= get_preference (pfx * "keyword_conditional", "#309090");
  lan->color_decoding (52)= get_preference (pfx * "keyword_control", "#000080");
}

int
encode_color (string s) {
  if (N(language_rep::color_encoding) == 0) initialize_color_encodings ();
  if (language_rep::color_encoding->contains (s))
    return language_rep::color_encoding[s];
  else return -1;
}

string
decode_color (string lan_name, int c) {
  language lan= prog_language (lan_name);
  if (N(lan->color_decoding) == 0) initialize_color_decodings (lan_name);
  if (lan->color_decoding->contains (c)) return lan->color_decoding[c];
  else return "";
}

/******************************************************************************
* Disable hyphenation
******************************************************************************/

struct hyphenless_language_rep: language_rep {
  language base;
  hyphenless_language_rep (string lan_name, language lan);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

hyphenless_language_rep::hyphenless_language_rep (string nm, language lan):
  language_rep (nm), base (lan) {}

text_property
hyphenless_language_rep::advance (tree t, int& pos) {
  return base->advance (t, pos);
}

array<int>
hyphenless_language_rep::get_hyphens (string s) {
  ASSERT (N(s) != 0, "hyphenation of empty string");
  int i, n= N(s)-1;
  array<int> penalty (n);
  for (i=0; i<n; i++) penalty[i]= HYPH_INVALID;
  return penalty;
}

void
hyphenless_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  base->hyphenate (s, after, left, right);
}

language
hyphenless_language (language base) {
  string name= base->res_name * "-hyphenless";
  if (language::instances -> contains (name)) return language (name);
  return tm_new<hyphenless_language_rep> (name, base);
}

/******************************************************************************
* Ad hoc hyphenation patterns
******************************************************************************/

struct ad_hoc_language_rep: language_rep {
  language base;
  hashmap<string,string> hyphens;

  ad_hoc_language_rep (string lan_name, language lan, tree hyphs);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s);
  void hyphenate (string s, int after, string& left, string& right);
};

ad_hoc_language_rep::ad_hoc_language_rep (string nm, language lan, tree hyphs):
  language_rep (nm), base (lan), hyphens ("?")
{
  if (is_atomic (hyphs)) {
    string h= hyphs->label;
    string s= replace (h, "-", "");
    hyphens (s)= h;
  }
}

text_property
ad_hoc_language_rep::advance (tree t, int& pos) {
  return base->advance (t, pos);
}

array<int>
ad_hoc_language_rep::get_hyphens (string s) {
  if (hyphens->contains (s)) {
    string h= hyphens[s];
    array<int> penalty;
    for (int i=0; i<N(h); tm_char_forwards (h, i))
      if (h[i] != '-') penalty << HYPH_INVALID;
      else if (N(penalty)>0) penalty[N(penalty)-1]= 0;
    return penalty;
  }
  else return base->get_hyphens (s);
}

void
ad_hoc_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{
  if (hyphens->contains (s)) {
    array<int> penalty= get_hyphens (s);
    std_hyphenate (s, after, left, right, penalty[after], true);
  }
  else base->hyphenate (s, after, left, right);
}

language
ad_hoc_language (language base, tree hyphs) {
  static hashmap<tree,int> abbrevs;
  if (!abbrevs->contains (hyphs))
    abbrevs (hyphs)= N (abbrevs);
  string name= base->res_name * "-" * as_string (abbrevs [hyphs]);
  if (language::instances -> contains (name)) return language (name);
  return tm_new<ad_hoc_language_rep> (name, base, hyphs);
}

/******************************************************************************
* Interface with spell engines and cache
******************************************************************************/

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_spellservice.h"
#define ispell_start mac_spell_start
#define ispell_check mac_spell_check
#define ispell_accept mac_spell_accept
#define ispell_insert mac_spell_insert
#define ispell_done mac_spell_done
#else
#include "Ispell/ispell.hpp"
#endif

static bool spell_active= false;
static hashmap<string,bool> spell_busy (false);
static hashmap<string,int > spell_cache (0);
static hashmap<string,bool> spell_temp (false);

void
spell_start () {
  spell_active= true;
}

void
spell_done () {
  spell_active= false;
  hashmap<string,bool> h= copy (spell_busy);
  for (iterator<string> it= iterate (h); it->busy (); )
    spell_done (it->next ());
}

string
spell_start (string lan) {
  if (spell_busy->contains (lan)) return "ok";
  spell_busy (lan)= true;
  return ispell_start (lan);
}

void
spell_done (string lan) {
  if (spell_active) return;
  spell_busy->reset (lan);
  ispell_done (lan);
  hashmap<string,bool> aux (false);
  for (iterator<string> it= iterate (spell_temp); it->busy (); ) {
    string key= it->next ();
    if (starts (key, lan * ":")) aux (key)= true;
  }
  for (iterator<string> it= iterate (aux); it->busy (); ) {
    string key= it->next ();
    spell_cache->reset (key);
    spell_temp ->reset (key);
  }
}

tree
spell_check (string lan, string s) {
  if (spell_busy->contains (lan)) {
    if (lan == "verbatim") return "ok";
    string f= uni_Locase_all (s);
    if (f == s) {
      tree r= ispell_check (lan, s);
      return r;
    }
    else {
      string l= uni_locase_all (s);
      tree r= ispell_check (lan, l);
      if (s == uni_upcase_all (s) && is_tuple (r))
        for (int i=1; i<N(r); i++)
          if (is_atomic (r[i]))
            r[i]= uni_upcase_all (r[i]->label);
      return r;
    }
  }
  else {
    if (spell_start (lan) == "ok"){
      tree r= spell_check (lan, s);
      spell_done (lan);
      return r;
    }
    else {
      spell_active= false;
      spell_done (lan);
      return "ok";  
    }
  }
}

bool
check_word (string lan, string s) {
  string key= lan * ":" * s;
  string f= uni_Locase_all (s);
  string l= uni_locase_first (f);
  if (s != l && s != f) key= lan * ":" * l;
  int val= spell_cache[key];
  if (val == 0) {
    tree t= spell_check (lan, s);
    if (t == "ok") val= 1;
    else val= -1;
    spell_cache (key)= val;
  }
  return val == 1;
}

void
spell_accept (string lan, string s, bool permanent) {
  string f= uni_Locase_all (s);
  string l= uni_locase_first (f);
  if (s != f) s= l;
  string key= lan * ":" * s;
  spell_cache (key) = 1;
  if (!permanent) spell_temp (key)= 1;
  ispell_accept (lan, s);
}

void
spell_insert (string lan, string s) {
  string f= uni_Locase_all (s);
  string l= uni_locase_first (f);
  if (s != f) s= l;
  string key= lan * ":" * s;
  spell_cache (key) = 1;
  ispell_insert (lan, s);
}
