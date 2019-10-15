
/******************************************************************************
* MODULE     : language.hpp
* DESCRIPTION: language specific features, which include
*              - punctuation rules
*              - hyphenation
*              - (future) dictionary and grammar rumes for spell-checker
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LANGUAGE_H
#define LANGUAGE_H
#include "space.hpp"
#include "array.hpp"
#include "resource.hpp"

RESOURCE(language);

/******************************************************************************
* The text property class
******************************************************************************/

#define TP_NORMAL             0
#define TP_HYPH               1
#define TP_THIN_SPACE         2
#define TP_SPACE              3
#define TP_DSPACE             4
#define TP_NB_THIN_SPACE      5
#define TP_NB_SPACE           6
#define TP_NB_DSPACE          7
#define TP_PERIOD             8
#define TP_CJK_NORMAL         9
#define TP_CJK_NO_BREAK      10
#define TP_CJK_PERIOD        11
#define TP_CJK_WIDE_PERIOD   12
#define TP_OPERATOR          13
#define TP_SHORTOP           14
#define TP_OTHER             15

#define SPC_NONE              0
#define SPC_THIN_SPACE        1
#define SPC_SPACE             2
#define SPC_DSPACE            3
#define SPC_PERIOD            4
#define SPC_TINY              5
#define SPC_CJK_NORMAL        6
#define SPC_CJK_PERIOD        7
#define SPC_CJK_WIDE_PERIOD   8
#define SPC_HALF              9
#define SPC_OPERATOR         10
#define SPC_WIDEOP           11
#define SPC_BIGOP            12
#define SPC_SHORT_APPLY      13
#define SPC_APPLY            14
#define SPC_MULTIPLY         15
#define SPC_MIDDLE           16
#define SPC_END_MARKER       17

#define HYPH_STD       10000
#define HYPH_PANIC     1000000
#define HYPH_INVALID   100000000

#define OP_UNKNOWN            0
#define OP_TEXT               1
#define OP_SKIP               2
#define OP_SYMBOL             3
#define OP_UNARY              4
#define OP_BINARY             5
#define OP_N_ARY              6
#define OP_PREFIX             7
#define OP_POSTFIX            8
#define OP_INFIX              9
#define OP_APPLY             10
#define OP_SEPARATOR         11
#define OP_OPENING_BRACKET   12
#define OP_MIDDLE_BRACKET    13
#define OP_CLOSING_BRACKET   14
#define OP_BIG               15
#define OP_TOTAL             16

#define LIMITS_NONE           0
#define LIMITS_DISPLAY        1
#define LIMITS_ALWAYS         2

struct text_property_rep {
  int  type;
  int  spc_before;
  int  spc_after;
  int  pen_before;
  int  pen_after;

  int  op_type;
  int  priority;
  int  limits;

  tree_label macro;

  text_property_rep (int type= TP_OTHER,
		     int spc_before= SPC_NONE, int spc_after= SPC_NONE,
		     int pen_before= 0, int pen_after= HYPH_INVALID,
		     int op_type= OP_SYMBOL, int priority= 1000,
		     int limits= LIMITS_NONE,
                     tree_label macro= STRING);
};

text_property_rep copy (text_property_rep tpr);
typedef text_property_rep* text_property;

/******************************************************************************
* Possible successions of mathematical operators
******************************************************************************/

#define SUCCESSION_OK         0
#define REMOVE_SPACE_BEFORE   1
#define REMOVE_CURRENT_SPACE  2
#define REMOVE_ALL_SPACE      3

extern int succession_status_table [OP_TOTAL * OP_TOTAL];
inline int succession_status (int op1, int op2) {
  return succession_status_table [op1 * OP_TOTAL + op2]; }
//int succession_status (int op1, int op2);
void init_succession_status_table ();

/******************************************************************************
* The language structure
******************************************************************************/

struct language_rep: rep<language> {
  string lan_name;  // name of the language
  int hl_lan;
  static hashmap<string,int> color_encoding;
  hashmap<int,string> color_decoding;
  
  language_rep (string s);
  virtual text_property advance (tree t, int& pos) = 0;
  virtual array<int> get_hyphens (string s) = 0;
  virtual void hyphenate (string s, int after, string& l, string& r) = 0;
  virtual string get_group (string s);
  virtual array<string> get_members (string s);
  virtual void highlight (tree t);
  virtual string get_color (tree t, int start, int end);
};

array<string> get_supported_languages ();
language text_language (string s);
language math_language (string s);
language prog_language (string s);
language hyphenless_language (language base);
language ad_hoc_language (language base, tree hyphs);

string math_symbol_group (string s, string lan= "std-math");
array<string> math_group_members (string s, string lan= "std-math");
string math_symbol_type (string s, string lan= "std-math");

void   initialize_color_decodings (string lan_name);
int    encode_color (string s);
string decode_color (string lan, int c);

void spell_start ();
void spell_done ();
string spell_start (string lan);
void spell_done (string lan);
tree spell_check (string lan, string s);
bool check_word (string lan, string s);
void spell_accept (string lan, string s, bool permanent= false);
void spell_insert (string lan, string s);

#endif // defined LANGUAGE_H
