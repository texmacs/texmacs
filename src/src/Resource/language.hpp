
/******************************************************************************
* MODULE     : language.hpp
* DESCRIPTION: language specific features, which include
*              - punctuation rules
*              - hyphenation
*              - (future) dictionary and grammar rumes for spell-checker
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LANGUAGE_H
#define LANGUAGE_H
#include "space.hpp"
#include "array.hpp"
#include "encoding.hpp"

RESOURCE(language);

/******************************************************************************
* The text property class
******************************************************************************/

#define TP_NORMAL     0
#define TP_HYPH       1
#define TP_SPACE      2
#define TP_BLANK      3
#define TP_PERIOD     4
#define TP_OPERATOR   5
#define TP_SHORTOP    6
#define TP_OTHER      7

#define SPC_NONE      0
#define SPC_SPACE     1
#define SPC_PERIOD    2
#define SPC_TINY      3
#define SPC_OPERATOR  4
#define SPC_BIGOP     5

#define HYPH_STD      10000
#define HYPH_PANIC    1000000
#define HYPH_INVALID  100000000

#define OP_UNKNOWN           0
#define OP_SYMBOL            1
#define OP_PREFIX            2
#define OP_POSTFIX           3
#define OP_INFIX             4
#define OP_LEFT_ASS_INFIX    5
#define OP_RIGHT_ASS_INFIX   6
#define OP_ASS_INFIX         7
#define OP_OPENING_BRACKET   8
#define OP_SEPARATOR         9
#define OP_CLOSING_BRACKET   10

#define LIMITS_NONE          0
#define LIMITS_DISPLAY       1
#define LIMITS_ALWAYS        2

struct text_property_rep {
  int  type;
  int  spc_before;
  int  spc_after;
  int  pen_before;
  int  pen_after;

  int  op_type;
  int  priority;
  int  limits;

  text_property_rep (int type= TP_OTHER,
		     int spc_before= SPC_NONE, int spc_after= SPC_NONE,
		     int pen_before= 0, int pen_after= HYPH_INVALID,
		     int op_type= OP_SYMBOL, int priority= 1000,
		     int limits= LIMITS_NONE);
};

typedef text_property_rep* text_property;

/******************************************************************************
* The language structure
******************************************************************************/

struct language_rep: rep<language> {
  string   lan_name;  // name of the language
  encoding enc;       // the underlying encoding of the language

  inline language_rep (string s, encoding enc= math_enc);
  virtual text_property advance (string s, int& pos) = 0;
  virtual array<int> get_hyphens (string s) = 0;
  virtual void hyphenate (string s, int after, string& l, string& r) = 0;
};

inline language_rep::language_rep (string s, encoding enc2):
  rep<language> (s), enc(enc2) {}

language text_language (string s);
language math_language (string s);
language prog_language (string s);

string locale_to_language (string s);
string language_to_locale (string s);
string get_locale_language ();
string get_date (string lan, string fm);

string math_symbol_type (string s, string lan= "texmath");

#endif // defined LANGUAGE_H
