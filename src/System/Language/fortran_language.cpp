
/******************************************************************************
* MODULE     : fortran_language.cpp
* DESCRIPTION: Fortran 2008 language
* COPYRIGHT  : (C) 2019  Marduk Bolaños, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "scheme.hpp"

fortran_language_rep::fortran_language_rep (string name):
  abstract_language_rep (name)
{
  number_parser.use_fortran_style ();
  inline_comment_parser.set_starts (list<string>("!"));
}

text_property
fortran_language_rep::advance (tree t, int& pos) {
  string s= t->label;
  if (pos>=N(s)) return &tp_normal_rep;
  char c= s[pos];
  if (c == ' ') {
    pos++; return &tp_space_rep; }
  if (number_parser.parse (s, pos)) {
    return &tp_normal_rep;
  }
  if (belongs_to_identifier (c)) {
    parse_alpha (s, pos); return &tp_normal_rep; }
  tm_char_forwards (s, pos);
  return &tp_normal_rep;
}

array<int>
fortran_language_rep::get_hyphens (string s) {
  int i;
  array<int> penalty (N(s)+1);
  penalty[0]= HYPH_INVALID;
  for (i=1; i<N(s); i++)
    if (s[i-1] == '-' && is_alpha (s[i]))
      penalty[i]= HYPH_STD;
    else penalty[i]= HYPH_INVALID;
  penalty[i]= HYPH_INVALID;
  return penalty;
}

void
fortran_language_rep::hyphenate (
  string s, int after, string& left, string& right)
{ 
  left = s (0, after);
  right= s (after, N(s));
}

static void
fortran_color_setup_constants (hashmap<string, string> & t) {
  string c= "constant";
  t ("*,")= c;
  t ("in")= c;
  t ("inout")= c;
  t ("none")= c;
  t ("out")= c;
  t ("REAL32")= c;
  t ("REAL64")= c;
  t ("REAL128")= c;
}

static void
fortran_color_setup_declare_types (hashmap<string, string> & t) {
  string c= "declare_type";
  t ("type")= c;
  t ("class")= c;
}


static void
fortran_color_setup_declare_function (hashmap<string, string> & t) {
  string c= "declare_function";
  t ("abstract")= c;
  t ("associate")= c;
  t ("block")= c;
  t ("data")= c;
  t ("function")= c;
  t ("subroutine")= c;
  t ("interface")= c;
  t ("module")= c;
  t ("submodule")= c;
  t ("program")= c;
  t ("end")= c;
}

static void
fortran_color_setup_keywords (hashmap<string, string> & t) {
  string c= "keyword";
  t ("allocatable")= c;
  t ("call")= c;
  t ("contains")= c;
  t ("cycle")= c;
  t ("elemental")= c;
  t ("error")= c;
  t ("exit")= c;
  t ("external")= c;
  t ("implicit")= c;
  t ("intrinsic")= c;
  t ("is")= c;
  t ("only")= c;
  t ("optional")= c;
  t ("parameter")= c;
  t ("pointer")= c;
  t ("private")= c;
  t ("protected")= c;
  t ("public")= c;
  t ("pure")= c;
  t ("recursive")= c;
  t ("return")= c;
  t ("save")= c;
  t ("stop")= c;
  t ("sync all")= c;
  t ("target")= c;
  t ("use")= c;
}

static void
fortran_color_setup_variable_types (hashmap<string, string> & t) {
  string c= "variable_type";
  t ("character")= c;
  t ("complex")= c;
  t ("integer")= c;
  t ("logical")= c;
  t ("procedure")= c;
  t ("real")= c;
}

static void
fortran_color_setup_constant_functions (hashmap<string, string> & t) {
  string c= "constant_function";
  t ("allocate")= c;
  t ("close")= c;
  t ("codimension")= c;
  t ("deallocate")= c;
  t ("dimension")= c;
  t ("flush")= c;
  t ("inquire")= c;
  t ("intent")= c;
  t ("nullify")= c;
  t ("open")= c;
  t ("print")= c;
  t ("read")= c;
  t ("result")= c;
  t ("write")= c;
}

static void
fortran_color_setup_variable_functions (hashmap<string, string> & t) {
  string c= "variable_function";
  t ("abs")= c;
  t ("aimag")= c;
  t ("aint")= c;
  t ("anint")= c;
  t ("ceiling")= c;
  t ("cmplx")= c;
  t ("conjg")= c;
  t ("dble")= c;
  t ("dim")= c;
  t ("dprod")= c;
  t ("floor")= c;
  t ("int")= c;
  t ("max")= c;
  t ("min")= c;
  t ("mod")= c;
  t ("modulo")= c;
  t ("nint")= c;
  //t ("real")= c;
  t ("sign")= c;

  t ("acos")= c;
  t ("asin")= c;
  t ("atan")= c;
  t ("atan2")= c;
  t ("bessel_j0")= c;
  t ("bessel_j1")= c;
  t ("bessel_jn")= c;
  t ("bessel_y0")= c;
  t ("bessel_y1")= c;
  t ("bessel_yn")= c;
  t ("cos")= c;
  t ("cosh")= c;
  t ("erf")= c;
  t ("erfc")= c;
  t ("erfc_scaled")= c;
  t ("exp")= c;
  t ("gamma")= c;
  t ("log")= c;
  t ("log10")= c;
  t ("log_gamma")= c;
  t ("sin")= c;
  t ("sinh")= c;
  t ("sqrt")= c;
  t ("tan")= c;
  t ("tanh")= c;


  t ("dot_product")= c;
  t ("matmul")= c;
  t ("product")= c;
  t ("sum")= c;

  t ("random_init")= c;
  t ("random_number")= c;
  t ("random_seed")= c;

  t ("digits")= c;
  t ("epsilon")= c;
  t ("huge")= c;
  t ("maxexponent")= c;
  t ("minexponent")= c;
  t ("precision")= c;
  t ("radix")= c;
  t ("range")= c;
  t ("tiny")= c;

  t ("exponent")= c;
  t ("fraction")= c;
  t ("nearest")= c;
  t ("rrspacing")= c;
  t ("scale")= c;
  t ("set_exponent")= c;
  t ("spacing")= c;

  t ("bit_size")= c;
  t ("btest")= c;
  t ("iand")= c;
  t ("ibclr")= c;
  t ("ibits")= c;
  t ("ibset")= c;
  t ("ieor")= c;
  t ("ior")= c;
  t ("ishft")= c;
  t ("ishftc")= c;
  t ("not")= c;

  t ("achar")= c;
  t ("adjustl")= c;
  t ("adjustr")= c;
  t ("char")= c;
  t ("iachar")= c;
  t ("ichar")= c;
  t ("index")= c;
  t ("len")= c;
  t ("len_trim")= c;
  t ("lge")= c;
  t ("lgt")= c;
  t ("lle")= c;
  t ("llt")= c;
  t ("repeat")= c;
  t ("scan")= c;
  t ("trim")= c;
  t ("verify")= c;
  t ("trim")= c;

  t ("kind")= c;
  t ("selected_int_kind")= c;
  t ("selected_real_kind")= c;

  t ("date_and_time")= c;
  t ("execute_command_line")= c;
  t ("get_command_argument")= c;
  t ("get_environment_variable")= c;
  t ("system_clock")= c;

  t ("merge")= c;

  t ("allocated")= c;
  t ("size")= c;
  t ("storage_size")= c;

  t ("num_images")= c;
  t ("this_image")= c;

  t ("present")= c;

  t ("maxval")= c;
  t ("reshape")= c;
  t ("shape")= c;
  t ("transpose")= c;
}



static void
fortran_color_setup_keywords_conditional (hashmap<string, string> & t) {
  string c= "keyword_conditional";
  t ("if")= c;
  t ("then")= c;
  t ("else")= c;
  t ("else if")= c;
  t ("endif")= c;
  t ("for")= c;
  t ("forall")= c;
  t ("do")= c;
  t ("enddo")= c;
  t ("case")= c;
  t ("concurrent")= c;
  t ("select")= c;
  t ("while")= c;
}

static void
fortran_color_setup_keywords_control (hashmap<string, string> & t) {
  string c= "keyword_control";
  t ("continue")= c;
  t ("pause")= c;
  t ("return")= c;
}

static void
fortran_color_setup_operator (hashmap<string, string>& t) {
  string c= "operator";
  t ("+")= c;
  t ("-")= c;
  t ("*")= c;
  t ("/")= c;
  t ("**")= c;
  t ("=")= c;
  t ("==")= c;
  t ("/=")= c;
  t ("<less>")= c;
  t ("<gtr>")= c;
  t ("<less>=")= c;
  t ("<gtr>=")= c;
  t ("=<gtr>")= c;
  t (".and.")= c;
  t (".or.")= c;
  t (".not.")= c;
  t (".eqv.")= c;
  t (".neqv.")= c;
  t ("//")= c;
}

static void
fortran_color_setup_operator_special (hashmap<string, string> & t) {
  string c= "operator_special";
  t ("&")= c;
  t ("::")= c;
}

static void
fortran_color_setup_operator_openclose (hashmap<string, string> & t) {
  string c= "operator_openclose";
  t ("[")= c;
  t ("(")= c;
  t (")")= c;
  t ("]")= c;
}

static void
fortran_color_setup_operator_field (hashmap<string, string> & t) {
  t ("%")= "operator_field";
}

static bool
string_end (string s, int pos) {
  while (pos<N(s) && (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '.')) pos++;
  if (pos == N(s) || (pos + 1 < N(s) && s[pos] == '/' && s[pos+1] == '/'))
    return true;
  return false;
}

static bool
parse_string (string s, int& pos) {
  int start= pos;
  if (pos >= N(s)) return false;
  if (s[pos] == '\"' || s[pos] == '\'') {
    pos++;
    while (pos < N(s)) {
      if (s[pos] == '\"' || s[pos] == '\'') {
        pos++;
        if (pos < N(s) && !(s[pos] == '\"' || s[pos] == '\'')) {
          return false;
        }
      }
      else if (pos + 1 < N(s) && s[pos] == '.' && s[pos+1] == '.') {
        if (string_end (s, pos)) {
          pos--;
          return true;
        }
      }
      pos++;
    }
    if (pos >= N(s) && s[start] == '\'') {
      // A non ended quote means a transpose operator
      pos= start;
      return false;
    }
  }
  return false;
}

 
string
fortran_language_rep::parse_keywords (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos>=N(s)) return "";
  if (is_digit (s[i])) return "";
  while ((i<N(s)) && belongs_to_identifier (s[i])) i++;
  string r= s (pos, i);
  if (t->contains (r)) {
    string tr= t(r);
    if (tr == "keyword_conditional" ||
        tr == "keyword_control" ||
        tr == "keyword" ||
        tr == "declare_function" ||
        tr == "declare_type" ||
        tr == "variable_type" ||
        tr == "constant_function" ||
        tr == "variable_function" ||
        tr == "constant") {
      pos=i;
      return tr;
    }
  }
  return "";
}

string
fortran_language_rep::parse_operators (hashmap<string,string>& t, string s, int& pos) {
  int i;
  for (i=11; i>=1; i--) {
    string r=s(pos,pos+i);
    if (t->contains (r)) {
      if (t(r) == "operator") {
        pos=pos+i;
        return "operator";
      }
      else if (t(r) == "operator_special") {
        if (s[pos] == '.')
          do pos++; while (s[pos] == '.');
        else
          pos=pos+i;
        return "operator_special";
      }
      else if (t(r) == "operator_openclose") {
        pos=pos+i;
        return "operator_openclose";
      }
      else if (t(r) == "operator_field") {
        pos=pos+i;
        while ((pos<N(s)) && belongs_to_identifier (s[pos])) pos++;
        return "operator_field";
      }
    }
  }
  return "";
}

string
fortran_language_rep::get_color (tree t, int start, int end) {
  static bool setup_done= false;
  if (!setup_done) {
    /* 
     * NOTE: it seems there is no way to take into account multiline
     * dependencies. Then such weird syntax like
     *
     * str= "some string beginning ... // some comment
     * some string end"
     *
     * will not be correctly typeset. For the same reasons, i/o function args
     * are not highlighted.
     *
     */

    fortran_color_setup_constants (colored);
    fortran_color_setup_declare_function (colored);
    fortran_color_setup_declare_types (colored);
    fortran_color_setup_keywords (colored);
    fortran_color_setup_keywords_conditional (colored);
    fortran_color_setup_keywords_control (colored);
    fortran_color_setup_variable_types (colored);
    fortran_color_setup_constant_functions (colored);
    fortran_color_setup_variable_functions (colored);
    fortran_color_setup_operator (colored);
    fortran_color_setup_operator_special (colored);
    fortran_color_setup_operator_openclose (colored);
    fortran_color_setup_operator_field (colored);
    setup_done= true;
  }

  static string none= "";
  if (start >= end) return none;
  string s= t->label;
  int pos= 0;
  int opos=0;
  string type;
  bool cut_str= false;
  do {
    type= none;
    do {
      opos= pos;
      if (cut_str) {
        while (s[pos] == '.') pos++;
        if (opos < pos) {
          type= "operator_special";
          break;
        }
      }
      if (blanks_parser.parse (s, pos)) {
        break;
      }
      if (inline_comment_parser.parse (s, pos)) {
        type= "comment";
        break;
      }
      cut_str= parse_string (s, pos);
      if (opos < pos) {
        type= "constant_string";
        break;
      }

      if (!cut_str) {
        type= parse_keywords (colored, s, pos);
        if (opos < pos) {
          break;
        }
        type= parse_operators (colored, s, pos);
        if (opos < pos) {
          break;
        }
        if (number_parser.parse (s, pos)) {
          type= "constant_number";
          break;
        }
        parse_identifier (colored, s, pos);
        if (opos < pos) {
          type= none;
          break;
        }
      }
      pos= opos;
      pos++;
    }
    while (false);
  }
  while (pos <= start);
  if (type == none) return none;
  return decode_color ("fortran", encode_color (type));
}
