
/******************************************************************************
* MODULE     : parser.hpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PARSER_H
#define PARSER_H 
#include "string.hpp"

class number_parser_rep {
private:
  bool long_suffix= false;
  bool double_suffix= false;
  bool float_suffix= false;
  bool j_suffix= false;
  bool ull_suffix= false;
  bool locase_i_suffix= false;
  bool scientific_notation= false;
  bool prefix_0b= false;
  bool prefix_0x= false;
  bool prefix_0o= false;
  bool separator= false;
  char sep = '_';

public:
  number_parser_rep () {};
  void parse (string s, int& pos);
  void parse_binary (string s, int& pos);
  void parse_hex (string s, int& pos);
  void parse_octal (string s, int &pos);
  void parse_suffix (string s, int&pos);

  inline void support_double_suffix(bool param) { double_suffix= param; }
  inline void support_float_suffix(bool param) { float_suffix= param; }
  inline void support_j_suffix(bool param) { j_suffix= param; }
  inline void support_long_suffix(bool param) { long_suffix= param; }
  inline void support_ull_suffix(bool param) { ull_suffix= param; }
  inline void support_locase_i_suffix(bool param) { locase_i_suffix= param; }
  inline void support_scientific_notation (bool param) { scientific_notation= param; }
  inline void support_prefix_0x (bool param) { prefix_0x= param; }
  inline void support_prefix_0b (bool param) { prefix_0b= param; }
  inline void support_prefix_0o (bool param) { prefix_0o= param; }
  inline void support_separator (char param) { sep= param; separator= true; }
  inline bool is_separator (char param) { return separator && sep == param; }

  void use_cpp_style ();
  void use_python_style ();
  void use_fortran_style ();
  void use_java_style ();
  void use_scala_style ();
  void use_r_style ();
};

#endif // defined PARSER_H
