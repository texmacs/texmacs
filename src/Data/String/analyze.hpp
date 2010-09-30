
/******************************************************************************
* MODULE     : analyze.hpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ANALYZE_H
#define ANALYZE_H
#include "string.hpp"
#include "array.hpp"
#include "hashset.hpp"

class object;

bool is_alpha (char c);
bool is_iso_alpha (char c);
bool is_locase (char c);
bool is_upcase (char c);
bool is_digit (char c);
bool is_numeric (char c);
bool is_punctuation (char c);
bool is_space (char s);
bool is_alpha (string s);
bool is_locase_alpha (string s);
bool is_iso_alpha (string s);
bool is_numeric (string s);

char   upcase (char s);
char   locase (char s);
string upcase_first (string s);
string locase_first (string s);
string upcase_all (string s);
string locase_all (string s);
string string_union (string s1, string s2);
string string_minus (string s1, string s2);
string iso_to_koi8 (string s);
string koi8_to_iso (string s);
string iso_to_koi8uk (string s);
string koi8uk_to_iso (string s);
string il2_to_cork (string s);
string cork_to_il2 (string s);
string ispanish_to_spanish (string s);
string spanish_to_ispanish (string s);
string igerman_to_german (string s);
string german_to_igerman (string s);
string old_tm_to_xml_cdata (string s);
object tm_to_xml_cdata (string s);
string old_xml_cdata_to_tm (string s);
string tm_to_xml_name (string s);
string xml_name_to_tm (string s);
string xml_unspace (string s, bool first, bool last);
bool   contains_unicode_char (string s);

string roman_nr (int nr);
string Roman_nr (int nr);
string alpha_nr (int nr);
string Alpha_nr (int nr);
string fnsymbol_nr (int nr);

string as_hexadecimal (int i);
string as_hexadecimal (pointer ptr);
string as_hexadecimal (int i, int length);
int    from_hexadecimal (string s);

string tm_encode (string s);
string tm_decode (string s);
string tm_var_encode (string s);
string tm_correct (string s);
void   tm_char_forwards (string s, int& pos);
void   tm_char_backwards (string s, int& pos);
int    tm_char_next (string s, int pos);
int    tm_char_previous (string s, int pos);
string tm_forward_access (string s, int i);
string tm_backward_access (string s, int i);
int    tm_string_length (string s);
array<string> tm_tokenize (string s);
string tm_recompose (array<string> a);

string scm_quote (string s);
string scm_unquote (string s);
string raw_quote (string s);
string raw_unquote (string s);
string escape_sh (string s);
string escape_generic (string s);
string escape_verbatim (string s);
string escape_spaces (string s);
string dos_to_better (string s);

bool test (string s, int i, const char* test);
bool test (string s, int i, string test);
bool starts (string s, const char* test);
bool starts (string s, const string test);
bool ends (string s, const char* test);
bool ends (string s, const string test);
bool read (string s, int& i, const char* test);
bool read (string s, int& i, string test);
bool read_line (string s, int& i, string& result);
bool read_int (string s, int& i, int& result);
bool read_double (string s, int& i, double& result);
void skip_spaces (string s, int& i);
void skip_line (string s, int& i);
void skip_symbol (string s, int& i);

void parse (string s, int& pos, QI& ret);
void parse (string s, int& pos, QN& ret);
void parse (string s, int& pos, HI& ret);
void parse (string s, int& pos, HN& ret);
void parse (string s, int& pos, SI& ret);
void parse (string s, int& pos, SI*& a, int len);

int    search_forwards (string what, string in);
int    search_forwards (string what, int pos, string in);
int    search_backwards (string what, string in);
int    search_backwards (string what, int pos, string in);
int    count_occurrences (string what, string in);
int    count_occurrences (string what, string in);
bool   occurs (string what, string in);
string replace (string s, string what, string by);
bool   match_wildcard (string s, string w);

array<string> as_completions (hashset<string> h);
array<string> close_completions (array<string> a);
array<string> strip_completions (array<string> a, string prefix);

#endif // defined ANALYZE_H
