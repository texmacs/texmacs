
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
#include "ntuple.hpp"

class object;

inline bool is_alpha (char c) {
  return ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z')); }
inline bool is_locase (char c) {
  return (c>='a') && (c<='z'); }
inline bool is_upcase (char c) {
  return (c>='A') && (c<='Z'); }
inline bool is_digit (char c) {
  return (c>='0') && (c<='9'); }
inline bool is_binary_digit (char c) {
  return c == '0' || c == '1'; }
inline bool is_octal_digit (char c) {
  return c >= '0' && c <= '7'; }
inline bool is_hex_digit (char c) {
  return (c >= '0' && c <= '9') ||
    (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'); }
inline bool is_numeric (char c) {
  return ((c>='0') && (c<='9')) || (c=='.'); }
inline bool is_punctuation (char c) {
  return (c=='.') || (c==',') || (c==':') || (c=='\'') || (c=='`') ||
    (c==';') || (c=='!') || (c=='?'); }
inline bool is_space (char c) {
  return (c == ' ') || (c == '\11') || (c == '\12') || (c == '\15'); }

bool is_iso_alpha (char c);
bool is_iso_locase (char c);
bool is_iso_upcase (char c);

bool is_alpha (string s);
bool is_locase_alpha (string s);
bool is_iso_alpha (string s);
bool is_numeric (string s);

char   upcase (char s);
char   locase (char s);
char   closing_delimiter (char c);
string upcase_first (string s);
string locase_first (string s);
string upcase_all (string s);
string locase_all (string s);
string string_union (string s1, string s2);
string string_minus (string s1, string s2);
string utf8_to_t2a (string s);
string t2a_to_utf8 (string s);
string utf8_to_cork (string s);
string cork_to_utf8 (string s);
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
bool   contains_unicode_char (string s);
tree   convert_OTS1_symbols_to_universal_encoding (tree t);
string convert_tabs_to_spaces (string s, int w);
string downgrade_math_letters (string s);

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
array<string> tm_string_split (string s);
string tm_recompose (array<string> a);
int    tm_search_forwards (string s, int pos, string in);
int    tm_search_backwards (string s, int pos, string in);

string scm_quote (string s);
string scm_unquote (string s);
string raw_quote (string s);
string raw_unquote (string s);
string escape_sh (string s);
string escape_generic (string s);
string escape_verbatim (string s);
string escape_spaces (string s);
string unescape_guile (string s);
string dos_to_better (string s);

bool test (string s, int i, const char* test);
bool test (string s, int i, string test);
bool starts (string s, const char* test);
bool starts (string s, const string test);
bool ends (string s, const char* test);
bool ends (string s, const string test);
bool read (string s, int& i, const char* test);
bool read (string s, int& i, string test);
bool read (string s, string test);
bool read_line (string s, int& i, string& result);
bool read_int (string s, int& i, int& result);
bool read_double (string s, int& i, double& result);
bool read_word (string s, int& i, string& result);
bool is_whitespace (string s);
void skip_spaces (string s, int& i);
void skip_whitespace (string s, int& i);
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
int    search_forwards (array<string> what_list, int pos, string in);
int    search_backwards (string what, string in);
int    search_backwards (string what, int pos, string in);
int    count_occurrences (string what, string in);
int    count_occurrences (string what, string in);
bool   occurs (string what, string in);
int    overlapping (string s1, string s2);
string replace (string s, string what, string by);
bool   match_wildcard (string s, string w);
array<string> tokenize (string s, string sep);
string recompose (array<string> a, string sep);
string trim_spaces_left (string s);
tree   trim_spaces_left (tree t);
array<string> trim_spaces_left (array<string> a);
string trim_spaces_right (string s);
tree   trim_spaces_right (tree t);
array<string> trim_spaces_right (array<string> a);
string trim_spaces (string s);
tree   trim_spaces (tree t);
array<string> trim_spaces (array<string> a);

path find_left_bracket (path p, const string& lbr, const string& rbr);
path find_right_bracket (path p, const string& lbr, const string& rbr);

array<string> as_completions (hashset<string> h);
array<string> close_completions (array<string> a);
array<string> strip_completions (array<string> a, string prefix);

array<int> differences (string s1, string s2);
int distance (string s1, string s2);

void parse_length (string s, double& len, string& unit);

#endif // defined ANALYZE_H
