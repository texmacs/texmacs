
/******************************************************************************
* MODULE     : preprocessor_parser.cpp
* DESCRIPTION: shared preprocessor parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "preprocessor_parser.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

preprocessor_parser_rep::preprocessor_parser_rep() {
  m_start = '#';
}

void
preprocessor_parser_rep::set_start (string start) {
  if (N(start) == 1) {
    m_start= start[0];
  }
}

void
preprocessor_parser_rep::set_directives (array<string> directives) {
  for (int i=0; i<N(directives); i++) {
    m_directives << directives[i];
  }
}

bool
preprocessor_parser_rep::can_parse (string s, int pos) {
  if (!parser_rep::can_parse (s, pos)) return false;
  
  if (s[pos] != '#') return false;
  
  int first_non_blank_pos= 0;
  skip_spaces (s, first_non_blank_pos);
  return first_non_blank_pos == pos;
}

void
preprocessor_parser_rep::do_parse (string s, int& pos) {
  // A language with empty directives does not have preprocessors
  if (N(m_directives) == 0) return;
  
  int opos= pos;
  
  if (s[pos] != m_start) return;
  pos= pos+1;
  
  string word;
  if (!read_word (s, pos, word)) return;
  bool hit_directives= m_directives->contains (word);
  
  if (hit_directives && pos < N(s) && s[pos] == ' ') {
    // "The next char of the directive may be ' '
  } else if (hit_directives && pos >= N(s)) {
    // "The next char of the directive may be '\n'
  } else {
    // For invalid pre-processors, reset to the original position
    // debug_packrat << get_parser_name() << ": "
    //   << raw_quote (word) << " is not a valid directive" << LF;
    pos= opos;
  }
}

string
preprocessor_parser_rep::to_string () {
  string ret;
  ret << parser_rep::to_string ();
  ret << "  " << "start" << ": " << m_start << "\n"
      << "  " << "directives:" << "\n";

  iterator<string> iter= iterate (m_directives);
  while (iter->busy ()) {
    string directive= iter->next ();
    ret << "    - " << directive << "\n";
  }
  
  return ret;
}
