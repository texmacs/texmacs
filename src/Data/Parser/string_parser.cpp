
/******************************************************************************
* MODULE     : string_parser.cpp
* DESCRIPTION: shared string parsing routines for various programming languages
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string_parser.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

string_parser_rep::string_parser_rep () {
  use_esc_parser= false;
  reset ();
}

void string_parser_rep::reset () {
  do_finish ();
  m_escaped= false;
  m_skip_escaped= false;
}

bool
string_parser_rep::can_parse (string s, int pos) {
  if (parser_rep::can_parse (s, pos)) {
    if (unfinished ()) return true;

    iterator<string> iter= iterate (m_pairs);
    while (iter->busy ()) {
      m_start= iter->next ();
      if (test (s, pos, m_start)) {
        m_start_size= N(m_start);
        return true;
      }
    }
  }
  return false;
}

void
string_parser_rep::do_parse (string s, int& pos) {
  m_escaped= false;

  if (m_start_size <= 0) {
    debug_packrat << "m_start is empty unexpectedly with "
                  << pos << ":" << s << LF;
    return;
  }

  if (!unfinished ()) {
    // Always use the longer matched m_start
    iterator<string> iter= iterate (m_pairs);
    while (iter->busy ()) {
      string key= iter->next ();
      if (m_start_size >= N(key)) continue;
      if (starts (key, m_start) && test (s, pos, key)) {
        m_start= key;
        m_start_size= N(key);
      }
    }

    // Advance over the opening
    pos+= m_start_size;
  }

  // Advance until the end or the escaped sequence
  string closing= m_pairs (m_start);
  while (pos<N(s) && !test (s, pos, closing)) {
    if (use_esc_parser && m_esc_parser.can_parse (s, pos)) {
      if (m_skip_escaped) {
        m_esc_parser.parse (s, pos);
      } else {
        m_escaped= true;
        m_finished= false;
        // Exit since we encountered a escaped sequence
        return ;
      }
    }

    pos++;
  }

  // Advance over the closing
  if (test (s, pos, closing)) {
    pos+= N(closing);
    do_finish ();
  } 
}

bool string_parser_rep::unfinished () {
  return !m_finished;
}

void string_parser_rep::do_finish () {
  m_start_size= 0;
  m_finished= true;
}

void string_parser_rep::set_pairs (hashmap<string, string> p_pairs) {
  m_pairs= p_pairs;
}

bool string_parser_rep::escaped () {
  return m_escaped;
}

void string_parser_rep::set_escaped_char_parser (escaped_char_parser_rep p_esc_parser) {
  m_esc_parser= p_esc_parser;
  use_esc_parser= true;
}

bool string_parser_rep::parse_escaped (string s, int& pos) {
  m_escaped= false;
  if (m_esc_parser.parse (s, pos)) {
    return true;
  }
  return false;
}

void string_parser_rep::skip_escaped (bool skip) {
  m_skip_escaped= skip;
}
