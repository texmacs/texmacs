
/******************************************************************************
* MODULE     : impl_language.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "impl_language.hpp"
#include "path.hpp"

extern tree the_et;

/*
 static bool
 is_line (tree t) {
 path p= obtain_ip (t);
 if (is_nil (p) || last_item (p) < 0) return false;
 tree pt= subtree (the_et, reverse (p->next));
 if (!is_func (pt, DOCUMENT)) return false;
 return true;
 }
 */

int
line_number (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return p->item;
}

int
number_of_lines (tree t) {
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return -1;
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return -1;
  return N(pt);
}

tree
line_inc (tree t, int i) {
  if (i == 0) return t;
  path p= obtain_ip (t);
  if (is_nil (p) || last_item (p) < 0) return tree (ERROR);
  tree pt= subtree (the_et, reverse (p->next));
  if (!is_func (pt, DOCUMENT)) return tree (ERROR);
  if ((p->item + i < 0) || (p->item + i >= N(pt))) return tree (ERROR);
  return pt[p->item + i];
}

static void
parse_comment_multi_lines (string s, int& pos) {
  if (pos+1 < N(s) && s[pos] == '/' && s[pos+1] == '*')
    pos += 2;
}

static bool
parse_string (string s, int& pos, bool force) {
  int n= N(s);
  static string delim;
  if (pos >= n) return false;
  if (s[pos] == '\"' || s[pos] == '\'') {
    delim= s(pos, pos+1);
    pos+= N(delim);
  }
  else if (!force)
    return false;
  while (pos<n && !test (s, pos, delim)) {
    if (s[pos] == '\\') {
      return true;
    }
    else
      pos++;
  }
  if (test (s, pos, delim))
    pos+= N(delim);
  return false;
}

static bool
begin_comment (string s, int i) {
  bool comment= false;
  int opos, pos= 0;
  do {
    do {
      opos= pos;
      parse_string (s, pos, false);
      if (opos < pos) break;
      parse_comment_multi_lines (s, pos);
      if (opos < pos) {
        comment = true;
        break;
      }
      pos++;
    } while (false);
  } while (pos <= i);
  return comment;
}

static int
after_begin_comment (int i, tree t) {
  tree   t2= t;
  string s2= t->label;
  int  line= line_number (t2);
  do {
    if (begin_comment (s2, i)) return line;
    t2= line_inc (t2, -1);
    --line;
      // line_inc returns tree(ERROR) upon error
    if (!is_atomic (t2)) return -1; // FIXME
    s2= t2->label;
    i = N(s2) - 1;
  } while (line > -1);
  return -1;
}

static void
parse_end_comment (string s, int& pos) {
  if (pos+1 < N(s) && s[pos] == '*' && s[pos+1] == '/')
    pos += 2;
}

static bool
end_comment (string s, int i) {
  int opos, pos= 0;
  do {
    do {
      opos= pos;
      parse_string (s, pos, false);
      if (opos < pos) break;
      parse_end_comment (s, pos);
      if (opos < pos && pos>i) return true;
      pos++;
    } while (false);
  } while (pos < N(s));
  return false;
}

static int
before_end_comment (int i, tree t) {
  int   end= number_of_lines (t);
  tree   t2= t;
  string s2= t2->label;
  int  line= line_number (t2);
  do {
    if (end_comment (s2, i)) return line;
    t2= line_inc (t2, 1);
    ++line;
      // line_inc returns tree(ERROR) upon error
    if (!is_atomic (t2)) return -1; // FIXME
    s2= t2->label;
    i = 0;
  } while (line <= end);
  return -1;
}

bool
in_comment (int pos, tree t) {
  int beg= after_begin_comment (pos, t);
  if (beg >= 0) {
    int cur= line_number (t);
    int end= before_end_comment (pos, line_inc (t, beg - cur));
    return end >= beg && cur <= end;
  }
  return false;
}

bool
abstract_language_rep::belongs_to_identifier(char c) {
  return (is_digit (c) || is_alpha (c) || (c=='_'));
}

void
abstract_language_rep::parse_identifier (hashmap<string, string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (!t->contains (s (pos, i))) pos= i;
}

void
abstract_language_rep::parse_keyword (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}

void
abstract_language_rep::parse_type (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}

void
abstract_language_rep::parse_constant (hashmap<string,string>& t, string s, int& pos) {
  int i= pos;
  if (pos >= N(s) || is_digit (s[i])) return;
  while (i < N(s) && belongs_to_identifier (s[i])) i++;
  if (t->contains (s (pos, i)))
    pos= i;
}
