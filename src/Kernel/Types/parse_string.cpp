
/******************************************************************************
* MODULE     : parse_string.cpp
* DESCRIPTION: strings from which it is both easy to read and write characters
*              they are used for entity replacement in the XML parser
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "parse_string.hpp"
#include "analyze.hpp"

void
parse_string_rep::advance (int n) {
  if (is_nil (l) || n <= 0) return;
  p->item += n;
  if (p->item >= N (l->item)) {
    n= p->item - N (l->item);
    l= l->next;
    p= p->next;
    advance (n);
  }
}

string
parse_string_rep::read (int n) {
  string s;
  while (!is_nil (l) && p->item + n > N (l->item)) {
    s << l->item (p->item, N (l->item));
    n -= (N (l->item) - p->item);
    l  = l->next;
    p  = p->next;
  }
  if (is_nil (l)) return s;
  s << l->item (p->item, p->item + n);
  p->item += n;
  if (p->item >= N(l->item)) {
    l= l->next;
    p= p->next;
  }
  return s;
}

void
parse_string_rep::write (string s) {
  if (N(s) > 0) {
    l= list<string> (s, l);
    p= list<int>    (0, p);
  }
}

char
parse_string_rep::get_char (int n) {
  if (is_nil (l)) return 0;
  if (p->item + n < N (l->item))
    return l->item [p->item + n];

  list<string> ll= l;
  list<int>    pp= p;
  while (!is_nil (l) && pp->item + n >= N (ll->item)) {
    n -= (N (ll->item) - pp->item);
    ll = ll->next;
    pp = pp->next;
  }
  if (is_nil (ll)) return 0;
  return ll->item [pp->item + n];
}

string
parse_string_rep::get_string (int n) {
  if (is_nil (l)) return "";
  if (p->item + n <= N (l->item))
    return l->item (p->item, p->item + n);

  string s;
  list<string> ll= l;
  list<int>    pp= p;
  while (n >= 0 && !is_nil (ll)) {
    int m= min (N (ll->item) - pp->item, n);
    s << ll->item (pp->item, pp->item + m);
    n -= m;
    ll = ll->next;
    pp = pp->next;
  }
  return s;
}

bool
parse_string_rep::test (string s) {
  if (is_nil (l)) return N(s) == 0;
  if (p->item + N(s) <= N (l->item))
    return ::test (l->item, p->item, s);

  return get_string (N(s)) == s;
}

bool
test (parse_string s, string what) {
  return s->test (what);
}

tm_ostream&
operator << (tm_ostream& out, parse_string s) {
  list<string> l= s->l;
  list<int>    p= s->p;
  while (!is_nil (l)) {
    out << l->item (p->item, N(l->item));
    l= l->next;
    p= p->next;
  }
  return out;
}
