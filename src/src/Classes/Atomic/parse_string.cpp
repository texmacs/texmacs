
/******************************************************************************
* MODULE     : parse_string.cpp
* DESCRIPTION: strings from which it is both easy to read and write characters
*              they are used for entity replacement in the XML parser
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "parse_string.hpp"
#include "analyze.hpp"

void
parse_string_rep::advance (int n) {
  if (nil (l) || n <= 0) return;
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
  while (!nil (l) && p->item + n > N (l->item)) {
    s << l->item (p->item, N (l->item));
    n -= (N (l->item) - p->item);
    l  = l->next;
    p  = p->next;
  }
  if (nil (l)) return s;
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
  if (nil (l)) return 0;
  if (p->item + n < N (l->item))
    return l->item [p->item + n];

  list<string> ll= l;
  list<int>    pp= p;
  while (!nil (l) && pp->item + n >= N (ll->item)) {
    n -= (N (ll->item) - pp->item);
    ll = ll->next;
    pp = pp->next;
  }
  if (nil (ll)) return 0;
  return ll->item [pp->item + n];
}

string
parse_string_rep::get_string (int n) {
  if (nil (l)) return "";
  if (p->item + n <= N (l->item))
    return l->item (p->item, p->item + n);

  string s;
  list<string> ll= l;
  list<int>    pp= p;
  while (n >= 0 && !nil (ll)) {
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
  if (nil (l)) return N(s) == 0;
  if (p->item + N(s) <= N (l->item))
    return ::test (l->item, p->item, s);

  return get_string (N(s)) == s;
}

bool
test (parse_string s, string what) {
  return s->test (what);
}

ostream&
operator << (ostream& out, parse_string s) {
  list<string> l= s->l;
  list<int>    p= s->p;
  while (!nil (l)) {
    out << l->item (p->item, N(l->item));
    l= l->next;
    p= p->next;
  }
  return out;
}
