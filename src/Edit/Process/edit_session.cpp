
/******************************************************************************
* MODULE     : edit_session.cpp
* DESCRIPTION: input and output handling for programming sessions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Process/edit_process.hpp"
#include "list.hpp"
#include "connect.hpp"
#include "convert.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "timer.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_process_rep::edit_process_rep () {}
edit_process_rep::~edit_process_rep () {}

/******************************************************************************
* Tab completion
******************************************************************************/

static string cursor_symbol ("[tmcursor]");

static tree
put_cursor (tree t, path p) {
  if (is_atomic (t)) {
    string s= t->label;
    return s (0, p->item) * cursor_symbol * s (p->item, N(s));
  }
  else {
    if (p == path (0)) return tree (CONCAT, cursor_symbol, t);
    else if (p == path (1)) return tree (CONCAT, t, cursor_symbol);
    else {
      int i, n= N(t);
      tree u (t, n);
      for (i=0; i<n; i++)
	if (i == p->item) u[i]= put_cursor (t[i], p->next);
	else u[i]= t[i];
      return u;
    }
  }
}

bool
edit_process_rep::session_complete_try (tree tt) {
  path p= reverse (obtain_ip (tt));
  tree st= subtree (et, p);
  if ((N(tp) <= N(p)) || (tp[N(p)] != 1)) return false;
  tree t= put_cursor (st[1], tail (tp, N(p)+1));
  // cout << t << LF;

  (void) eval ("(use-modules (utils plugins plugin-cmd))");
  string lan= get_env_string (PROG_LANGUAGE);
  string ses= get_env_string (PROG_SESSION);
  string s  = as_string (call ("verbatim-serialize", lan, tree_to_stree (t)));
  s= s (0, N(s)-1);

  int pos= search_forwards (cursor_symbol, s);
  if (pos == -1) return false;
  s= s (0, pos) * s (pos + N(cursor_symbol), N(s));
  // cout << s << ", " << pos << LF;

  string cmd= "(complete " * scm_quote (s) * " " * as_string (pos) * ")";
  tree r= connection_cmd (lan, ses, cmd);

  if (!is_tuple (r)) return false;
  int i, n= N(r);
  string prefix;
  array<string> compls;
  for (i=0; i<n; i++)
    if (is_atomic (r[i])) {
      string l= r[i]->label;
      if (is_quoted (l)) l= scm_unquote (l);
      if (prefix == "") prefix= l;
      else compls << l;
    }
  // cout << prefix << ", " << compls << LF;

  if ((prefix == "") || (N(compls) == 0)) return false;
  complete_start (prefix, compls);
  return true;
}
