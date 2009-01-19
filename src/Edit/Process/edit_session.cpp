
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

edit_process_rep::edit_process_rep ():
  new_mutators (false), mutators_updated (false),
  nr_mutators (0), next_mutate (texmacs_time()), mutator_time (next_mutate),
  math_input (false), message_l (""), message_r ("") {}
edit_process_rep::~edit_process_rep () {}

/******************************************************************************
* Mutators
******************************************************************************/

#define MUTATE_FAST_INTERACTION 100
#define MUTATE_SLOW_INTERACTION 500

static path mutator_path;
static string MUTATOR ("mutator");

static int
mutate (tree t, path ip) {
  if (is_atomic (t)) return 0;
  int i, sum=0;
  if (is_compound (t, MUTATOR, 2) &&
      (ip == obtain_ip (t)))
    {
      mutator_path= reverse (path (0, ip));
      string s= as_string (t[1]); // eval_secure (s);
      if (s != "")
	if (as_bool (eval ("(secure? '" * s * ")"))) {
	  (void) eval (s);
	}
      sum= 1;
      mutator_path= path ();
    }
  for (i=0; i<N(t); i++)
    sum += mutate (t[i], path (i, ip));
  return sum;
}

void
edit_process_rep::process_mutators () {
  if (mutators_updated && (nr_mutators == 0)) return;
  if (texmacs_time()-next_mutate < 0) return;
  new_mutators= false;
  mutators_updated= true;
  mutator_time= texmacs_time ();
  nr_mutators= mutate (subtree (et, rp), reverse (rp));
  if (!mutators_updated) {
    // cout << "Mutation occurred\n";
    next_mutate= texmacs_time () + MUTATE_FAST_INTERACTION;
  }
  else {
    // cout << "No mutations occurred\n";
    next_mutate= texmacs_time () + MUTATE_SLOW_INTERACTION;
  }
}

path
edit_process_rep::get_mutator_path () {
  return mutator_path;
}

time_t
edit_process_rep::get_mutator_time () {
  return mutator_time;
}

void
edit_process_rep::invalidate_mutators () {
  mutators_updated= false;
  if (new_mutators) next_mutate= texmacs_time ()+ MUTATE_FAST_INTERACTION;
  else next_mutate= texmacs_time ()+ MUTATE_SLOW_INTERACTION;
}

void
edit_process_rep::insert_mutator (tree body, string cmd) {
  new_mutators= true;
  insert_tree (compound ("mutator", body, cmd), path (0, end (body)));
}

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
