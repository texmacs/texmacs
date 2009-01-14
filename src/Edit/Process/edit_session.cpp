
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
* Other useful subroutines
******************************************************************************/

static bool
is_var_empty (tree t) {
  if (is_atomic (t)) return (t == "");
  if (is_compound (t, "math", 1)) return is_var_empty (t[0]);
  if (is_document (t) || is_concat (t)) {
    int i;
    for (i=0; i<N(t); i++)
      if (!is_var_empty (t[i])) return false;
    return true;
  }
  return false;
}

/******************************************************************************
* Routines for sessions
******************************************************************************/

void
edit_process_rep::make_session (string lan, string session) {
  /* add plug-in style package if provided */
  string lolan= locase_all (lan);
  if ((lolan != lan) && (!connection_declared (lan))) lan= lolan;
  if (exists (url ("$TEXMACS_STYLE_PATH", lan * ".ts")))
    init_add_package (lan);

  /* insert session tag and output tag for start-up banner */
  path p (4, path (0, path (0, 0)));
  tree body (DOCUMENT, "");
  tree w= tree (WITH);
  w << PROG_LANGUAGE << lan << PROG_SESSION << session
    << compound ("session", body);
  insert_tree (w, p);
  insert_tree (compound ("output", tree (DOCUMENT, "")), path (0, 0, 0));
  if (lan == "scheme") {
    path op= search_upwards ("output");
    start_input (lan, session, op);
    return;
  }

  /* start asynchronous connection */
  (void) eval ("(use-modules (utils plugins plugin-cmd))");
  string handle= as_string (call ("plugin-async-start", lan, session));
  if (starts (handle, "error:")) {
    path op= search_upwards ("output");
    if (!is_nil (op)) start_input (lan, session, op);
    set_message (handle (7, N(handle)), "connect#" * lan);
  }
  else {
    string cmd= "(mutate-plugin-result \"" * handle * "\")";
    insert_mutator (tree (DOCUMENT, ""), cmd);
  }
}

void
edit_process_rep::start_input (string lan, string session, path p) {
  /* consistency checks and remove empty output */
  bool cursor_inside= (p <= tp);
  tree st= subtree (et, p);
  if (!is_compound (st, "output")) return;
  if (!is_document (subtree (et, path_up (p)))) return;
  if ((N(st) == 1) && is_var_empty (st [0])) remove (p, 1);
  else p= path_inc (p);

  /* get prompt and default input */
  tree   prompt = "";
  tree   input  = "";
  if (connection_declared (lan)) {
    prompt = connection_read (lan, session, "prompt");
    input  = connection_read (lan, session, "input");
  }
  while (is_document (prompt))
    if (N(prompt) == 0) prompt= "";
    else prompt= prompt[0];
  if (!last_prompt->contains (tuple (lan, session)))
    last_prompt (tuple (lan, session))= lan * "] ";
  if (prompt == "") prompt= copy (last_prompt [tuple (lan, session)]);
  last_prompt (tuple (lan, session))= prompt;
  if (!is_document (input)) input= tree (DOCUMENT, input);
  if (math_input) input= compound ("math", input);

  /* skip textput tags, remove old input tag and insert new input tag */
  while ((last_item (p) < N (subtree (et, path_up (p)))) &&
	 is_compound (subtree (et, p), "textput", 1))
    p= path_inc (p);
  if ((last_item (p) < N (subtree (et, path_up (p)))) &&
      is_compound (subtree (et, p), "input", 2)) {
    if (is_var_empty (input)) {
      input= copy (subtree (et, p)[1]);
      math_input= is_compound (input, "math", 1);
    }
    remove (p, 1);
  }
  insert (p, tree (DOCUMENT, compound ("input", prompt, input)));
  if (cursor_inside)
    go_to_correct (p * path (1, end (input)));

  /* footer messages */
  if (message_l != "") {
    set_message (message_l, message_r);
    message_l= "";
    message_r= "";
  }
  else set_message ("", "");
}

void
edit_process_rep::process_input () {
  path p= search_upwards ("input");
  if (is_nil (p) || (N (subtree (et, p)) != 2)) return;
  tree t= subtree (et, p) [1];
  string lan= get_env_string (PROG_LANGUAGE);

  if (lan == "scheme") {
    start_output ();
    tree u= sv->evaluate ("scheme", "default", t);
    if (!is_document (u)) u= tree (DOCUMENT, u);
    insert_tree (u);
    path op= search_upwards ("output");
    if (!is_nil (op)) start_input ("scheme", "default", op);
  }
  else if (connection_declared (lan)) {
    start_output ();
    (void) eval ("(use-modules (utils plugins plugin-cmd))");
    string session= get_env_string (PROG_SESSION);
    string handle= as_string (call ("plugin-async-feed", lan, session, t));
    if (starts (handle, "error:")) {
      path op= search_upwards ("output");
      if (!is_nil (op)) start_input (lan, session, op);    
      set_message (handle (7, N(handle)), "connect#" * lan);
    }
    else {
      string cmd= "(mutate-plugin-result \"" * handle * "\")";
      insert_mutator (tree (DOCUMENT, ""), cmd);
    }
  }
  else {
    set_message ("Package#'" * lan * "'#not declared",
		 "Evaluate#'" * lan * "'#expression");
  }
}

void
edit_process_rep::start_output () {
  path p;
  bool needs_return= false;
  if (!is_nil (p= search_upwards ("input"))) {
    go_to (p * 1);
    needs_return= true;
  }
  else if (!is_nil (p= search_upwards ("output"))) {
    go_to (p * 1);
    needs_return= true;
  }

  path q = path_up (tp, 2);
  int  i = last_item (path_up (tp));
  tree st= subtree (et, q);
  if (is_document (st) &&
      (i+1 < N(st)) &&
      is_compound (st[i+1], "output", 1))
    {
      assign (q * (i+1), compound ("output", tree (DOCUMENT, "")));
      go_to (q * path (i+1, 0, 0, 0));
    }
  else {
    if (needs_return) insert_return ();
    insert_tree (compound ("output", tree (DOCUMENT, "")),
		 path (0, path (0, 0)));
  }
}

void
edit_process_rep::session_message (string l, string r) {
  message_l= l;
  message_r= r;
}

void
edit_process_rep::session_use_math_input (bool flag) {
  if (math_input != flag) {
    math_input= flag;
    path p= search_upwards ("input");
    if (is_nil (p)) return;
    tree input (DOCUMENT, "");
    path q (0, 0);
    if (math_input) {
      input= compound ("math", input);
      q= path (0, q);
    }
    assign (p * 1, input);
    go_to (p * path (1, q));
  }
}

bool
edit_process_rep::session_is_using_math_input () {
  return math_input;
}

int
edit_process_rep::status_connection () {
  string lan    = get_env_string (PROG_LANGUAGE);
  string session= get_env_string (PROG_SESSION);
  return connection_status (lan, session);
}

bool
edit_process_rep::busy_connection () {
  return status_connection () == WAITING_FOR_OUTPUT;
}

void
edit_process_rep::interrupt_connection () {
  string lan    = get_env_string (PROG_LANGUAGE);
  string session= get_env_string (PROG_SESSION);
  if (connection_status (lan, session) == WAITING_FOR_OUTPUT)
    connection_interrupt (lan, session);
}

void
edit_process_rep::stop_connection () {
  string lan    = get_env_string (PROG_LANGUAGE);
  string session= get_env_string (PROG_SESSION);
  if (connection_status (lan, session) != CONNECTION_DEAD)
    connection_stop (lan, session);
}

/******************************************************************************
* Cursor movement inside sessions
******************************************************************************/

void
edit_process_rep::session_var_go_up () {
  path p= search_upwards ("input");
  if (is_nil (p)) return;
  path q= search_previous_compound (p, "input");
  if (q != p) {
    tree st= subtree (et, q);
    go_to_correct (q * path (1, end (st[1])));
    select_from_cursor_if_active ();
  }
}

void
edit_process_rep::session_var_go_down () {
  path p= search_upwards ("input");
  if (is_nil (p)) return;
  path q= search_next_compound (p, "input");
  if (q != p) {
    tree st= subtree (et, q);
    go_to_correct (q * path (1, end (st[1])));
    select_from_cursor_if_active ();
  }
}

void
edit_process_rep::session_go_up () {
  path p= search_upwards ("input");
  if (is_nil (p)) return;
  int i= tp[N(p)];
  path old_tp= tp;
  go_up ();
  p= search_upwards ("input");
  if (is_nil (p) || ((tp[N(p)] != 1) && (tp[N(p)] != i))) {
    go_to (old_tp);
    session_var_go_up ();
  }
  select_from_cursor_if_active ();
}

void
edit_process_rep::session_go_down () {
  path p= search_upwards ("input");
  if (is_nil (p)) return;
  int i= tp[N(p)];
  path old_tp= tp;
  go_down ();
  p= search_upwards ("input");
  if (is_nil (p) || ((tp[N(p)] != 1) && (tp[N(p)] != i))) {
    go_to (old_tp);
    session_var_go_down ();
  }
  select_from_cursor_if_active ();
}

void
edit_process_rep::session_go_page_up () {
  int i;
  for (i=0; i<5; i++)
    session_var_go_up ();
}

void
edit_process_rep::session_go_page_down () {
  int i;
  for (i=0; i<5; i++)
    session_var_go_down ();
}

void
edit_process_rep::session_remove (bool forward) {
  path p= search_upwards ("math");
  if (is_nil (p)) {
    p= search_upwards ("input");
    if (is_nil (p) || (tp == (forward? end (et, p * 1): start (et, p * 1))))
      return;
  }
  else if (tp == (forward? end (et, p * 0): start (et, p * 0))) return;
  remove_text (forward);
}

/******************************************************************************
* Utility operations on fields in sessions
******************************************************************************/

static void
skip_forwards (tree et, path& p, string tag, int arity) {
  if (last_item (p) < N (subtree (et, path_up (p))))
    if (is_compound (subtree (et, p), tag, arity))
      p= path_inc (p);
}

static void
skip_backwards (tree et, path& p, string tag, int arity) {
  if (last_item (p) > 0)
    if (is_compound (subtree (et, path_dec (p)), tag, arity))
      p= path_dec (p);
}

void
edit_process_rep::session_insert_text_field () {
  path p= search_upwards ("input");
  if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
  insert (p, tree (DOCUMENT, compound ("textput", tree (DOCUMENT, ""))));
  go_to (p * path (0, path (0, 0)));
}

void
edit_process_rep::session_insert_input_at (path p) {
  string lan    = get_env_string (PROG_LANGUAGE);
  string session= get_env_string (PROG_SESSION);
  tree prompt= copy (last_prompt [tuple (lan, session)]);
  tree input = tree (DOCUMENT, "");
  if (math_input) input= compound ("math", input);
  insert (p, tree (DOCUMENT, compound ("input", prompt, input)));
  go_to_correct (p * path (1, end (input)));
}

void
edit_process_rep::session_insert_input_below () {
  path p= search_upwards ("input");
  if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
  p= path_inc (p);
  skip_forwards (et, p, "output", 1);
  session_insert_input_at (p);
}

void
edit_process_rep::session_insert_input_above () {
  path p= search_upwards ("input");
  if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
  skip_backwards (et, p, "textput", 1);
  session_insert_input_at (p);
}

void
edit_process_rep::session_fold_input () {
  path p= search_upwards ("input");
  if (is_nil (p)) return;
  path q= path_inc (p);
  skip_backwards (et, p, "textput", 1);
  skip_forwards (et, q, "output", 1);
  tree del= copy (subtree (et, path_up (p)) (last_item (p), last_item (q)));
  tree ins= compound ("unfolded", tree (DOCUMENT, ""), del);
  remove (p, last_item (q) - last_item (p));
  insert (p, tree (DOCUMENT, ins));
  go_to (p * path (0, path (0, 0)));
}

void
edit_process_rep::session_remove_input (bool forward) {
  if (forward) {
    path p= search_upwards ("input");
    if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
    path q= path_inc (p);
    skip_backwards (et, p, "textput", 1);
    skip_forwards (et, q, "output", 1);
    path r= q;
    skip_forwards (et, r, "textput", 1);
    if (last_item (r) >= N (subtree (et, path_up (r)))) return;
    if (!is_compound (subtree (et, r), "input", 2)) return;
    go_to_correct (r * path (1, end (subtree (et, r * 1))));
    remove (p, last_item (q) - last_item (p));
  }
  else {
    path p= search_upwards ("input");
    if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
    skip_backwards (et, p, "textput", 1);
    path q= p;
    skip_backwards (et, q, "output", 1);
    skip_backwards (et, q, "input", 2);
    skip_backwards (et, q, "textput", 1);
    if (q != p) remove (q, last_item (p) - last_item (q));
  }
}

void
edit_process_rep::session_remove_all_outputs () {
  path p= search_upwards ("input");
  if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
  tree st= subtree (et, path_up (p));
  int i, n= N (st);
  for (i=n-1; i>=0; i--)
    if (is_compound (st[i], "output", 1))
      remove (path_up (p) * i, 1);
}

void
edit_process_rep::session_remove_previous_output () {
  path p= search_upwards ("output");
  if (is_nil (p) || (!is_document (subtree (et, path_up (p))))) return;
  path q= p;
  skip_backwards (et, p, "input", 2);
  skip_backwards (et, p, "textput", 1);
  skip_backwards (et, p, "output", 1);
  if ((p == q) || (!is_compound (subtree (et, p), "output", 1))) return;
  remove (p, 1);
}

void
edit_process_rep::session_split () {
  if (!inside ("input")) return;
  path p= search_upwards ("input");
  skip_backwards (et, p, "textput", 1);
  path q= search_upwards ("session");
  if (is_nil (p) ||
      (!(rp < q)) ||
      (N(q - rp) < 2) ||
      (last_item (p) == 0) ||
      (!is_document (subtree (et, path_up (p)))) ||
      (!is_document (subtree (et, path_up (q, 2)))) ||
      (!is_func (subtree (et, path_up (q)), WITH, 5)) ||
      (path_up (p, 2) != q) ||
      (last_item (q) != 4))
    return;
  tree st = subtree (et, path_up (p));
  tree del= st (last_item (p), N(st));
  tree w  = copy (subtree (et, path_up (q)));
  w[4]= compound ("session", copy (del));
  insert (path_inc (path_up (q)), tree (DOCUMENT, "", w));
  go_to (path_inc (path_up (q)) * 0);
  remove (p, N(del));
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
