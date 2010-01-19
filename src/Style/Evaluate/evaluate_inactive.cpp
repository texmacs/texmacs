
/******************************************************************************
* MODULE     : evaluate_inactive.cpp
* DESCRIPTION: generate source code representations for inactive trees
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "memorizer.hpp"
#include "std_environment.hpp"
#include "vars.hpp"

/******************************************************************************
* Some trees need to be defined only once (ensuring a fixed address)
******************************************************************************/

static tree psep_var ("par-par-sep");
static tree psep_0fn ("0fn");
static tree mode_var ("mode");
static tree mode_src ("src");
static tree surround1 ("");
static tree surround2 (VSPACE, "0.5fn");

/******************************************************************************
* Forward definitions
******************************************************************************/

bool is_long_arg (tree t, int i);
bool is_long (tree t);
string arg_type (tree t, int i);

tree
highlight (tree t, string kind) {
  if (is_compound (t))        return t;
  else if (kind == "")        return t;
  else if (kind == "macro")   return compound ("src-macro", t);
  else if (kind == "var")     return compound ("src-var", t);
  else if (kind == "arg")     return compound ("src-arg", t);
  else if (kind == "tt")      return compound ("src-tt", t);
  else if (kind == "integer") return compound ("src-integer", t);
  else if (kind == "length")  return compound ("src-length", t);
  else if (kind == "error")   return compound ("src-error", t);
  return t;
}

class inactive_style;
tree rewrite_inactive (tree t, inactive_style sty);
tree rewrite_inactive_default (tree t, inactive_style sty);

/******************************************************************************
* Values for inactive style parameters
******************************************************************************/

#define STYLE_ANGULAR         0
#define STYLE_SCHEME          1
#define STYLE_LATEX           2
#define STYLE_FUNCTIONAL      3

#define SPECIAL_RAW           0
#define SPECIAL_FORMAT        1
#define SPECIAL_NORMAL        2
#define SPECIAL_MAXIMAL       3

#define COMPACT_ALL           0
#define COMPACT_INLINE_ARGS   1
#define COMPACT_INLINE_START  2
#define COMPACT_INLINE        3
#define COMPACT_NONE          4

#define CLOSE_MINIMAL         0
#define CLOSE_COMPACT         1
#define CLOSE_LONG            2
#define CLOSE_REPEAT          3

/******************************************************************************
* Inactive style parameters
******************************************************************************/

struct inactive_style_rep {
  unsigned style   : 4;
  unsigned special : 4;
  unsigned compact : 4;
  unsigned close   : 4;
  unsigned mode    : 4;
  unsigned block   : 1;
  unsigned flush   : 1;
  unsigned recover : 1;
};

class inactive_style {
  int rep;
public:
  inline inactive_style (): rep (0) {}
  inline inactive_style (const inactive_style& sty): rep (sty.rep) {}
  inline inactive_style& operator = (const inactive_style& sty) {
    rep= sty.rep; return *this; }
  inline inactive_style_rep* operator -> () {
    return (inactive_style_rep*) ((void*) &rep); }
  inline bool operator == (inactive_style sty) { return rep == sty.rep; }
  inline bool operator != (inactive_style sty) { return rep != sty.rep; }
	inline friend int hash (inactive_style sty);
};

inline inactive_style set_bf (inactive_style sty, bool block, bool flush) {
  inactive_style new_sty= sty;
  new_sty->block= block;
  new_sty->flush= flush;
  return new_sty; }

inline inactive_style reset_bf (inactive_style sty) {
  inactive_style new_sty= sty;
  new_sty->block= 0;
  new_sty->flush= 0;
  return new_sty; }

inline int hash (inactive_style sty) {
  return sty.rep; }

inactive_style
retrieve (environment env) {
  string s;
  inactive_style sty;

  s= as_string (env [SRC_STYLE]);
  if (s == "angular") sty->style= STYLE_ANGULAR;
  else if (s == "scheme") sty->style= STYLE_SCHEME;
  else if (s == "latex") sty->style= STYLE_LATEX;
  else if (s == "functional") sty->style= STYLE_FUNCTIONAL;
  else sty->style= STYLE_ANGULAR;
  
  s= as_string (env [SRC_SPECIAL]);
  if (s == "raw") sty->special= SPECIAL_RAW;
  else if (s == "format") sty->special= SPECIAL_FORMAT;
  else if (s == "normal") sty->special= SPECIAL_NORMAL;
  else if (s == "maximal") sty->special= SPECIAL_MAXIMAL;
  else sty->special= SPECIAL_NORMAL;

  s= as_string (env [SRC_COMPACT]);
  if (s == "all") sty->compact= COMPACT_ALL;
  else if (s == "inline args") sty->compact= COMPACT_INLINE_ARGS;
  else if (s == "normal") sty->compact= COMPACT_INLINE_START;
  else if (s == "inline") sty->compact= COMPACT_INLINE;
  else if (s == "none") sty->compact= COMPACT_NONE;
  else sty->compact= COMPACT_INLINE_START;

  s= as_string (env [SRC_CLOSE]);
  if (s == "minimal") sty->close= CLOSE_MINIMAL;
  else if (s == "compact") sty->close= CLOSE_COMPACT;
  else if (s == "long") sty->close= CLOSE_LONG;
  else if (s == "repeat") sty->close= CLOSE_REPEAT;
  else sty->close= CLOSE_COMPACT;

  return sty;
}

/******************************************************************************
* Memorizing rewritings
******************************************************************************/

static tree no_tree (UNINIT);

class memorizer;
class inactive_memorizer_rep: public compound_memorizer_rep {
  environment env_in;
  tree t_in;
  inactive_style sty_in;
  environment env_out;
  tree t_out;

public:
  inline inactive_memorizer_rep (environment env, tree t, inactive_style sty):
    env_in (env), t_in (t), sty_in (sty), env_out (), t_out (no_tree) {}
  void print (tm_ostream& out) {
    out << "inactive_memorizer (" << t_in << ")"; }

  int type () { return MEMORIZE_INACTIVE; }
  int hash () {
    return weak_hash (env_in) ^ weak_hash (t_in) ^ ::hash (sty_in); }
  bool equal (memorizer_rep* mem) {
    inactive_memorizer_rep* rep= (inactive_memorizer_rep*) mem;
    return
      weak_equal (env_in, rep->env_in) &&
      weak_equal (t_in, rep->t_in) &&
      sty_in == rep->sty_in; }

  void set_environment (environment env) { env_out= env; }
  environment get_environment () { return env_out; }
  void set_tree (tree t) { t_out= t; }
  tree get_tree () { return t_out; }
};

inline memorizer
inactive_memorizer (environment env, tree t, inactive_style sty) {
  return (memorizer_rep*) tm_new<inactive_memorizer_rep> (env, t, sty);
}

/******************************************************************************
* Compute rendering of inactive markup
******************************************************************************/

tree
rewrite_inactive_arg (tree t, int i, inactive_style sty) {
  tree r= t[i];
  if ((sty->mode == INACTIVE_INLINE_RECURSE) ||
      (sty->mode == INACTIVE_BLOCK_RECURSE))
    {
      /*
      if (N (recover_env) > 0) {
	int j;
	tree recover= copy (recover_env), old_recover= recover_env;
	for (j=0; j<N(recover); j+=2) {
	  string var= recover[j]->label;
	  recover[j+1]= read (var);
	  write_update (var, recover_env[j+1]);
	}
	recover_env= tuple ();
	r= rewrite_inactive (r, sty);
	recover_env= old_recover;
	for (j=0; j<N(recover); j+=2)
	  write_update (recover[j]->label, recover[j+1]);
      }
      else
      */
	r= rewrite_inactive (r, sty);
    }
  return highlight (r, arg_type (t, i));
}

tree
rewrite_inactive_raw_data (tree t, inactive_style sty) {
  (void) t;
  return rewrite_inactive_default (tree (RAW_DATA), sty);
}

tree
rewrite_inactive_document (tree t, inactive_style sty) {
  if ((sty->block || (sty->compact == COMPACT_NONE)) &&
      (sty->special > SPECIAL_RAW) &&
      (sty->compact != COMPACT_ALL))
    {
      int i, n= N(t);
      tree r (DOCUMENT, n);
      for (i=0; i<n; i++) {
	inactive_style ss= set_bf (sty, true, sty->flush || (i<n-1));
	r[i]= rewrite_inactive_arg (t, i, ss);
      }
      return r;
    }
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_concat (tree t, inactive_style sty) {
  if ((sty->special > SPECIAL_RAW) && (sty->compact != COMPACT_NONE)) {
    int i, n= N(t);
    tree r (CONCAT, n);
    for (i=0; i<n; i++)
      r[i]= rewrite_inactive_arg (t, i, reset_bf (sty));
    return r;
  }
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_value (tree t, inactive_style sty) {
  if ((N(t) == 1) && is_atomic (t[0]) &&
      sty->style != STYLE_SCHEME && sty->special >= SPECIAL_NORMAL)
    return highlight (t[0],
		      sty->mode == INACTIVE_INLINE_ERROR ||
		      sty->mode == INACTIVE_BLOCK_ERROR ?
		      string ("error"): string ("var"));
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_arg (tree t, inactive_style sty) {
  if ((N(t) == 1) && is_atomic (t[0]) &&
      sty->style != STYLE_SCHEME && sty->special >= SPECIAL_NORMAL)
    return highlight (t[0],
		      sty->mode == INACTIVE_INLINE_ERROR ||
		      sty->mode == INACTIVE_BLOCK_ERROR ?
		      string ("error"): string ("arg"));
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_symbol (tree t, inactive_style sty) {
  if ((N(t) == 1) && is_atomic (t[0]) && (sty->special >= SPECIAL_NORMAL))
    return tree (INLINE_TAG, t[0]);
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_style_with (tree t, inactive_style sty, bool once) {
  (void) once;
  int /*i,*/ n= N(t);
  tree recover= tuple ();
  /*
  for (i=0; i<n-1; i+=2)
    if (is_atomic (t[i])) {
      recover << t[i] << read (t[i]->label);
      write_update (t[i]->label, t[i+1]);
    }
    if (once) recover_env= recover;
  */
  tree r= rewrite_inactive (t[n-1], sty);
  /*
  for (i=0; i<N(recover); i+=2)
    write_update (recover[i]->label, recover[i+1]);
  if (once) recover_env= tuple ();
  */
  return r;
}

tree
rewrite_inactive_active (tree t, inactive_style sty) {
  tree st= t[0];
  int i, n= N(st);
  tree r (st, n);
  bool mp= is_multi_paragraph (st);
  for (i=0; i<n; i++) {
    bool smp= mp && is_long_arg (st, i);
    if (is_func (st, WITH) && (i<n-1)) r[i]= st[i];
    else {
      inactive_style ss= set_bf (sty, sty->block && smp, sty->flush && smp);
      r[i]= rewrite_inactive_arg (st, i, ss);
    }
  }
  return r;
}

tree
rewrite_inactive_var_active (tree t, inactive_style sty) {
  tree r= tree (WITH, mode_var, std_env [MODE], t[0]);
  if (sty->flush &&
      (sty->compact != COMPACT_ALL) &&
      (is_multi_paragraph (t[0])) || (sty->compact == COMPACT_NONE))
    r= tree (SURROUND, "", compound ("right-flush"), r);
  return r;
}

tree
rewrite_inactive_hybrid (tree t, inactive_style sty) {
  if (is_atomic (t[0]) && (sty->special >= SPECIAL_NORMAL)) {
    int i, n= N(t);
    tree r (INLINE_TAG, n);
    r[0]= tree (CONCAT, "\\", highlight (t[0], "var"));
    for (i=1; i<n; i++)
      r[i]= rewrite_inactive_arg (t, i, reset_bf (sty));
    return r;
  }
  return rewrite_inactive_default (t, sty);
}

tree
rewrite_inactive_default (tree t, inactive_style sty) {
  int i, d= 0, n= N(t);
  tree op= as_string (L(t));
  if ((L(t) == COMPOUND) &&
      is_atomic (t[0]) &&
      (sty->special >= SPECIAL_NORMAL))
    {
      d = 1;
      op= highlight (t[0], "var");
    }
  if (sty->mode == INACTIVE_INLINE_ERROR ||
      sty->mode == INACTIVE_BLOCK_ERROR)
    op= highlight (op, "error");

  if ((N(t) == d) ||
      (sty->compact == COMPACT_ALL) ||
      ((!sty->block) && (sty->compact != COMPACT_NONE)) ||
      (!is_long (t)) && (sty->compact != COMPACT_NONE))
    {
      tree r (INLINE_TAG, n+1-d);
      r[0]= op;
      for (i=d; i<n; i++)
	r[i+1-d]= rewrite_inactive_arg (t, i, reset_bf (sty));
      return r;
    }
  else {
    tree doc (DOCUMENT);
    bool compact= (sty->compact < COMPACT_INLINE);
 
    for (i=d; i<n; i++) {
      tree next;
      if ((!compact) || is_long_arg (t, i)) {
	if (i==d) doc << tree (OPEN_TAG, op);
	inactive_style ss= set_bf (sty, true, sty->close >= CLOSE_LONG);
	next= rewrite_inactive_arg (t, i, ss);
	next= compound ("indent", next);
	i++;
      }

      int start= i;
      for (; i<n; i++)
	if ((!compact) || is_long_arg (t, i)) break;
      int end= i;
      tree_label l= MIDDLE_TAG;
      if (end == n) l= CLOSE_TAG;
      if (start == d) l= OPEN_TAG;
      tree u (l, end - start + 1);
      u[0]= op;
      for (i=0; i<end-start; i++)
	u[i+1]= rewrite_inactive_arg (t, start+i, reset_bf (sty));
      i= end-1;
      compact= (sty->compact < COMPACT_INLINE_START);

      if (start==d) doc << u;
      else {
	if (sty->close < CLOSE_LONG)
	  doc << tree (SURROUND, "", u, next);
	else doc << next << u;
      }
    }

    if (sty->flush) doc= tree (SURROUND, "", compound ("right-flush"), doc);
    return doc;
  }
}

tree
rewrite_inactive_impl (tree t, inactive_style sty) {
  switch (L(t)) {
  case UNINIT:
    if (sty->special >= SPECIAL_NORMAL)
      return highlight ("?", "error");
    else return rewrite_inactive_default (t, sty);
  case RAW_DATA:
    return rewrite_inactive_raw_data (t, sty);
  case DOCUMENT:
    return rewrite_inactive_document (t, sty);
  case CONCAT:
    return rewrite_inactive_concat (t, sty);
  case VALUE:
    return rewrite_inactive_value (t, sty);
  case ARG:
    return rewrite_inactive_arg (t, sty);
  case STYLE_WITH:
    return rewrite_inactive_style_with (t, sty, true);
  case VAR_STYLE_WITH:
    return rewrite_inactive_style_with (t, sty, false);
  case STYLE_ONLY:
    return rewrite_inactive_active (t, sty);
  case VAR_STYLE_ONLY:
    return rewrite_inactive_var_active (t, sty);
  case ACTIVE:
    return rewrite_inactive_active (t, sty);
  case VAR_ACTIVE:
    return rewrite_inactive_var_active (t, sty);
  case SYMBOL:
    return rewrite_inactive_symbol (t, sty);
  case HYBRID:
    return rewrite_inactive_hybrid (t, sty);
  default:
    return rewrite_inactive_default (t, sty);
  }
}

/******************************************************************************
* Main rewriting routines
******************************************************************************/

static tree quote1 (WITH, "color", "blue", "``");
static tree quote2 (WITH, "color", "blue", "''");

tree
rewrite_inactive (tree t, inactive_style sty) {
  if (is_atomic (t)) {
    if (sty->style == STYLE_SCHEME)
      return tree (CONCAT, quote1, t, quote2);
    return t;
  }
  cout << "Inactive "
       << "[" << (t.operator -> ())
       << ", " << (std_env.operator -> ()) << "] "
       << t << INDENT << LF;
  memorizer mem= inactive_memorizer (std_env, t, sty);
  if (is_memorized (mem)) {
    cout << UNINDENT << "Memorized " << mem->get_tree () << LF;
    std_env= mem->get_environment ();
    return mem->get_tree ();
  }
  memorize_start ();
  tree r= rewrite_inactive_impl (t, sty);
  mem->set_tree (r);
  mem->set_environment (std_env);
  memorize_end ();
  cout << UNINDENT << "Rewritten as " << mem->get_tree () << LF;
  return mem->get_tree ();
}

tree
rewrite_inactive (tree t, int inactive_mode) {
  //recover_env= tuple ();
  inactive_style sty= retrieve (std_env);
  sty->mode= inactive_mode;
  bool flag= (sty->mode >= INACTIVE_BLOCK_RECURSE);
  sty->block= sty->flush= flag;
  tree r= rewrite_inactive (t, sty);
  if (is_multi_paragraph (r)) {
    r= tree (WITH, psep_var, psep_0fn, r);
    r= tree (SURROUND, surround1, surround2, r);
  }
  if ((sty->mode == INACTIVE_INLINE_RECURSE) ||
      (sty->mode == INACTIVE_BLOCK_RECURSE))
    r= tree (WITH, mode_var, mode_src, r);
  return r;
}
