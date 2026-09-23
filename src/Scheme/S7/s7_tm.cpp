
/******************************************************************************
* MODULE     : s7_tm.cpp
* DESCRIPTION: Interface to S7
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "s7_tm.hpp"
#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs (should not belong here)

#include <unistd.h> // for getpid

/******************************************************************************
 * Initialization of s7
 ******************************************************************************/

s7_scheme *tm_s7;
s7_pointer user_env;

int tm_s7_argc;
char **tm_s7_argv;

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  tm_s7_argc = argc;
  tm_s7_argv = argv;
  
  tm_s7 = s7_init ();

  // make a new user environment (used in evaluation)
  user_env = s7_inlet (tm_s7, s7_nil (tm_s7));
  s7_gc_protect (tm_s7, user_env);
  
  call_back (argc, argv);
}

/******************************************************************************
 * Evaluation of files
 ******************************************************************************/

tmscm
eval_scheme_file (string file) {
    //static int cumul= 0;
    //timer tm;
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
  c_string _file (file);
  tmscm result= s7_load_with_environment (tm_s7, _file, user_env);
    //int extra= tm->watch (); cumul += extra;
    //cout << extra << "\t" << cumul << "\t" << file << "\n";
  return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/

tmscm
eval_scheme (string s) {
  // cout << "Eval] " << s << "\n";
  c_string _s (s);
  tmscm result= s7_eval_c_string_with_environment (tm_s7, _s, user_env);
  return result;
}

/******************************************************************************
 * Using scheme objects as functions
 ******************************************************************************/

struct arg_list { int  n; tmscm* a; };

static tmscm
TeXmacs_call_scm (arg_list *args) {
  int i;
  tmscm l= s7_nil (tm_s7);
  for (i=args->n; i>=1; i--)
    l= s7_cons (tm_s7, args->a[i], l);
  return s7_call (tm_s7, args->a[0], l);
}

tmscm
call_scheme (tmscm fun) {
// uncomment block to display scheme call
/*
  tmscm ENDLscm= scm_from_locale_string ("\n");
  tmscm source=scm_procedure_source(fun);
  scm_call_2(scm_c_eval_string("display*"), source, ENDLscm);
  scm_call_2(scm_c_eval_string("display*"),  scm_procedure_environment(fun), ENDLscm);
  scm_call_2(scm_c_eval_string("display*"),  scm_procedure_properties(fun), ENDLscm);
  //DBGFMT1(debug_tmwidgets, source);
*/
  tmscm a[]= { fun }; arg_list args= { 0, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  tmscm a[]= { fun, a1 }; arg_list args= { 1, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  tmscm a[]= { fun, a1, a2 }; arg_list args= { 2, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  tmscm a[]= { fun, a1, a2, a3 }; arg_list args= { 3, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  tmscm a[]= { fun, a1, a2, a3, a4 }; arg_list args= { 4, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  const int n= N(a);
  STACK_NEW_ARRAY(scm, tmscm, n+1);
  int i;
  scm[0]= fun;
  for (i=0; i<n; i++) scm[i+1]= a[i];
  arg_list args= { n, scm };
  tmscm ret= TeXmacs_call_scm (&args);
  STACK_DELETE_ARRAY(scm);
  return ret;
}


/******************************************************************************
 * Miscellaneous routines for use by glue only
 ******************************************************************************/

string
scheme_dialect () {
  return "s7";
}

/******************************************************************************
 * Strings
 ******************************************************************************/


tmscm
string_to_tmscm (string s) {
  c_string _s (s);
  tmscm r= s7_make_string_with_length (tm_s7, _s, N(s));
  return r;
}

string
tmscm_to_string (tmscm s) {
  s7_int len_r = s7_string_length (s);
  const char* _r= s7_string (s);
  string r (_r, len_r);
  return r;
}

/******************************************************************************
 * Symbols
 ******************************************************************************/

tmscm
symbol_to_tmscm (string s) {
  c_string _s (s);
  tmscm r= s7_make_symbol (tm_s7, _s);
  return r;
}

string
tmscm_to_symbol (tmscm s) {
  const char* _r= s7_symbol_name (s);
  string r (_r);
  return r;
}

/******************************************************************************
 * Blackbox
 ******************************************************************************/

static int blackbox_tag = 0;

static tmscm blackbox_to_string (s7_scheme *sc, tmscm args)
{
  // FIXME: take into account sc!
  
  tmscm blackbox_smob = s7_car (args);

  string s = "<blackbox>";
  int type_ = type_box (tmscm_to_blackbox (blackbox_smob)) ;
  if (type_ == type_helper<tree>::id) {
    tree t= tmscm_to_tree (blackbox_smob);
    s= "<tree " * tree_to_texmacs (t) * ">";
  }
  else if (type_ == type_helper<observer>::id) {
    s= "<observer>";
  }
  else if (type_ == type_helper<widget>::id) {
    s= "<widget>";
  }
  else if (type_ == type_helper<promise<widget> >::id) {
    s= "<promise-widget>";
  }
  else if (type_ == type_helper<command>::id) {
    s= "<command>";
  }
  else if (type_ == type_helper<url>::id) {
    url u= tmscm_to_url (blackbox_smob);
    s= "<url " * as_string (u) * ">";
  }
  else if (type_ == type_helper<modification>::id) {
    s= "<modification>";
  }
  else if (type_ == type_helper<patch>::id) {
    s= "<patch>";
  }
  
  return string_to_tmscm (s);
}
  
static s7_pointer free_blackbox (s7_scheme *sc, s7_pointer obj)
{
  blackbox *ptr = (blackbox *) s7_c_object_value (obj);
  tm_delete (ptr);

  // Don't remove this, segmentation error could happen :)
  return (NULL);
}

static s7_pointer mark_blackbox (s7_scheme *sc, s7_pointer obj)
{
  return (NULL);
}

static s7_pointer blackbox_is_equal(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p1, p2;
  p1 = s7_car (args);
  p2 = s7_cadr (args);
  if (p1 == p2)
    return s7_t (sc);
  if ((!s7_is_c_object (p2)) ||
      (s7_c_object_type (p2) != blackbox_tag))
    return s7_f (sc);
  return bool_to_tmscm (tmscm_to_blackbox (p1) == tmscm_to_blackbox (p2));
}


bool
tmscm_is_blackbox (tmscm t) {
  return (s7_is_c_object(t) && s7_c_object_type(t) == blackbox_tag);
}

tmscm
blackbox_to_tmscm (blackbox b) {
  return s7_make_c_object (tm_s7, blackbox_tag, (void*) (tm_new<blackbox> (b)));
}

blackbox
tmscm_to_blackbox (tmscm blackbox_smob) {
  return *((blackbox *) s7_c_object_value (blackbox_smob));
}

/******************************************************************************
 * Compatibility
 ******************************************************************************/

static s7_pointer g_current_time (s7_scheme *sc, s7_pointer args)
{
  s7_int res;
  
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday (&tp, NULL);
  res = tp.tv_sec;
#else
  timeb tb;
  ftime (&tb);
  res = tb.time;
#endif

  return s7_make_integer(sc, res);
}

static s7_pointer g_getpid (s7_scheme *sc, s7_pointer args)
{
//FIXME: we really have to use QCoreApplication::applicationPid()
//for cross-platform support
  
  return(s7_make_integer(sc, (s7_int)getpid()));
}

void
initialize_compat () {

  s7_pointer cur_env = s7_curlet (tm_s7);
  s7_scheme *sc = tm_s7;
  
  s7_define(sc, cur_env, s7_make_symbol (tm_s7, "current-time"),
    s7_make_typed_function (sc, "current-time", g_current_time, 0, 0,
                            false, "current-time", NULL));
  
  s7_define(sc, cur_env, s7_make_symbol(sc, "getpid"),
            s7_make_typed_function(sc, "getpid", g_getpid, 0, 0,
                                   false, "int getpid(void)",
                                   s7_make_signature(sc, 2, s7_make_symbol(sc, "integer?"), s7_t(sc))));
}



/******************************************************************************
 * Initialization
 ******************************************************************************/

void
initialize_smobs () {
  blackbox_tag = s7_make_c_type (tm_s7, "blackbox");
  s7_c_type_set_gc_free (tm_s7, blackbox_tag, free_blackbox);
  s7_c_type_set_gc_mark (tm_s7, blackbox_tag, mark_blackbox);
  s7_c_type_set_is_equal (tm_s7, blackbox_tag, blackbox_is_equal);
  s7_c_type_set_to_string (tm_s7, blackbox_tag, blackbox_to_string);
}

tmscm object_stack;

void
initialize_scheme () {
  const char* init_prg = "(begin \n"
//  "(read-set! keywords 'prefix)\n"
//  "(read-enable 'positions)\n"
//  "(debug-enable 'debug)\n"
#ifdef DEBUG_ON
//  "(debug-enable 'backtrace)\n"
#endif
//  "\n"
  "(define (display-to-string obj)\n"
  "  (call-with-output-string\n"
  "    (lambda (port) (display obj port))))\n"
  "\n"
  "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
  "(define object-stack '(()))\n"
  ")";

  // eval in the root enviornment
  s7_eval_c_string (tm_s7, init_prg);
  initialize_compat ();
  initialize_smobs ();
  initialize_glue ();
  object_stack= s7_name_to_value (tm_s7, "object-stack");
  
    // uncomment to have a guile repl available at startup	
    //	gh_repl(guile_argc, guile_argv);
    //scm_shell (guile_argc, guile_argv);
  
  
}

