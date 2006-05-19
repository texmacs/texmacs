
/******************************************************************************
* MODULE     : evaluate.cpp
* DESCRIPTION: Execution of scheme commands via guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "guile.hpp"
#include "Glue/glue.hpp"
#include "Scheme/evaluate.hpp"
#include "file.hpp"
#include "timer.hpp"

SCM object_stack;

/******************************************************************************
* Installation of guile and initialization of guile
******************************************************************************/

void
start_guile (int argc, char** argv, void (*call_back) (int, char**)) {
#ifdef DOTS_OK
  gh_enter (argc, argv, (void (*)(...)) ((void*) call_back));
#else
  gh_enter (argc, argv, call_back);
#endif
}

void
initialize_guile () {
  char* init_prg =
    "(read-set! keywords 'prefix)\n"
    "(read-enable 'positions)\n"
    "(debug-enable 'debug)\n"
    ";(debug-enable 'backtrace)\n"
    "\n"
    "(define (object->string obj)\n"
    "  (call-with-output-string\n"
    "    (lambda (port) (write obj port))))\n"
    "\n"
    "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
    "(define object-stack '(()))";

  scm_c_eval_string (init_prg);
  initialize_glue ();
  object_stack= gh_lookup ("object-stack");
}

/******************************************************************************
* Catching errors (with thanks to Dale P. Smith)
******************************************************************************/

SCM
TeXmacs_lazy_catcher (void *data, SCM tag, SCM throw_args) {
  SCM eport= scm_current_error_port();
  scm_handle_by_message_noexit (data, tag, throw_args);
  scm_force_output (eport);
  scm_ithrow (tag, throw_args, 1);
  return SCM_UNSPECIFIED; /* never returns */
}

SCM
TeXmacs_catcher (void *data, SCM tag, SCM args) {
  (void) data;
  return scm_cons (tag, args);
}

/******************************************************************************
* Evaluation of files
******************************************************************************/

static SCM
TeXmacs_lazy_eval_file (char *file) {
  return scm_internal_lazy_catch (SCM_BOOL_T,
    (scm_t_catch_body) scm_c_primitive_load, file,
    (scm_t_catch_handler) TeXmacs_lazy_catcher, file);
}

static SCM
TeXmacs_eval_file (char *file) {
  return scm_internal_catch (SCM_BOOL_T,
    (scm_t_catch_body) TeXmacs_lazy_eval_file, file,
    (scm_t_catch_handler) TeXmacs_catcher, file);
}

SCM
eval_scheme_file (string file) {
  //static int cumul= 0;
  //timer tm;
  if (DEBUG_STD) cout << "TeXmacs] Evaluating " << file << "...\n";
  char* _file= as_charp (file);
  SCM result= TeXmacs_eval_file (_file);
  delete[] _file;
  //int extra= tm->watch (); cumul += extra;
  //cout << extra << "\t" << cumul << "\t" << file << "\n";
  return result;
}

/******************************************************************************
* Evaluation of strings
******************************************************************************/

static SCM
TeXmacs_lazy_eval_string (char *s) {
  return scm_internal_lazy_catch (SCM_BOOL_T,
    (scm_t_catch_body) scm_c_eval_string, s,
    (scm_t_catch_handler) TeXmacs_lazy_catcher, s);
}

static SCM
TeXmacs_eval_string (char *s) {
  return scm_internal_catch (SCM_BOOL_T,
    (scm_t_catch_body) TeXmacs_lazy_eval_string, s,
    (scm_t_catch_handler) TeXmacs_catcher, s);
}

SCM
eval_scheme (string s) {
  // cout << "Eval] " << s << "\n";
  char* _s= as_charp (s);
  SCM result= TeXmacs_eval_string (_s);
  delete[] _s;
  return result;
}

/******************************************************************************
* Using scheme objects as functions
******************************************************************************/

struct arg_list { int  n; SCM* a; };

static SCM
TeXmacs_call (arg_list* args) {
  switch (args->n) {
  case 0: return scm_call_0 (args->a[0]); break;
  case 1: return scm_call_1 (args->a[0], args->a[1]); break;
  case 2: return scm_call_2 (args->a[0], args->a[1], args->a[2]); break;
  case 3:
    return scm_call_3 (args->a[0], args->a[1], args->a[2], args->a[3]); break;
  default:
    {
      int i;
      SCM l= SCM_NULL;
      for (i=args->n; i>=1; i--)
	l= scm_cons (args->a[i], l);
      return scm_apply_0 (args->a[0], l);
    }
  }
}

static SCM
TeXmacs_lazy_call_scm (arg_list* args) {
  return scm_internal_lazy_catch (SCM_BOOL_T,
    (scm_t_catch_body) TeXmacs_call, (void*) args,
    (scm_t_catch_handler) TeXmacs_lazy_catcher, (void*) args);
}

static SCM
TeXmacs_call_scm (arg_list *args) {
  return scm_internal_catch (SCM_BOOL_T,
    (scm_t_catch_body) TeXmacs_lazy_call_scm, (void*) args,
    (scm_t_catch_handler) TeXmacs_catcher, (void*) args);
}

SCM
call_scheme (SCM fun) {
  SCM a[]= { fun }; arg_list args= { 0, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1) {
  SCM a[]= { fun, a1 }; arg_list args= { 1, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1, SCM a2) {
  SCM a[]= { fun, a1, a2 }; arg_list args= { 2, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1, SCM a2, SCM a3) {
  SCM a[]= { fun, a1, a2, a3 }; arg_list args= { 3, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, array<SCM> a) {
  const int n= N(a);
  STACK_NEW_ARRAY(scm, SCM, n+1);
  int i;
  scm[0]= fun;
  for (i=0; i<n; i++) scm[i+1]= a[i];
  arg_list args= { n, scm };
  SCM ret= TeXmacs_call_scm (&args);
  STACK_DELETE_ARRAY(scm);
  return ret;
}
