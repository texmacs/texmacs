/* Copyright (C) 1999,2000,2001, 2003, 2006, 2008 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/gh.h"
#include "libguile/hash.h"
#include "libguile/list.h"
#include "libguile/ports.h"
#include "libguile/smob.h"
#include "libguile/symbols.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"

#include "libguile/environments.h"



scm_t_bits scm_tc16_environment;
scm_t_bits scm_tc16_observer;
#define DEFAULT_OBARRAY_SIZE 31

SCM scm_system_environment;



/* error conditions */

/*
 * Throw an error if symbol is not bound in environment func
 */
void
scm_error_environment_unbound (const char *func, SCM env, SCM symbol)
{
  /* Dirk:FIXME:: Should throw an environment:unbound type error */
  char error[] = "Symbol `~A' not bound in environment `~A'.";
  SCM arguments = scm_cons2 (symbol, env, SCM_EOL);
  scm_misc_error (func, error, arguments);
}


/*
 * Throw an error if func tried to create (define) or remove
 * (undefine) a new binding for symbol in env
 */
void
scm_error_environment_immutable_binding (const char *func, SCM env, SCM symbol)
{
  /* Dirk:FIXME:: Should throw an environment:immutable-binding type error */
  char error[] = "Immutable binding in environment ~A (symbol: `~A').";
  SCM arguments = scm_cons2 (env, symbol, SCM_EOL);
  scm_misc_error (func, error, arguments);
}


/*
 * Throw an error if func tried to change an immutable location.
 */
void
scm_error_environment_immutable_location (const char *func, SCM env, SCM symbol)
{
  /* Dirk:FIXME:: Should throw an environment:immutable-location type error */
  char error[] = "Immutable location in environment `~A' (symbol: `~A').";
  SCM arguments = scm_cons2 (env, symbol, SCM_EOL);
  scm_misc_error (func, error, arguments);
}



/* generic environments */


/* Create an environment for the given type.  Dereferencing type twice must
 * deliver the initialized set of environment functions.  Thus, type will
 * also determine the signature of the underlying environment implementation.
 * Dereferencing type once will typically deliver the data fields used by the
 * underlying environment implementation.
 */
SCM
scm_make_environment (void *type)
{
  return scm_cell (scm_tc16_environment, (scm_t_bits) type);
}


SCM_DEFINE (scm_environment_p, "environment?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is an environment, or @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_environment_p
{
  return scm_from_bool (SCM_ENVIRONMENT_P (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_bound_p, "environment-bound?", 2, 0, 0, 
	    (SCM env, SCM sym),
	    "Return @code{#t} if @var{sym} is bound in @var{env}, or\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_environment_bound_p
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, FUNC_NAME);

  return scm_from_bool (SCM_ENVIRONMENT_BOUND_P (env, sym));
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_ref, "environment-ref", 2, 0, 0,
	    (SCM env, SCM sym),
	    "Return the value of the location bound to @var{sym} in\n"
	    "@var{env}. If @var{sym} is unbound in @var{env}, signal an\n"
	    "@code{environment:unbound} error.")
#define FUNC_NAME s_scm_environment_ref
{
  SCM val;

  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, FUNC_NAME);

  val = SCM_ENVIRONMENT_REF (env, sym);

  if (!SCM_UNBNDP (val))
    return val;
  else
    scm_error_environment_unbound (FUNC_NAME, env, sym);
}
#undef FUNC_NAME


/* This C function is identical to environment-ref, except that if symbol is
 * unbound in env, it returns the value SCM_UNDEFINED, instead of signalling
 * an error.
 */ 
SCM
scm_c_environment_ref (SCM env, SCM sym)
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, "scm_c_environment_ref");
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, "scm_c_environment_ref");
  return SCM_ENVIRONMENT_REF (env, sym);
}


static SCM
environment_default_folder (SCM proc, SCM symbol, SCM value, SCM tail)
{
  return scm_call_3 (proc, symbol, value, tail);
}


SCM_DEFINE (scm_environment_fold, "environment-fold", 3, 0, 0, 
	    (SCM env, SCM proc, SCM init),
	    "Iterate over all the bindings in @var{env}, accumulating some\n"
	    "value.\n"
	    "For each binding in @var{env}, apply @var{proc} to the symbol\n"
	    "bound, its value, and the result from the previous application\n"
	    "of @var{proc}.\n"
	    "Use @var{init} as @var{proc}'s third argument the first time\n"
	    "@var{proc} is applied.\n"
	    "If @var{env} contains no bindings, this function simply returns\n"
	    "@var{init}.\n"
	    "If @var{env} binds the symbol sym1 to the value val1, sym2 to\n"
	    "val2, and so on, then this procedure computes:\n"
	    "@lisp\n"
	    "  (proc sym1 val1\n"
	    "        (proc sym2 val2\n"
	    "              ...\n"
	    "              (proc symn valn\n"
	    "                    init)))\n"
	    "@end lisp\n"
	    "Each binding in @var{env} will be processed exactly once.\n"
	    "@code{environment-fold} makes no guarantees about the order in\n"
	    "which the bindings are processed.\n"
	    "Here is a function which, given an environment, constructs an\n"
	    "association list representing that environment's bindings,\n"
	    "using environment-fold:\n"
	    "@lisp\n"
	    "  (define (environment->alist env)\n"
	    "    (environment-fold env\n"
	    "                      (lambda (sym val tail)\n"
	    "                        (cons (cons sym val) tail))\n"
	    "                      '()))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_environment_fold
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc)), 
	      proc, SCM_ARG2, FUNC_NAME);

  return SCM_ENVIRONMENT_FOLD (env, environment_default_folder, proc, init);
}
#undef FUNC_NAME


/* This is the C-level analog of environment-fold. For each binding in ENV,
 * make the call:
 *   (*proc) (data, symbol, value, previous)
 * where previous is the value returned from the last call to *PROC, or INIT
 * for the first call. If ENV contains no bindings, return INIT. 
 */
SCM
scm_c_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init)
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, "scm_c_environment_fold");

  return SCM_ENVIRONMENT_FOLD (env, proc, data, init);
}


SCM_DEFINE (scm_environment_define, "environment-define", 3, 0, 0, 
	    (SCM env, SCM sym, SCM val),
	    "Bind @var{sym} to a new location containing @var{val} in\n"
	    "@var{env}. If @var{sym} is already bound to another location\n"
	    "in @var{env} and the binding is mutable, that binding is\n"
	    "replaced.  The new binding and location are both mutable. The\n"
	    "return value is unspecified.\n"
	    "If @var{sym} is already bound in @var{env}, and the binding is\n"
	    "immutable, signal an @code{environment:immutable-binding} error.")
#define FUNC_NAME s_scm_environment_define
{
  SCM status;

  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, FUNC_NAME);

  status = SCM_ENVIRONMENT_DEFINE (env, sym, val);

  if (scm_is_eq (status, SCM_ENVIRONMENT_SUCCESS))
    return SCM_UNSPECIFIED;
  else if (scm_is_eq (status, SCM_ENVIRONMENT_BINDING_IMMUTABLE))
    scm_error_environment_immutable_binding (FUNC_NAME, env, sym);
  else
    abort();
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_undefine, "environment-undefine", 2, 0, 0, 
	    (SCM env, SCM sym),
	    "Remove any binding for @var{sym} from @var{env}. If @var{sym}\n"
	    "is unbound in @var{env}, do nothing.  The return value is\n"
	    "unspecified.\n"
	    "If @var{sym} is already bound in @var{env}, and the binding is\n"
	    "immutable, signal an @code{environment:immutable-binding} error.")
#define FUNC_NAME s_scm_environment_undefine
{
  SCM status;

  SCM_ASSERT(SCM_ENVIRONMENT_P(env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT(scm_is_symbol(sym), sym, SCM_ARG2, FUNC_NAME);

  status = SCM_ENVIRONMENT_UNDEFINE (env, sym);

  if (scm_is_eq (status, SCM_ENVIRONMENT_SUCCESS))
    return SCM_UNSPECIFIED;
  else if (scm_is_eq (status, SCM_ENVIRONMENT_BINDING_IMMUTABLE))
    scm_error_environment_immutable_binding (FUNC_NAME, env, sym);
  else
    abort();
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_set_x, "environment-set!", 3, 0, 0, 
	    (SCM env, SCM sym, SCM val),
	    "If @var{env} binds @var{sym} to some location, change that\n"
	    "location's value to @var{val}.  The return value is\n"
	    "unspecified.\n"
	    "If @var{sym} is not bound in @var{env}, signal an\n"
	    "@code{environment:unbound} error.  If @var{env} binds @var{sym}\n"
	    "to an immutable location, signal an\n"
	    "@code{environment:immutable-location} error.")
#define FUNC_NAME s_scm_environment_set_x
{
  SCM status;

  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, FUNC_NAME);

  status = SCM_ENVIRONMENT_SET (env, sym, val);

  if (scm_is_eq (status, SCM_ENVIRONMENT_SUCCESS))
    return SCM_UNSPECIFIED;
  else if (SCM_UNBNDP (status))
    scm_error_environment_unbound (FUNC_NAME, env, sym);
  else if (scm_is_eq (status, SCM_ENVIRONMENT_LOCATION_IMMUTABLE))
    scm_error_environment_immutable_binding (FUNC_NAME, env, sym);
  else
    abort();
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_cell, "environment-cell", 3, 0, 0, 
	    (SCM env, SCM sym, SCM for_write),
	    "Return the value cell which @var{env} binds to @var{sym}, or\n"
	    "@code{#f} if the binding does not live in a value cell.\n"
	    "The argument @var{for-write} indicates whether the caller\n"
	    "intends to modify the variable's value by mutating the value\n"
	    "cell.  If the variable is immutable, then\n"
	    "@code{environment-cell} signals an\n"
	    "@code{environment:immutable-location} error.\n"
	    "If @var{sym} is unbound in @var{env}, signal an\n"
	    "@code{environment:unbound} error.\n"
	    "If you use this function, you should consider using\n"
	    "@code{environment-observe}, to be notified when @var{sym} gets\n"
	    "re-bound to a new value cell, or becomes undefined.")
#define FUNC_NAME s_scm_environment_cell
{
  SCM location;

  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_bool (for_write), for_write, SCM_ARG3, FUNC_NAME);

  location = SCM_ENVIRONMENT_CELL (env, sym, scm_is_true (for_write));
  if (!SCM_IMP (location))
    return location;
  else if (SCM_UNBNDP (location))
    scm_error_environment_unbound (FUNC_NAME, env, sym);
  else if (scm_is_eq (location, SCM_ENVIRONMENT_LOCATION_IMMUTABLE))
    scm_error_environment_immutable_location (FUNC_NAME, env, sym);
  else /* no cell */
    return location;
}
#undef FUNC_NAME


/* This C function is identical to environment-cell, with the following
 * exceptions:   If symbol is unbound in env, it returns the value
 * SCM_UNDEFINED, instead of signalling an error.  If symbol is bound to an
 * immutable location but the cell is requested for write, the value 
 * SCM_ENVIRONMENT_LOCATION_IMMUTABLE is returned.
 */
SCM
scm_c_environment_cell(SCM env, SCM sym, int for_write)
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, "scm_c_environment_cell");
  SCM_ASSERT (scm_is_symbol (sym), sym, SCM_ARG2, "scm_c_environment_cell");

  return SCM_ENVIRONMENT_CELL (env, sym, for_write);
}


static void
environment_default_observer (SCM env, SCM proc)
{
  scm_call_1 (proc, env);
}


SCM_DEFINE (scm_environment_observe, "environment-observe", 2, 0, 0, 
	    (SCM env, SCM proc),
	    "Whenever @var{env}'s bindings change, apply @var{proc} to\n"
	    "@var{env}.\n"
	    "This function returns an object, token, which you can pass to\n"
	    "@code{environment-unobserve} to remove @var{proc} from the set\n"
	    "of procedures observing @var{env}.  The type and value of\n"
	    "token is unspecified.")
#define FUNC_NAME s_scm_environment_observe
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return SCM_ENVIRONMENT_OBSERVE (env, environment_default_observer, proc, 0);
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_observe_weak, "environment-observe-weak", 2, 0, 0,
	    (SCM env, SCM proc),
	    "This function is the same as environment-observe, except that\n"
	    "the reference @var{env} retains to @var{proc} is a weak\n"
	    "reference. This means that, if there are no other live,\n"
	    "non-weak references to @var{proc}, it will be\n"
	    "garbage-collected, and dropped from @var{env}'s\n"
	    "list of observing procedures.")
#define FUNC_NAME s_scm_environment_observe_weak
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return SCM_ENVIRONMENT_OBSERVE (env, environment_default_observer, proc, 1);
}
#undef FUNC_NAME


/* This is the C-level analog of the Scheme functions environment-observe and
 * environment-observe-weak.  Whenever env's bindings change, call the
 * function proc, passing it env and data. If weak_p is non-zero, env will
 * retain only a weak reference to data, and if data is garbage collected, the
 * entire observation will be dropped.  This function returns a token, with
 * the same meaning as those returned by environment-observe and
 * environment-observe-weak.
 */
SCM
scm_c_environment_observe (SCM env, scm_environment_observer proc, SCM data, int weak_p)
#define FUNC_NAME "scm_c_environment_observe"
{
  SCM_ASSERT (SCM_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return SCM_ENVIRONMENT_OBSERVE (env, proc, data, weak_p);
}
#undef FUNC_NAME


SCM_DEFINE (scm_environment_unobserve, "environment-unobserve", 1, 0, 0, 
	    (SCM token),
	    "Cancel the observation request which returned the value\n"
	    "@var{token}.  The return value is unspecified.\n"
	    "If a call @code{(environment-observe env proc)} returns\n"
	    "@var{token}, then the call @code{(environment-unobserve token)}\n"
	    "will cause @var{proc} to no longer be called when @var{env}'s\n"
	    "bindings change.")
#define FUNC_NAME s_scm_environment_unobserve
{
  SCM env;

  SCM_ASSERT (SCM_OBSERVER_P (token), token, SCM_ARG1, FUNC_NAME);

  env = SCM_OBSERVER_ENVIRONMENT (token);
  SCM_ENVIRONMENT_UNOBSERVE (env, token);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static SCM
environment_mark (SCM env)
{
  return (*(SCM_ENVIRONMENT_FUNCS (env)->mark)) (env);
}


static size_t
environment_free (SCM env)
{
  (*(SCM_ENVIRONMENT_FUNCS (env)->free)) (env);
  return 0;
}


static int
environment_print (SCM env, SCM port, scm_print_state *pstate)
{
  return (*(SCM_ENVIRONMENT_FUNCS (env)->print)) (env, port, pstate);
}



/* observers */

static SCM
observer_mark (SCM observer)
{
  scm_gc_mark (SCM_OBSERVER_ENVIRONMENT (observer));
  scm_gc_mark (SCM_OBSERVER_DATA (observer));
  return SCM_BOOL_F;
}


static int
observer_print (SCM type, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  SCM address = scm_from_size_t (SCM_UNPACK (type));
  SCM base16 = scm_number_to_string (address, scm_from_int (16));

  scm_puts ("#<observer ", port);
  scm_display (base16, port);
  scm_puts (">", port);

  return 1;
}



/* obarrays
 *
 * Obarrays form the basic lookup tables used to implement most of guile's
 * built-in environment types.  An obarray is implemented as a hash table with
 * symbols as keys.  The content of the data depends on the environment type.
 */


/*
 * Enter symbol into obarray.  The symbol must not already exist in obarray.
 * The freshly generated (symbol . data) cell is returned.
 */
static SCM
obarray_enter (SCM obarray, SCM symbol, SCM data)
{
  size_t hash = scm_i_symbol_hash (symbol) % SCM_HASHTABLE_N_BUCKETS (obarray);
  SCM entry = scm_cons (symbol, data);
  SCM slot = scm_cons (entry, SCM_HASHTABLE_BUCKET (obarray, hash));
  SCM_SET_HASHTABLE_BUCKET  (obarray, hash, slot);
  SCM_HASHTABLE_INCREMENT (obarray);
  if (SCM_HASHTABLE_N_ITEMS (obarray) > SCM_HASHTABLE_UPPER (obarray))
    scm_i_rehash (obarray, scm_i_hash_symbol, 0, "obarray_enter");

  return entry;
}


/*
 * Enter symbol into obarray.  An existing entry for symbol is replaced.  If
 * an entry existed, the old (symbol . data) cell is returned, #f otherwise.
 */
static SCM
obarray_replace (SCM obarray, SCM symbol, SCM data)
{
  size_t hash = scm_i_symbol_hash (symbol) % SCM_HASHTABLE_N_BUCKETS (obarray);
  SCM new_entry = scm_cons (symbol, data);
  SCM lsym;
  SCM slot;

  for (lsym = SCM_HASHTABLE_BUCKET (obarray, hash);
       !scm_is_null (lsym);
       lsym = SCM_CDR (lsym))
    {
      SCM old_entry = SCM_CAR (lsym);
      if (scm_is_eq (SCM_CAR (old_entry), symbol))
	{
	  SCM_SETCAR (lsym, new_entry);
	  return old_entry;
	}
    }

  slot = scm_cons (new_entry, SCM_HASHTABLE_BUCKET (obarray, hash));
  SCM_SET_HASHTABLE_BUCKET (obarray, hash, slot);
  SCM_HASHTABLE_INCREMENT (obarray);
  if (SCM_HASHTABLE_N_ITEMS (obarray) > SCM_HASHTABLE_UPPER (obarray))
    scm_i_rehash (obarray, scm_i_hash_symbol, 0, "obarray_replace");

  return SCM_BOOL_F;
}


/*
 * Look up symbol in obarray
 */
static SCM
obarray_retrieve (SCM obarray, SCM sym)
{
  size_t hash = scm_i_symbol_hash (sym) % SCM_HASHTABLE_N_BUCKETS (obarray);
  SCM lsym;

  for (lsym = SCM_HASHTABLE_BUCKET (obarray, hash);
       !scm_is_null (lsym);
       lsym = SCM_CDR (lsym))
    {
      SCM entry = SCM_CAR (lsym);
      if (scm_is_eq (SCM_CAR (entry), sym))
	return entry;
    }

  return SCM_UNDEFINED;
}


/*
 * Remove entry from obarray.  If the symbol was found and removed, the old
 * (symbol . data) cell is returned, #f otherwise.
 */
static SCM
obarray_remove (SCM obarray, SCM sym)
{
  size_t hash = scm_i_symbol_hash (sym) % SCM_HASHTABLE_N_BUCKETS (obarray);
  SCM table_entry = SCM_HASHTABLE_BUCKET (obarray, hash);
  SCM handle = scm_sloppy_assq (sym, table_entry);

  if (scm_is_pair (handle))
    {
      SCM new_table_entry = scm_delq1_x (handle, table_entry);
      SCM_SET_HASHTABLE_BUCKET (obarray, hash, new_table_entry);
      SCM_HASHTABLE_DECREMENT (obarray);
    }

  return handle;
}


static void
obarray_remove_all (SCM obarray)
{
  size_t size = SCM_HASHTABLE_N_BUCKETS (obarray);
  size_t i;

  for (i = 0; i < size; i++)
    {
      SCM_SET_HASHTABLE_BUCKET (obarray, i, SCM_EOL);
    }
  SCM_SET_HASHTABLE_N_ITEMS (obarray, 0);
}



/* core environments base
 *
 * This struct and the corresponding functions form a base class for guile's
 * built-in environment types.
 */


struct core_environments_base {
  struct scm_environment_funcs *funcs;

  SCM observers;
  SCM weak_observers;
};


#define CORE_ENVIRONMENTS_BASE(env) \
  ((struct core_environments_base *) SCM_CELL_WORD_1 (env))
#define CORE_ENVIRONMENT_OBSERVERS(env) \
  (CORE_ENVIRONMENTS_BASE (env)->observers)
#define SCM_SET_CORE_ENVIRONMENT_OBSERVERS(env, v) \
  (CORE_ENVIRONMENT_OBSERVERS (env) = (v))
#define CORE_ENVIRONMENT_WEAK_OBSERVER_VECTOR(env) \
  (CORE_ENVIRONMENTS_BASE (env)->weak_observers)
#define CORE_ENVIRONMENT_WEAK_OBSERVERS(env) \
  (scm_c_vector_ref (CORE_ENVIRONMENT_WEAK_OBSERVER_VECTOR (env), 0))
#define SCM_SET_CORE_ENVIRONMENT_WEAK_OBSERVERS(env, v) \
  (scm_c_vector_set_x (CORE_ENVIRONMENT_WEAK_OBSERVER_VECTOR (env), 0, (v)))



static SCM
core_environments_observe (SCM env, scm_environment_observer proc, SCM data, int weak_p)
{
  SCM observer = scm_double_cell (scm_tc16_observer,
				  SCM_UNPACK (env),
				  SCM_UNPACK (data),
				  (scm_t_bits) proc);

  if (!weak_p)
    {
      SCM observers = CORE_ENVIRONMENT_OBSERVERS (env);
      SCM new_observers = scm_cons (observer, observers);
      SCM_SET_CORE_ENVIRONMENT_OBSERVERS (env, new_observers);
    }
  else
    {
      SCM observers = CORE_ENVIRONMENT_WEAK_OBSERVERS (env);
      SCM new_observers = scm_acons (SCM_BOOL_F, observer, observers);
      SCM_SET_CORE_ENVIRONMENT_WEAK_OBSERVERS (env, new_observers);
    }

  return observer;
}


static void
core_environments_unobserve (SCM env, SCM observer)
{
  unsigned int handling_weaks;
  for (handling_weaks = 0; handling_weaks <= 1; ++handling_weaks)
    {
      SCM l = handling_weaks
	? CORE_ENVIRONMENT_WEAK_OBSERVERS (env)
	: CORE_ENVIRONMENT_OBSERVERS (env);

      if (!scm_is_null (l))
	{
	  SCM rest = SCM_CDR (l);
	  SCM first = handling_weaks
	    ? SCM_CDAR (l)
	    : SCM_CAR (l);

	  if (scm_is_eq (first, observer))
	    {
	      /* Remove the first observer */
	      if (handling_weaks)
		SCM_SET_CORE_ENVIRONMENT_WEAK_OBSERVERS (env, rest);
              else
                SCM_SET_CORE_ENVIRONMENT_OBSERVERS (env, rest);
	      return;
	    }

	  do {
	    SCM rest = SCM_CDR (l);

	    if (!scm_is_null (rest)) 
	      {
		SCM next = handling_weaks
		  ? SCM_CDAR (l)
		  : SCM_CAR (l);

		if (scm_is_eq (next, observer))
		  {
		    SCM_SETCDR (l, SCM_CDR (rest));
		    return;
		  }
	      }

	    l = rest;
	  } while (!scm_is_null (l));
	}
    }

  /* Dirk:FIXME:: What to do now, since the observer is not found? */
}


static SCM
core_environments_mark (SCM env)
{
  scm_gc_mark (CORE_ENVIRONMENT_OBSERVERS (env));
  return CORE_ENVIRONMENT_WEAK_OBSERVER_VECTOR (env);
}


static void
core_environments_finalize (SCM env SCM_UNUSED)
{
}


static void
core_environments_preinit (struct core_environments_base *body)
{
  body->funcs = NULL;
  body->observers = SCM_BOOL_F;
  body->weak_observers = SCM_BOOL_F;
}


static void
core_environments_init (struct core_environments_base *body,
			       struct scm_environment_funcs *funcs)
{
  body->funcs = funcs;
  body->observers = SCM_EOL;
  body->weak_observers = scm_make_weak_value_alist_vector (scm_from_int (1));
}


/* Tell all observers to clear their caches.
 *
 * Environments have to be informed about changes in the following cases:
 * - The observed env has a new binding.  This must be always reported.
 * - The observed env has dropped a binding.  This must be always reported.
 * - A binding in the observed environment has changed.  This must only be
 *   reported, if there is a chance that the binding is being cached outside.
 *   However, this potential optimization is not performed currently.
 *
 * Errors that occur while the observers are called are accumulated and
 * signalled as one single error message to the caller.
 */

struct update_data
{
  SCM observer;
  SCM environment;
};


static SCM
update_catch_body (void *ptr)
{
  struct update_data *data = (struct update_data *) ptr;
  SCM observer = data->observer;

  (*SCM_OBSERVER_PROC (observer)) 
    (data->environment, SCM_OBSERVER_DATA (observer));

  return SCM_UNDEFINED;
}


static SCM
update_catch_handler (void *ptr, SCM tag, SCM args)
{
  struct update_data *data = (struct update_data *) ptr;
  SCM observer = data->observer;
  SCM message =
    scm_from_locale_string ("Observer `~A' signals `~A' error: ~S");

  return scm_cons (message, scm_list_3 (observer, tag, args));
}


static void
core_environments_broadcast (SCM env)
#define FUNC_NAME "core_environments_broadcast"
{
  unsigned int handling_weaks;
  SCM errors = SCM_EOL;

  for (handling_weaks = 0; handling_weaks <= 1; ++handling_weaks)
    {
      SCM observers = handling_weaks
	? CORE_ENVIRONMENT_WEAK_OBSERVERS (env)
	: CORE_ENVIRONMENT_OBSERVERS (env);

      for (; !scm_is_null (observers); observers = SCM_CDR (observers))
	{
          struct update_data data;
	  SCM observer = handling_weaks
	    ? SCM_CDAR (observers)
	    : SCM_CAR (observers);
          SCM error;

          data.observer = observer;
          data.environment = env;

          error = scm_internal_catch (SCM_BOOL_T, 
                                      update_catch_body, &data, 
                                      update_catch_handler, &data);

          if (!SCM_UNBNDP (error))
            errors = scm_cons (error, errors);
	}
    }

  if (!scm_is_null (errors))
    {
      /* Dirk:FIXME:: As soon as scm_misc_error is fixed to handle the name
       * parameter correctly it should not be necessary any more to also pass
       * namestr in order to get the desired information from the error
       * message.
       */
      SCM ordered_errors = scm_reverse (errors);
      scm_misc_error 
        (FUNC_NAME,
         "Observers of `~A' have signalled the following errors: ~S",
         scm_cons2 (env, ordered_errors, SCM_EOL));
    }
}
#undef FUNC_NAME



/* leaf environments
 *
 * A leaf environment is simply a mutable set of definitions. A leaf
 * environment supports no operations beyond the common set.
 *
 * Implementation:  The obarray of the leaf environment holds (symbol . value)
 * pairs.  No further information is necessary, since all bindings and
 * locations in a leaf environment are mutable.
 */


struct leaf_environment {
  struct core_environments_base base;

  SCM obarray;
};


#define LEAF_ENVIRONMENT(env) \
  ((struct leaf_environment *) SCM_CELL_WORD_1 (env))



static SCM
leaf_environment_ref (SCM env, SCM sym)
{
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;
  SCM binding = obarray_retrieve (obarray, sym);
  return SCM_UNBNDP (binding) ? binding : SCM_CDR (binding);
}


static SCM
leaf_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init)
{
  size_t i;
  SCM result = init;
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;

  for (i = 0; i < SCM_HASHTABLE_N_BUCKETS (obarray); i++)
    {
      SCM l;
      for (l = SCM_HASHTABLE_BUCKET (obarray, i);
	   !scm_is_null (l);
	   l = SCM_CDR (l))
	{
	  SCM binding = SCM_CAR (l);
	  SCM symbol = SCM_CAR (binding);
	  SCM value = SCM_CDR (binding);
	  result = (*proc) (data, symbol, value, result);
	}
    }
  return result;
}


static SCM
leaf_environment_define (SCM env, SCM sym, SCM val)
#define FUNC_NAME "leaf_environment_define"
{
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;

  obarray_replace (obarray, sym, val);
  core_environments_broadcast (env);

  return SCM_ENVIRONMENT_SUCCESS;
}
#undef FUNC_NAME


static SCM
leaf_environment_undefine (SCM env, SCM sym)
#define FUNC_NAME "leaf_environment_undefine"
{
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;
  SCM removed = obarray_remove (obarray, sym);
  
  if (scm_is_true (removed))
    core_environments_broadcast (env);

  return SCM_ENVIRONMENT_SUCCESS;
}
#undef FUNC_NAME


static SCM
leaf_environment_set_x (SCM env, SCM sym, SCM val)
#define FUNC_NAME "leaf_environment_set_x"
{
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;
  SCM binding = obarray_retrieve (obarray, sym);

  if (!SCM_UNBNDP (binding))
    {
      SCM_SETCDR (binding, val);
      return SCM_ENVIRONMENT_SUCCESS;
    }
  else
    {
      return SCM_UNDEFINED;
    }
}
#undef FUNC_NAME


static SCM
leaf_environment_cell (SCM env, SCM sym, int for_write SCM_UNUSED)
{
  SCM obarray = LEAF_ENVIRONMENT (env)->obarray;
  SCM binding = obarray_retrieve (obarray, sym);
  return binding;
}


static SCM
leaf_environment_mark (SCM env)
{
  scm_gc_mark (LEAF_ENVIRONMENT (env)->obarray);
  return core_environments_mark (env);
}


static void
leaf_environment_free (SCM env)
{
  core_environments_finalize (env);
  scm_gc_free (LEAF_ENVIRONMENT (env), sizeof (struct leaf_environment),
	       "leaf environment");
}


static int
leaf_environment_print (SCM type, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  SCM address = scm_from_size_t (SCM_UNPACK (type));
  SCM base16 = scm_number_to_string (address, scm_from_int (16));

  scm_puts ("#<leaf environment ", port);
  scm_display (base16, port);
  scm_puts (">", port);

  return 1;
}


static struct scm_environment_funcs leaf_environment_funcs = {
  leaf_environment_ref,
  leaf_environment_fold,
  leaf_environment_define,
  leaf_environment_undefine,
  leaf_environment_set_x,
  leaf_environment_cell,
  core_environments_observe,
  core_environments_unobserve,
  leaf_environment_mark,
  leaf_environment_free,
  leaf_environment_print
};


void *scm_type_leaf_environment = &leaf_environment_funcs;


SCM_DEFINE (scm_make_leaf_environment, "make-leaf-environment", 0, 0, 0, 
	    (),
	    "Create a new leaf environment, containing no bindings.\n"
	    "All bindings and locations created in the new environment\n"
	    "will be mutable.")
#define FUNC_NAME s_scm_make_leaf_environment
{
  size_t size = sizeof (struct leaf_environment);
  struct leaf_environment *body = scm_gc_malloc (size, "leaf environment");
  SCM env;

  core_environments_preinit (&body->base);
  body->obarray = SCM_BOOL_F;

  env = scm_make_environment (body);

  core_environments_init (&body->base, &leaf_environment_funcs);
  body->obarray = scm_c_make_hash_table (DEFAULT_OBARRAY_SIZE);  

  return env;
}
#undef FUNC_NAME


SCM_DEFINE (scm_leaf_environment_p, "leaf-environment?", 1, 0, 0, 
	    (SCM object),
	    "Return @code{#t} if object is a leaf environment, or @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_leaf_environment_p
{
  return scm_from_bool (SCM_LEAF_ENVIRONMENT_P (object));
}
#undef FUNC_NAME



/* eval environments
 *
 * A module's source code refers to definitions imported from other modules,
 * and definitions made within itself.  An eval environment combines two
 * environments -- a local environment and an imported environment -- to
 * produce a new environment in which both sorts of references can be
 * resolved.
 *
 * Implementation:  The obarray of the eval environment is used to cache
 * entries from the local and imported environments such that in most of the
 * cases only a single lookup is necessary.  Since for neither the local nor
 * the imported environment it is known, what kind of environment they form,
 * the most general case is assumed.  Therefore, entries in the obarray take
 * one of the following forms:
 *
 * 1) (<symbol> location mutability . source-env), where mutability indicates
 *    one of the following states:  IMMUTABLE if the location is known to be
 *    immutable, MUTABLE if the location is known to be mutable, UNKNOWN if
 *    the location has only been requested for non modifying accesses.
 *
 * 2) (symbol . source-env) if the symbol has a binding in the source-env, but
 *    if the source-env can't provide a cell for the binding.  Thus, for every
 *    access, the source-env has to be contacted directly.
 */


struct eval_environment {
  struct core_environments_base base;

  SCM obarray;

  SCM imported;
  SCM imported_observer;
  SCM local;
  SCM local_observer;
};


#define EVAL_ENVIRONMENT(env) \
  ((struct eval_environment *) SCM_CELL_WORD_1 (env))

#define IMMUTABLE SCM_I_MAKINUM (0)
#define MUTABLE   SCM_I_MAKINUM (1)
#define UNKNOWN   SCM_I_MAKINUM (2)

#define CACHED_LOCATION(x) SCM_CAR (x)
#define CACHED_MUTABILITY(x) SCM_CADR (x)
#define SET_CACHED_MUTABILITY(x, v) SCM_SETCAR (SCM_CDR (x), (v))
#define CACHED_SOURCE_ENVIRONMENT(x) SCM_CDDR (x)



/* eval_environment_lookup will report one of the following distinct results:
 * a) (<object> . value) if a cell could be obtained.
 * b) <environment> if the environment has to be contacted directly.
 * c) IMMUTABLE if an immutable cell was requested for write.
 * d) SCM_UNDEFINED if there is no binding for the symbol.
 */
static SCM
eval_environment_lookup (SCM env, SCM sym, int for_write)
{
  SCM obarray = EVAL_ENVIRONMENT (env)->obarray;
  SCM binding = obarray_retrieve (obarray, sym);

  if (!SCM_UNBNDP (binding))
    {
      /* The obarray holds an entry for the symbol. */

      SCM entry = SCM_CDR (binding);

      if (scm_is_pair (entry))
	{
	  /* The entry in the obarray is a cached location. */

	  SCM location = CACHED_LOCATION (entry);
	  SCM mutability;

	  if (!for_write)
	    return location;

	  mutability = CACHED_MUTABILITY (entry);
	  if (scm_is_eq (mutability, MUTABLE))
	    return location;

	  if (scm_is_eq (mutability, UNKNOWN))
	    {
	      SCM source_env = CACHED_SOURCE_ENVIRONMENT (entry);
	      SCM location = SCM_ENVIRONMENT_CELL (source_env, sym, 1);

	      if (scm_is_pair (location))
		{
		  SET_CACHED_MUTABILITY (entry, MUTABLE);
		  return location;
		}
	      else /* IMMUTABLE */
		{
		  SET_CACHED_MUTABILITY (entry, IMMUTABLE);
		  return IMMUTABLE;
		}
	    }

	  return IMMUTABLE;
	}
      else
	{
	  /* The obarray entry is an environment */

	  return entry;
	}
    }
  else
    {
      /* There is no entry for the symbol in the obarray.  This can either
       * mean that there has not been a request for the symbol yet, or that
       * the symbol is really undefined.  We are looking for the symbol in
       * both the local and the imported environment.  If we find a binding, a
       * cached entry is created.
       */

      struct eval_environment *body = EVAL_ENVIRONMENT (env);
      unsigned int handling_import;

      for (handling_import = 0; handling_import <= 1; ++handling_import)
	{
	  SCM source_env = handling_import ? body->imported : body->local;
	  SCM location = SCM_ENVIRONMENT_CELL (source_env, sym, for_write);

	  if (!SCM_UNBNDP (location))
	    {
	      if (scm_is_pair (location))
		{
		  SCM mutability = for_write ? MUTABLE : UNKNOWN;
		  SCM entry = scm_cons2 (location, mutability, source_env);
		  obarray_enter (obarray, sym, entry);
		  return location;
		}
	      else if (scm_is_eq (location, SCM_ENVIRONMENT_LOCATION_NO_CELL))
		{
		  obarray_enter (obarray, sym, source_env);
		  return source_env;
		}
	      else
		{
		  return IMMUTABLE;
		}
	    }
	}

      return SCM_UNDEFINED;
    }
}


static SCM
eval_environment_ref (SCM env, SCM sym)
#define FUNC_NAME "eval_environment_ref"
{
  SCM location = eval_environment_lookup (env, sym, 0);

  if (scm_is_pair (location))
    return SCM_CDR (location);
  else if (!SCM_UNBNDP (location))
    return SCM_ENVIRONMENT_REF (location, sym);
  else
    return SCM_UNDEFINED;
}
#undef FUNC_NAME


static SCM
eval_environment_folder (SCM extended_data, SCM symbol, SCM value, SCM tail)
{
  SCM local = SCM_CAR (extended_data);

  if (!SCM_ENVIRONMENT_BOUND_P (local, symbol))
    {
      SCM proc_as_nr = SCM_CADR (extended_data);
      unsigned long int proc_as_ul = scm_to_ulong (proc_as_nr);
      scm_environment_folder proc = (scm_environment_folder) proc_as_ul;
      SCM data = SCM_CDDR (extended_data);

      return (*proc) (data, symbol, value, tail);
    }
  else
    {
      return tail;
    }
}


static SCM
eval_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init)
{
  SCM local = EVAL_ENVIRONMENT (env)->local;
  SCM imported = EVAL_ENVIRONMENT (env)->imported;
  SCM proc_as_nr = scm_from_ulong ((unsigned long) proc);
  SCM extended_data = scm_cons2 (local, proc_as_nr, data);
  SCM tmp_result = scm_c_environment_fold (imported, eval_environment_folder, extended_data, init);

  return scm_c_environment_fold (local, proc, data, tmp_result);
}


static SCM
eval_environment_define (SCM env, SCM sym, SCM val)
#define FUNC_NAME "eval_environment_define"
{
  SCM local = EVAL_ENVIRONMENT (env)->local;
  return SCM_ENVIRONMENT_DEFINE (local, sym, val);
}
#undef FUNC_NAME


static SCM
eval_environment_undefine (SCM env, SCM sym)
#define FUNC_NAME "eval_environment_undefine"
{
  SCM local = EVAL_ENVIRONMENT (env)->local;
  return SCM_ENVIRONMENT_UNDEFINE (local, sym);
}
#undef FUNC_NAME


static SCM
eval_environment_set_x (SCM env, SCM sym, SCM val)
#define FUNC_NAME "eval_environment_set_x"
{
  SCM location = eval_environment_lookup (env, sym, 1);

  if (scm_is_pair (location))
    {
      SCM_SETCDR (location, val);
      return SCM_ENVIRONMENT_SUCCESS;
    }
  else if (SCM_ENVIRONMENT_P (location))
    {
      return SCM_ENVIRONMENT_SET (location, sym, val);
    }
  else if (scm_is_eq (location, IMMUTABLE))
    {
      return SCM_ENVIRONMENT_LOCATION_IMMUTABLE;
    }
  else
    {
      return SCM_UNDEFINED;
    }
}
#undef FUNC_NAME


static SCM
eval_environment_cell (SCM env, SCM sym, int for_write)
#define FUNC_NAME "eval_environment_cell"
{
  SCM location = eval_environment_lookup (env, sym, for_write);

  if (scm_is_pair (location))
    return location;
  else if (SCM_ENVIRONMENT_P (location))
    return SCM_ENVIRONMENT_LOCATION_NO_CELL;
  else if (scm_is_eq (location, IMMUTABLE))
    return SCM_ENVIRONMENT_LOCATION_IMMUTABLE;
  else
    return SCM_UNDEFINED;
}
#undef FUNC_NAME


static SCM
eval_environment_mark (SCM env)
{
  struct eval_environment *body = EVAL_ENVIRONMENT (env);

  scm_gc_mark (body->obarray);
  scm_gc_mark (body->imported);
  scm_gc_mark (body->imported_observer);
  scm_gc_mark (body->local);
  scm_gc_mark (body->local_observer);

  return core_environments_mark (env);
}


static void
eval_environment_free (SCM env)
{
  core_environments_finalize (env);
  scm_gc_free (EVAL_ENVIRONMENT (env), sizeof (struct eval_environment),
	       "eval environment");
}


static int
eval_environment_print (SCM type, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  SCM address = scm_from_size_t (SCM_UNPACK (type));
  SCM base16 = scm_number_to_string (address, scm_from_int (16));

  scm_puts ("#<eval environment ", port);
  scm_display (base16, port);
  scm_puts (">", port);

  return 1;
}


static struct scm_environment_funcs eval_environment_funcs = {
    eval_environment_ref,
    eval_environment_fold,
    eval_environment_define,
    eval_environment_undefine,
    eval_environment_set_x,
    eval_environment_cell,
    core_environments_observe,
    core_environments_unobserve,
    eval_environment_mark,
    eval_environment_free,
    eval_environment_print
};


void *scm_type_eval_environment = &eval_environment_funcs;


static void
eval_environment_observer (SCM caller SCM_UNUSED, SCM eval_env)
{
  SCM obarray = EVAL_ENVIRONMENT (eval_env)->obarray;

  obarray_remove_all (obarray);
  core_environments_broadcast (eval_env);
}


SCM_DEFINE (scm_make_eval_environment, "make-eval-environment", 2, 0, 0, 
	    (SCM local, SCM imported),
	    "Return a new environment object eval whose bindings are the\n"
	    "union of the bindings in the environments @var{local} and\n"
	    "@var{imported}, with bindings from @var{local} taking\n"
	    "precedence. Definitions made in eval are placed in @var{local}.\n"
	    "Applying @code{environment-define} or\n"
	    "@code{environment-undefine} to eval has the same effect as\n"
	    "applying the procedure to @var{local}.\n"
	    "Note that eval incorporates @var{local} and @var{imported} by\n"
	    "reference:\n"
	    "If, after creating eval, the program changes the bindings of\n"
	    "@var{local} or @var{imported}, those changes will be visible\n"
	    "in eval.\n"
	    "Since most Scheme evaluation takes place in eval environments,\n"
	    "they transparently cache the bindings received from @var{local}\n"
	    "and @var{imported}. Thus, the first time the program looks up\n"
	    "a symbol in eval, eval may make calls to @var{local} or\n"
	    "@var{imported} to find their bindings, but subsequent\n"
	    "references to that symbol will be as fast as references to\n"
	    "bindings in finite environments.\n"
	    "In typical use, @var{local} will be a finite environment, and\n"
	    "@var{imported} will be an import environment")
#define FUNC_NAME s_scm_make_eval_environment
{
  SCM env;
  struct eval_environment *body;

  SCM_ASSERT (SCM_ENVIRONMENT_P (local), local, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_ENVIRONMENT_P (imported), imported, SCM_ARG2, FUNC_NAME);

  body = scm_gc_malloc (sizeof (struct eval_environment), "eval environment");

  core_environments_preinit (&body->base);
  body->obarray = SCM_BOOL_F;
  body->imported = SCM_BOOL_F;
  body->imported_observer = SCM_BOOL_F;
  body->local = SCM_BOOL_F;
  body->local_observer = SCM_BOOL_F;

  env = scm_make_environment (body);

  core_environments_init (&body->base, &eval_environment_funcs);
  body->obarray = scm_c_make_hash_table (DEFAULT_OBARRAY_SIZE);  
  body->imported = imported;
  body->imported_observer
    = SCM_ENVIRONMENT_OBSERVE (imported, eval_environment_observer, env, 1);
  body->local = local;
  body->local_observer
    = SCM_ENVIRONMENT_OBSERVE (local, eval_environment_observer, env, 1);

  return env;
}
#undef FUNC_NAME


SCM_DEFINE (scm_eval_environment_p, "eval-environment?", 1, 0, 0,
	    (SCM object),
	    "Return @code{#t} if object is an eval environment, or @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_eval_environment_p
{
  return scm_from_bool (SCM_EVAL_ENVIRONMENT_P (object));
}
#undef FUNC_NAME


SCM_DEFINE (scm_eval_environment_local, "eval-environment-local", 1, 0, 0, 
	    (SCM env),
	    "Return the local environment of eval environment @var{env}.")
#define FUNC_NAME s_scm_eval_environment_local
{
  SCM_ASSERT (SCM_EVAL_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return EVAL_ENVIRONMENT (env)->local;
}
#undef FUNC_NAME


SCM_DEFINE (scm_eval_environment_set_local_x, "eval-environment-set-local!", 2, 0, 0, 
	    (SCM env, SCM local),
	    "Change @var{env}'s local environment to @var{local}.")
#define FUNC_NAME s_scm_eval_environment_set_local_x
{
  struct eval_environment *body;

  SCM_ASSERT (SCM_EVAL_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_ENVIRONMENT_P (local), local, SCM_ARG2, FUNC_NAME);

  body = EVAL_ENVIRONMENT (env);

  obarray_remove_all (body->obarray);
  SCM_ENVIRONMENT_UNOBSERVE (body->local, body->local_observer);

  body->local = local;
  body->local_observer
    = SCM_ENVIRONMENT_OBSERVE (local, eval_environment_observer, env, 1);

  core_environments_broadcast (env);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_eval_environment_imported, "eval-environment-imported", 1, 0, 0,
	    (SCM env),
	    "Return the imported environment of eval environment @var{env}.")
#define FUNC_NAME s_scm_eval_environment_imported
{
  SCM_ASSERT (SCM_EVAL_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return EVAL_ENVIRONMENT (env)->imported;
}
#undef FUNC_NAME


SCM_DEFINE (scm_eval_environment_set_imported_x, "eval-environment-set-imported!", 2, 0, 0, 
	    (SCM env, SCM imported),
	    "Change @var{env}'s imported environment to @var{imported}.")
#define FUNC_NAME s_scm_eval_environment_set_imported_x
{
  struct eval_environment *body;

  SCM_ASSERT (SCM_EVAL_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_ENVIRONMENT_P (imported), imported, SCM_ARG2, FUNC_NAME);

  body = EVAL_ENVIRONMENT (env);

  obarray_remove_all (body->obarray);
  SCM_ENVIRONMENT_UNOBSERVE (body->imported, body->imported_observer);

  body->imported = imported;
  body->imported_observer
    = SCM_ENVIRONMENT_OBSERVE (imported, eval_environment_observer, env, 1);

  core_environments_broadcast (env);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* import environments
 *
 * An import environment combines the bindings of a set of argument
 * environments, and checks for naming clashes.
 *
 * Implementation:  The import environment does no caching at all.  For every
 * access, the list of imported environments is scanned.
 */


struct import_environment {
  struct core_environments_base base;

  SCM imports;
  SCM import_observers;

  SCM conflict_proc;
};


#define IMPORT_ENVIRONMENT(env) \
  ((struct import_environment *) SCM_CELL_WORD_1 (env))



/* Lookup will report one of the following distinct results:
 * a) <environment> if only environment binds the symbol.
 * b) (env-1 env-2 ...) for conflicting bindings in env-1, ...
 * c) SCM_UNDEFINED if there is no binding for the symbol.
 */
static SCM
import_environment_lookup (SCM env, SCM sym)
{
  SCM imports = IMPORT_ENVIRONMENT (env)->imports;
  SCM result = SCM_UNDEFINED;
  SCM l;

  for (l = imports; !scm_is_null (l); l = SCM_CDR (l))
    {
      SCM imported = SCM_CAR (l);

      if (SCM_ENVIRONMENT_BOUND_P (imported, sym))
	{
	  if (SCM_UNBNDP (result))
	    result = imported;
	  else if (scm_is_pair (result))
	    result = scm_cons (imported, result);
	  else
	    result = scm_cons2 (imported, result, SCM_EOL);
	}
    }

  if (scm_is_pair (result))
    return scm_reverse (result);
  else
    return result;
}


static SCM
import_environment_conflict (SCM env, SCM sym, SCM imports)
{
  SCM conflict_proc = IMPORT_ENVIRONMENT (env)->conflict_proc;
  SCM args = scm_cons2 (env, sym, scm_cons (imports, SCM_EOL));

  return scm_apply_0 (conflict_proc, args);
}


static SCM
import_environment_ref (SCM env, SCM sym)
#define FUNC_NAME "import_environment_ref"
{
  SCM owner = import_environment_lookup (env, sym);

  if (SCM_UNBNDP (owner))
    {
      return SCM_UNDEFINED;
    }
  else if (scm_is_pair (owner))
    {
      SCM resolve = import_environment_conflict (env, sym, owner);

      if (SCM_ENVIRONMENT_P (resolve))
	return SCM_ENVIRONMENT_REF (resolve, sym);
      else
	return SCM_UNSPECIFIED;
    }
  else
    {
      return SCM_ENVIRONMENT_REF (owner, sym);
    }
}
#undef FUNC_NAME


static SCM
import_environment_folder (SCM extended_data, SCM symbol, SCM value, SCM tail)
#define FUNC_NAME "import_environment_fold"
{
  SCM import_env = SCM_CAR (extended_data);
  SCM imported_env = SCM_CADR (extended_data);
  SCM owner = import_environment_lookup (import_env, symbol);
  SCM proc_as_nr = SCM_CADDR (extended_data);
  unsigned long int proc_as_ul = scm_to_ulong (proc_as_nr);
  scm_environment_folder proc = (scm_environment_folder) proc_as_ul;
  SCM data = SCM_CDDDR (extended_data);

  if (scm_is_pair (owner) && scm_is_eq (SCM_CAR (owner), imported_env))
    owner = import_environment_conflict (import_env, symbol, owner);

  if (SCM_ENVIRONMENT_P (owner))
    return (*proc) (data, symbol, value, tail);
  else /* unresolved conflict */
    return (*proc) (data, symbol, SCM_UNSPECIFIED, tail);
}
#undef FUNC_NAME


static SCM
import_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init)
{
  SCM proc_as_nr = scm_from_ulong ((unsigned long) proc);
  SCM result = init;
  SCM l;

  for (l = IMPORT_ENVIRONMENT (env)->imports; !scm_is_null (l); l = SCM_CDR (l))
    {
      SCM imported_env = SCM_CAR (l);
      SCM extended_data = scm_cons (env, scm_cons2 (imported_env, proc_as_nr, data));

      result = scm_c_environment_fold (imported_env, import_environment_folder, extended_data, result);
    }

  return result;
}


static SCM
import_environment_define (SCM env SCM_UNUSED,
			   SCM sym SCM_UNUSED,
			   SCM val SCM_UNUSED)
#define FUNC_NAME "import_environment_define"
{
  return SCM_ENVIRONMENT_BINDING_IMMUTABLE;
}
#undef FUNC_NAME


static SCM
import_environment_undefine (SCM env SCM_UNUSED,
			     SCM sym SCM_UNUSED)
#define FUNC_NAME "import_environment_undefine"
{
  return SCM_ENVIRONMENT_BINDING_IMMUTABLE;
}
#undef FUNC_NAME


static SCM
import_environment_set_x (SCM env, SCM sym, SCM val)
#define FUNC_NAME "import_environment_set_x"
{
  SCM owner = import_environment_lookup (env, sym);

  if (SCM_UNBNDP (owner))
    {
      return SCM_UNDEFINED;
    }
  else if (scm_is_pair (owner))
    {
      SCM resolve = import_environment_conflict (env, sym, owner);

      if (SCM_ENVIRONMENT_P (resolve))
	return SCM_ENVIRONMENT_SET (resolve, sym, val);
      else
	return SCM_ENVIRONMENT_LOCATION_IMMUTABLE;
    }
  else
    {
      return SCM_ENVIRONMENT_SET (owner, sym, val);
    }
}
#undef FUNC_NAME


static SCM
import_environment_cell (SCM env, SCM sym, int for_write)
#define FUNC_NAME "import_environment_cell"
{
  SCM owner = import_environment_lookup (env, sym);

  if (SCM_UNBNDP (owner))
    {
      return SCM_UNDEFINED;
    }
  else if (scm_is_pair (owner))
    {
      SCM resolve = import_environment_conflict (env, sym, owner);

      if (SCM_ENVIRONMENT_P (resolve))
	return SCM_ENVIRONMENT_CELL (resolve, sym, for_write);
      else
	return SCM_ENVIRONMENT_LOCATION_NO_CELL;
    }
  else
    {
      return SCM_ENVIRONMENT_CELL (owner, sym, for_write);
    }
}
#undef FUNC_NAME


static SCM
import_environment_mark (SCM env)
{
  scm_gc_mark (IMPORT_ENVIRONMENT (env)->imports);
  scm_gc_mark (IMPORT_ENVIRONMENT (env)->import_observers);
  scm_gc_mark (IMPORT_ENVIRONMENT (env)->conflict_proc);
  return core_environments_mark (env);
}


static void
import_environment_free (SCM env)
{
  core_environments_finalize (env);
  scm_gc_free (IMPORT_ENVIRONMENT (env), sizeof (struct import_environment),
	       "import environment");
}


static int
import_environment_print (SCM type, SCM port, 
			  scm_print_state *pstate SCM_UNUSED)
{
  SCM address = scm_from_size_t (SCM_UNPACK (type));
  SCM base16 = scm_number_to_string (address, scm_from_int (16));

  scm_puts ("#<import environment ", port);
  scm_display (base16, port);
  scm_puts (">", port);

  return 1;
}


static struct scm_environment_funcs import_environment_funcs = {
  import_environment_ref,
  import_environment_fold,
  import_environment_define,
  import_environment_undefine,
  import_environment_set_x,
  import_environment_cell,
  core_environments_observe,
  core_environments_unobserve,
  import_environment_mark,
  import_environment_free,
  import_environment_print
};


void *scm_type_import_environment = &import_environment_funcs;


static void
import_environment_observer (SCM caller SCM_UNUSED, SCM import_env)
{
  core_environments_broadcast (import_env);
}


SCM_DEFINE (scm_make_import_environment, "make-import-environment", 2, 0, 0, 
	    (SCM imports, SCM conflict_proc),
	    "Return a new environment @var{imp} whose bindings are the union\n"
	    "of the bindings from the environments in @var{imports};\n"
	    "@var{imports} must be a list of environments. That is,\n"
	    "@var{imp} binds a symbol to a location when some element of\n"
	    "@var{imports} does.\n"
	    "If two different elements of @var{imports} have a binding for\n"
	    "the same symbol, the @var{conflict-proc} is called with the\n"
	    "following parameters:  the import environment, the symbol and\n"
	    "the list of the imported environments that bind the symbol.\n"
	    "If the @var{conflict-proc} returns an environment @var{env},\n"
	    "the conflict is considered as resolved and the binding from\n"
	    "@var{env} is used.  If the @var{conflict-proc} returns some\n"
	    "non-environment object, the conflict is considered unresolved\n"
	    "and the symbol is treated as unspecified in the import\n"
	    "environment.\n"
	    "The checking for conflicts may be performed lazily, i. e. at\n"
	    "the moment when a value or binding for a certain symbol is\n"
	    "requested instead of the moment when the environment is\n"
	    "created or the bindings of the imports change.\n"
	    "All bindings in @var{imp} are immutable. If you apply\n"
	    "@code{environment-define} or @code{environment-undefine} to\n"
	    "@var{imp}, Guile will signal an\n"
	    " @code{environment:immutable-binding} error. However,\n"
	    "notice that the set of bindings in @var{imp} may still change,\n"
	    "if one of its imported environments changes.")
#define FUNC_NAME s_scm_make_import_environment
{
  size_t size = sizeof (struct import_environment);
  struct import_environment *body = scm_gc_malloc (size, "import environment");
  SCM env;

  core_environments_preinit (&body->base);
  body->imports = SCM_BOOL_F;
  body->import_observers = SCM_BOOL_F;
  body->conflict_proc = SCM_BOOL_F;

  env = scm_make_environment (body);

  core_environments_init (&body->base, &import_environment_funcs);
  body->imports = SCM_EOL;
  body->import_observers = SCM_EOL;
  body->conflict_proc = conflict_proc;

  scm_import_environment_set_imports_x (env, imports);

  return env;
}
#undef FUNC_NAME


SCM_DEFINE (scm_import_environment_p, "import-environment?", 1, 0, 0, 
	    (SCM object),
	    "Return @code{#t} if object is an import environment, or\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_import_environment_p
{
  return scm_from_bool (SCM_IMPORT_ENVIRONMENT_P (object));
}
#undef FUNC_NAME


SCM_DEFINE (scm_import_environment_imports, "import-environment-imports", 1, 0, 0, 
	    (SCM env),
	    "Return the list of environments imported by the import\n"
	    "environment @var{env}.")
#define FUNC_NAME s_scm_import_environment_imports
{
  SCM_ASSERT (SCM_IMPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return IMPORT_ENVIRONMENT (env)->imports;
}
#undef FUNC_NAME


SCM_DEFINE (scm_import_environment_set_imports_x, "import-environment-set-imports!", 2, 0, 0, 
	    (SCM env, SCM imports),
	    "Change @var{env}'s list of imported environments to\n"
	    "@var{imports}, and check for conflicts.")
#define FUNC_NAME s_scm_import_environment_set_imports_x
{
  struct import_environment *body = IMPORT_ENVIRONMENT (env);
  SCM import_observers = SCM_EOL;
  SCM l;

  SCM_ASSERT (SCM_IMPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  for (l = imports; scm_is_pair (l); l = SCM_CDR (l))
    {
      SCM obj = SCM_CAR (l);
      SCM_ASSERT (SCM_ENVIRONMENT_P (obj), imports, SCM_ARG2, FUNC_NAME);
    }
  SCM_ASSERT (scm_is_null (l), imports, SCM_ARG2, FUNC_NAME);

  for (l = body->import_observers; !scm_is_null (l); l = SCM_CDR (l))
    {
      SCM obs = SCM_CAR (l);
      SCM_ENVIRONMENT_UNOBSERVE (env, obs);
    }

  for (l = imports; !scm_is_null (l); l = SCM_CDR (l))
    {
      SCM imp = SCM_CAR (l);
      SCM obs = SCM_ENVIRONMENT_OBSERVE (imp, import_environment_observer, env, 1);
      import_observers = scm_cons (obs, import_observers);
    }

  body->imports = imports;
  body->import_observers = import_observers;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* export environments
 *
 * An export environment restricts an environment to a specified set of
 * bindings.
 *
 * Implementation:  The export environment does no caching at all.  For every
 * access, the signature is scanned.  The signature that is stored internally
 * is an alist of pairs (symbol . (mutability)).
 */


struct export_environment {
  struct core_environments_base base;

  SCM private;
  SCM private_observer;

  SCM signature;
};


#define EXPORT_ENVIRONMENT(env) \
  ((struct export_environment *) SCM_CELL_WORD_1 (env))


SCM_SYMBOL (symbol_immutable_location, "immutable-location");
SCM_SYMBOL (symbol_mutable_location, "mutable-location");



static SCM
export_environment_ref (SCM env, SCM sym)
#define FUNC_NAME "export_environment_ref"
{
  struct export_environment *body = EXPORT_ENVIRONMENT (env);
  SCM entry = scm_assq (sym, body->signature);

  if (scm_is_false (entry))
    return SCM_UNDEFINED;
  else
    return SCM_ENVIRONMENT_REF (body->private, sym);
}
#undef FUNC_NAME


static SCM
export_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init)
{
  struct export_environment *body = EXPORT_ENVIRONMENT (env);
  SCM result = init;
  SCM l;

  for (l = body->signature; !scm_is_null (l); l = SCM_CDR (l))
    {
      SCM symbol = SCM_CAR (l);
      SCM value = SCM_ENVIRONMENT_REF (body->private, symbol);
      if (!SCM_UNBNDP (value))
	result = (*proc) (data, symbol, value, result);
    }
  return result;
}


static SCM
export_environment_define (SCM env SCM_UNUSED, 
			   SCM sym SCM_UNUSED, 
			   SCM val SCM_UNUSED)
#define FUNC_NAME "export_environment_define"
{
  return SCM_ENVIRONMENT_BINDING_IMMUTABLE;
}
#undef FUNC_NAME


static SCM
export_environment_undefine (SCM env SCM_UNUSED, SCM sym SCM_UNUSED)
#define FUNC_NAME "export_environment_undefine"
{
  return SCM_ENVIRONMENT_BINDING_IMMUTABLE;
}
#undef FUNC_NAME


static SCM
export_environment_set_x (SCM env, SCM sym, SCM val)
#define FUNC_NAME "export_environment_set_x"
{
  struct export_environment *body = EXPORT_ENVIRONMENT (env);
  SCM entry = scm_assq (sym, body->signature);

  if (scm_is_false (entry))
    {
      return SCM_UNDEFINED;
    }
  else
    {
      if (scm_is_eq (SCM_CADR (entry), symbol_mutable_location))
	return SCM_ENVIRONMENT_SET (body->private, sym, val);
      else
	return SCM_ENVIRONMENT_LOCATION_IMMUTABLE;
    }
}
#undef FUNC_NAME


static SCM
export_environment_cell (SCM env, SCM sym, int for_write)
#define FUNC_NAME "export_environment_cell"
{
  struct export_environment *body = EXPORT_ENVIRONMENT (env);
  SCM entry = scm_assq (sym, body->signature);

  if (scm_is_false (entry))
    {
      return SCM_UNDEFINED;
    }
  else
    {
      if (!for_write || scm_is_eq (SCM_CADR (entry), symbol_mutable_location))
	return SCM_ENVIRONMENT_CELL (body->private, sym, for_write);
      else
	return SCM_ENVIRONMENT_LOCATION_IMMUTABLE;
    }
}
#undef FUNC_NAME


static SCM
export_environment_mark (SCM env)
{
  struct export_environment *body = EXPORT_ENVIRONMENT (env);

  scm_gc_mark (body->private);
  scm_gc_mark (body->private_observer);
  scm_gc_mark (body->signature);

  return core_environments_mark (env);
}


static void
export_environment_free (SCM env)
{
  core_environments_finalize (env);
  scm_gc_free (EXPORT_ENVIRONMENT (env), sizeof (struct export_environment),
	       "export environment");
}


static int
export_environment_print (SCM type, SCM port,
			  scm_print_state *pstate SCM_UNUSED)
{
  SCM address = scm_from_size_t (SCM_UNPACK (type));
  SCM base16 = scm_number_to_string (address, scm_from_int (16));

  scm_puts ("#<export environment ", port);
  scm_display (base16, port);
  scm_puts (">", port);

  return 1;
}


static struct scm_environment_funcs export_environment_funcs = {
  export_environment_ref,
  export_environment_fold,
  export_environment_define,
  export_environment_undefine,
  export_environment_set_x,
  export_environment_cell,
  core_environments_observe,
  core_environments_unobserve,
  export_environment_mark,
  export_environment_free,
  export_environment_print
};


void *scm_type_export_environment = &export_environment_funcs;


static void
export_environment_observer (SCM caller SCM_UNUSED, SCM export_env)
{
  core_environments_broadcast (export_env);
}


SCM_DEFINE (scm_make_export_environment, "make-export-environment", 2, 0, 0, 
	    (SCM private, SCM signature),
	    "Return a new environment @var{exp} containing only those\n"
	    "bindings in private whose symbols are present in\n"
	    "@var{signature}. The @var{private} argument must be an\n"
	    "environment.\n\n"
	    "The environment @var{exp} binds symbol to location when\n"
	    "@var{env} does, and symbol is exported by @var{signature}.\n\n"
	    "@var{signature} is a list specifying which of the bindings in\n"
	    "@var{private} should be visible in @var{exp}. Each element of\n"
	    "@var{signature} should be a list of the form:\n"
	    "  (symbol attribute ...)\n"
	    "where each attribute is one of the following:\n"
	    "@table @asis\n"
	    "@item the symbol @code{mutable-location}\n"
	    "  @var{exp} should treat the\n"
	    "  location bound to symbol as mutable. That is, @var{exp}\n"
	    "  will pass calls to @code{environment-set!} or\n"
	    "  @code{environment-cell} directly through to private.\n"
	    "@item the symbol @code{immutable-location}\n"
	    "  @var{exp} should treat\n"
	    "  the location bound to symbol as immutable. If the program\n"
	    "  applies @code{environment-set!} to @var{exp} and symbol, or\n"
	    "  calls @code{environment-cell} to obtain a writable value\n"
	    "  cell, @code{environment-set!} will signal an\n"
	    "  @code{environment:immutable-location} error. Note that, even\n"
	    "  if an export environment treats a location as immutable, the\n"
	    "  underlying environment may treat it as mutable, so its\n"
	    "  value may change.\n"
	    "@end table\n"
	    "It is an error for an element of signature to specify both\n"
	    "@code{mutable-location} and @code{immutable-location}. If\n"
	    "neither is specified, @code{immutable-location} is assumed.\n\n"
	    "As a special case, if an element of signature is a lone\n"
	    "symbol @var{sym}, it is equivalent to an element of the form\n"
	    "@code{(sym)}.\n\n"
	    "All bindings in @var{exp} are immutable. If you apply\n"
	    "@code{environment-define} or @code{environment-undefine} to\n"
	    "@var{exp}, Guile will signal an\n"
	    "@code{environment:immutable-binding} error. However,\n"
	    "notice that the set of bindings in @var{exp} may still change,\n"
	    "if the bindings in private change.")
#define FUNC_NAME s_scm_make_export_environment
{
  size_t size;
  struct export_environment *body;
  SCM env;

  SCM_ASSERT (SCM_ENVIRONMENT_P (private), private, SCM_ARG1, FUNC_NAME);

  size = sizeof (struct export_environment);
  body = scm_gc_malloc (size, "export environment");

  core_environments_preinit (&body->base);
  body->private = SCM_BOOL_F;
  body->private_observer = SCM_BOOL_F;
  body->signature = SCM_BOOL_F;

  env = scm_make_environment (body);

  core_environments_init (&body->base, &export_environment_funcs);
  body->private = private;
  body->private_observer
    = SCM_ENVIRONMENT_OBSERVE (private, export_environment_observer, env, 1);
  body->signature = SCM_EOL;

  scm_export_environment_set_signature_x (env, signature);

  return env;
}
#undef FUNC_NAME


SCM_DEFINE (scm_export_environment_p, "export-environment?", 1, 0, 0, 
	    (SCM object),
	    "Return @code{#t} if object is an export environment, or\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_export_environment_p
{
  return scm_from_bool (SCM_EXPORT_ENVIRONMENT_P (object));
}
#undef FUNC_NAME


SCM_DEFINE (scm_export_environment_private, "export-environment-private", 1, 0, 0, 
	    (SCM env),
	    "Return the private environment of export environment @var{env}.")
#define FUNC_NAME s_scm_export_environment_private
{
  SCM_ASSERT (SCM_EXPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return EXPORT_ENVIRONMENT (env)->private;
}
#undef FUNC_NAME


SCM_DEFINE (scm_export_environment_set_private_x, "export-environment-set-private!", 2, 0, 0, 
	    (SCM env, SCM private),
	    "Change the private environment of export environment @var{env}.")
#define FUNC_NAME s_scm_export_environment_set_private_x
{
  struct export_environment *body;

  SCM_ASSERT (SCM_EXPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_ENVIRONMENT_P (private), private, SCM_ARG2, FUNC_NAME);

  body = EXPORT_ENVIRONMENT (env);
  SCM_ENVIRONMENT_UNOBSERVE (private, body->private_observer);

  body->private = private;
  body->private_observer
    = SCM_ENVIRONMENT_OBSERVE (private, export_environment_observer, env, 1);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_export_environment_signature, "export-environment-signature", 1, 0, 0, 
	    (SCM env),
	    "Return the signature of export environment @var{env}.")
#define FUNC_NAME s_scm_export_environment_signature
{
  SCM_ASSERT (SCM_EXPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);

  return EXPORT_ENVIRONMENT (env)->signature;
}
#undef FUNC_NAME


static SCM
export_environment_parse_signature (SCM signature, const char* caller)
{
  SCM result = SCM_EOL;
  SCM l;

  for (l = signature; scm_is_pair (l); l = SCM_CDR (l))
    {
      SCM entry = SCM_CAR (l);

      if (scm_is_symbol (entry))
	{
	  SCM new_entry = scm_cons2 (entry, symbol_immutable_location, SCM_EOL);
	  result = scm_cons (new_entry, result);
	}
      else
	{
	  SCM sym;
	  SCM new_entry;
	  int immutable = 0;
	  int mutable = 0;
	  SCM mutability;
	  SCM l2;

	  SCM_ASSERT (scm_is_pair (entry), entry, SCM_ARGn, caller);
	  SCM_ASSERT (scm_is_symbol (SCM_CAR (entry)), entry, SCM_ARGn, caller);

	  sym = SCM_CAR (entry);

	  for (l2 = SCM_CDR (entry); scm_is_pair (l2); l2 = SCM_CDR (l2))
	    {
	      SCM attribute = SCM_CAR (l2);
	      if (scm_is_eq (attribute, symbol_immutable_location))
		immutable = 1;
	      else if (scm_is_eq (attribute, symbol_mutable_location))
		mutable = 1;
	      else
		SCM_ASSERT (0, entry, SCM_ARGn, caller);
	    }
	  SCM_ASSERT (scm_is_null (l2), entry, SCM_ARGn, caller);
	  SCM_ASSERT (!mutable || !immutable, entry, SCM_ARGn, caller);

	  if (!mutable && !immutable)
	    immutable = 1;

	  mutability = mutable ? symbol_mutable_location : symbol_immutable_location;
	  new_entry = scm_cons2 (sym, mutability, SCM_EOL);
	  result = scm_cons (new_entry, result);
	}
    }
  SCM_ASSERT (scm_is_null (l), signature, SCM_ARGn, caller);

  /* Dirk:FIXME:: Now we know that signature is syntactically correct.  There
   * are, however, no checks for symbols entered twice with contradicting
   * mutabilities.  It would be nice, to implement this test, to be able to
   * call the sort functions conveniently from C.
   */

  return scm_reverse (result);
}


SCM_DEFINE (scm_export_environment_set_signature_x, "export-environment-set-signature!", 2, 0, 0, 
	    (SCM env, SCM signature),
	    "Change the signature of export environment @var{env}.")
#define FUNC_NAME s_scm_export_environment_set_signature_x
{
  SCM parsed_sig;

  SCM_ASSERT (SCM_EXPORT_ENVIRONMENT_P (env), env, SCM_ARG1, FUNC_NAME);
  parsed_sig = export_environment_parse_signature (signature, FUNC_NAME);

  EXPORT_ENVIRONMENT (env)->signature = parsed_sig;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_environments_prehistory ()
{
  /* create environment smob */
  scm_tc16_environment = scm_make_smob_type ("environment", 0);
  scm_set_smob_mark (scm_tc16_environment, environment_mark);
  scm_set_smob_free (scm_tc16_environment, environment_free);
  scm_set_smob_print (scm_tc16_environment, environment_print);

  /* create observer smob */
  scm_tc16_observer = scm_make_smob_type ("observer", 0);
  scm_set_smob_mark (scm_tc16_observer, observer_mark);
  scm_set_smob_print (scm_tc16_observer, observer_print);

  /* create system environment */
  scm_system_environment = scm_make_leaf_environment ();
  scm_permanent_object (scm_system_environment);
}


void
scm_init_environments ()
{
#include "libguile/environments.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
