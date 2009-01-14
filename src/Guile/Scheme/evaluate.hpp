
/******************************************************************************
* MODULE     : evaluate.hpp
* DESCRIPTION: Execution of scheme commands via guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EVALUATE_H
#define EVALUATE_H
#include "guile.hpp"
#include "string.hpp"
#include "array.hpp"

void start_guile (int argc, char** argv, void (*call_back) (int, char**));
void initialize_guile ();

extern SCM object_stack;

SCM eval_scheme_file (string name);
SCM eval_scheme (string s);
SCM call_scheme (SCM fun);
SCM call_scheme (SCM fun, SCM a1);
SCM call_scheme (SCM fun, SCM a1, SCM a2);
SCM call_scheme (SCM fun, SCM a1, SCM a2, SCM a3);
SCM call_scheme (SCM fun, SCM a1, SCM a2, SCM a3, SCM a4);
SCM call_scheme (SCM fun, array<SCM> a);

#endif // defined EVALUATE_H
