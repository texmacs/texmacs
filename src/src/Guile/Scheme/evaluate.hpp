
/******************************************************************************
* MODULE     : evaluate.hpp
* DESCRIPTION: Execution of scheme commands via guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
SCM call_scheme (SCM fun, array<SCM> a);

#endif // defined EVALUATE_H
