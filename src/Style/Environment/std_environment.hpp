
/******************************************************************************
* MODULE     : std_environment.hpp
* DESCRIPTION: environments for standard TeXmacs style rewriting mechanism
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef STD_ENVIRONMENT_H
#define STD_ENVIRONMENT_H
#include "basic_environment.hpp"
#include "hashmap.hpp"

void primitive (environment& env, hashmap<string,tree> h);
void assign (environment& env, assoc_environment local);
void begin_with (environment& env, assoc_environment local);
void end_with (environment& env);

#ifdef CLASSICAL_MACRO_EXPANSION
void macro_down (environment& env, assoc_environment local);
void macro_redown (environment& env, basic_environment local);
void macro_up (environment& env);
bool macro_top_level (environment& env);
basic_environment macro_arguments (environment& env);
#endif // CLASSICAL_MACRO_EXPANSION

#endif // defined STD_ENVIRONMENT_H
