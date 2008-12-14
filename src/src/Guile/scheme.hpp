
/******************************************************************************
* MODULE     : scheme.hpp
* DESCRIPTION: Abstract interface for the manipulation of scheme objects
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SCHEME_HH
#define SCHEME_HH

// Inclusions from "Glue/glue.hpp" and "Scheme/evaluate.hpp"
void initialize_glue ();
void start_guile (int argc, char** argv, void (*call_back) (int, char**));
void initialize_guile ();
// End inclusions

#include "Scheme/object.hpp"

#endif // defined SCHEME_HH
