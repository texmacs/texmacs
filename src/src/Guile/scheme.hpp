
/******************************************************************************
* MODULE     : scheme.hpp
* DESCRIPTION: Abstract interface for the manipulation of scheme objects
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef SCHEME_HH
#define SCHEME_HH

// Inclusions from "Glue/glue.hpp" and "Scheme/evaluate.hpp"
void initialize_glue ();
void install_guile (int argc, char** argv, void (*call_back) (int, char**));
void initialize_guile ();
// End inclusions

#include "Scheme/object.hpp"

#endif // defined SCHEME_HH
