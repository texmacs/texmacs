
/******************************************************************************
* MODULE     : command.cpp
* DESCRIPTION: Abstract dynamic commands
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "command.hpp"

/******************************************************************************
* standard commands without arguments
******************************************************************************/

class std_command_rep: public command_rep {
  void (*routine) (void);
public:
  std_command_rep (void (*routine) (void));
  void apply ();
};

std_command_rep::std_command_rep (void (*routine2) (void)):
  routine (routine2) {}
void std_command_rep::apply () { routine (); }

command::command (void (*routine) (void)):
  rep (tm_new<std_command_rep> (routine)) { rep->ref_count++; }
