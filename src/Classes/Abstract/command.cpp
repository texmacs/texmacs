
/******************************************************************************
* MODULE     : command.cpp
* DESCRIPTION: Abstract dynamic commands
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  rep (new std_command_rep (routine)) { rep->ref_count++; }
