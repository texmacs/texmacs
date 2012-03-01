
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
#include "scheme.hpp"

/******************************************************************************
* default implementation of command application with arguments
******************************************************************************/

void
command_rep::apply (object args) {
  (void) args;
  apply ();
}

void
command::operator () (object args) {
  rep->apply (args);
}

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

command::command (void (*routine) (void)) :
  rep (tm_new<std_command_rep> (routine)) { INC_COUNT(rep); }


/******************************************************************************
* callbacks
******************************************************************************/

class generic_command_rep: public command_rep {
  void (*callback) (void*, void*); // callback
  void *obj; // argument for callback
  void *info; // additional info
  
public:
  generic_command_rep (void (*_callback) (void*, void*), void *_obj, void *_info) 
    : callback (_callback), obj (_obj), info (_info) {}
  void apply () { if (callback) callback (obj, info); }
  tm_ostream& print (tm_ostream& out) { return out << "generic_command_rep"; }
};

command::command (void (*_callback) (void*, void*), void *_obj, void *_info) :
  rep (tm_new<generic_command_rep> (_callback, _obj, _info)) { INC_COUNT(rep); }


