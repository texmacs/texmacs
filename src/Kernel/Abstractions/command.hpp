
/******************************************************************************
* MODULE     : command.hpp
* DESCRIPTION: Abstract dynamic commands
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef COMMAND_H
#define COMMAND_H
#include "tree.hpp"

extern int command_count;
class command_rep: public abstract_struct {
public:
  inline command_rep () { TM_DEBUG(command_count++); }
  inline virtual ~command_rep () { TM_DEBUG(command_count--); }
  inline virtual ostream& print (ostream& out);
  virtual void apply () = 0;
};

class command {
public:
  ABSTRACT_NULL(command);
  command (void (*routine) (void));
  command (void (*_callback) (void*, void*), void *_obj, void *_info = NULL);

  inline void   operator () (void);
  inline friend ostream& operator << (ostream& out, command cmd);
};
ABSTRACT_NULL_CODE(command);

inline ostream& command_rep::print (ostream& out) { return out << "command"; }
inline void     command::operator () (void) { rep->apply(); }
inline bool     operator == (command cmd1, command cmd2) {
  return cmd1.rep == cmd2.rep; }
inline ostream& operator << (ostream& out, command cmd) {
  if (is_nil(cmd)) return out << "(null)"; else return cmd->print(out); }

#endif // defined COMMAND_H
