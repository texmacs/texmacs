
/******************************************************************************
* MODULE     : command.hpp
* DESCRIPTION: Abstract dynamic commands
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef COMMAND_H
#define COMMAND_H
#include "tree.hpp"

extern int command_count;
class command_rep: public abstract_struct {
public:
  inline command_rep () { DEBUG(command_count++); }
  inline virtual ~command_rep () { DEBUG(command_count--); }
  inline virtual ostream& print (ostream& out);
  virtual void apply () = 0;
};

class command {
public:
  ABSTRACT_NULL(command);
  command (void (*routine) (void));
  inline void   operator () (void);
  inline friend ostream& operator << (ostream& out, command cmd);
};
ABSTRACT_NULL_CODE(command);

inline ostream& command_rep::print (ostream& out) { return out << "command"; }
inline void     command::operator () (void) { rep->apply(); }
inline bool     operator == (command cmd1, command cmd2) {
  return cmd1.rep == cmd2.rep; }
inline ostream& operator << (ostream& out, command cmd) {
  if (nil(cmd)) return out << "(null)"; else return cmd->print(out); }

#endif // defined COMMAND_H
