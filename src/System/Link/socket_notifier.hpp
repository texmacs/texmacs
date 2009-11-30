
/******************************************************************************
* MODULE     : socket_notifier.hpp
* DESCRIPTION: Notifiers for socket activity
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SOCKET_NOTIFIER_H
#define SOCKET_NOTIFIER_H

#include "basic.hpp"
#include "hashset.hpp"
#include "command.hpp"

struct socket_notifier_rep: concrete_struct {
  int fd; // file descriptor for the socket
  command cmd;
  
public:
  socket_notifier_rep (int _fd, command _cmd):
    fd (_fd), cmd (_cmd) {}
  void notify () { if (!is_nil (cmd)) cmd->apply (); } 
};

class socket_notifier {
CONCRETE_NULL(socket_notifier);
  inline socket_notifier (int _fd, void (*_cb) (void*, void*),
			  void *_obj, void *_info = NULL):
    rep (tm_new<socket_notifier_rep> (_fd, command (_cb, _obj, _info))) {}
  friend bool operator == (socket_notifier sn1, socket_notifier sn2) {
    return (sn1.rep == sn2.rep); }
  friend int hash (socket_notifier sn) {
    return hash (sn.rep); }
};
CONCRETE_NULL_CODE(socket_notifier);

inline ostream& operator << (ostream& out, socket_notifier sn) {
  if (is_nil (sn)) return out << "nil socket_notifier";
else return out << "some socket_notifier"; }


void perform_select ();
void add_notifier (socket_notifier);
void remove_notifier (socket_notifier);

#endif // SOCKET_NOTIFIER_H
