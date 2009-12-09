
/******************************************************************************
* MODULE     : tm_link.hpp
* DESCRIPTION: Links between TeXmacs and extern programs
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_LINK_H
#define TM_LINK_H
#include "tree.hpp"
#include "command.hpp"

#define CONNECTION_DEAD    0
#define CONNECTION_DYING   1
#define WAITING_FOR_INPUT  2
#define WAITING_FOR_OUTPUT 3

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_COMMAND ((char) 16)
#define DATA_ESCAPE  ((char) 27)

#define LINK_IN   0
#define LINK_OUT  0
#define LINK_ERR  1

#define SOCKET_DEFAULT  0
#define SOCKET_CLIENT   1
#define SOCKET_SERVER   2

/******************************************************************************
* The tm_link class
******************************************************************************/

struct tm_link_rep: abstract_struct {
  bool   alive;   // link is alive
  string secret;  // empty string or secret key for encrypted connections

  command feed_cmd; // called when async data available
  
public:
  inline tm_link_rep () {}
  inline virtual ~tm_link_rep () {}

  virtual string  start () = 0;
  virtual void    write (string s, int channel) = 0;
  virtual string& watch (int channel) = 0;
  virtual string  read (int channel) = 0;
  virtual void    listen (int msecs) = 0;
  virtual void    interrupt () = 0;
  virtual void    stop () = 0;

  void write_packet (string s, int channel);
  bool complete_packet (int channel);
  string read_packet (int channel, int timeout, bool& success);
  void secure_server (string cmd);
  void secure_client ();

  void set_command (command _cmd) { feed_cmd = _cmd; }
  
  friend class tm_link;
};

class tm_link {
public:
  ABSTRACT_NULL(tm_link);
  inline bool operator == (tm_link l);
  inline bool operator != (tm_link l);
};

ABSTRACT_NULL_CODE(tm_link);
inline bool tm_link::operator == (tm_link l) { return rep == l.rep; }
inline bool tm_link::operator != (tm_link l) { return rep != l.rep; }

tm_link make_pipe_link (string cmd);
tm_link make_dynamic_link (string lib, string symb, string init, string ses);
tm_link make_socket_link (string h, int p, int t= SOCKET_DEFAULT, int fd= -1);
tm_link make_socket_server (int port);
tm_link find_socket_link (int fd);

void close_all_pipes ();

#endif // TM_LINK_H
