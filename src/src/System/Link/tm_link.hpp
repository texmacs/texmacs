
/******************************************************************************
* MODULE     : tm_link.cpp
* DESCRIPTION: Links between TeXmacs and extern programs
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_LINK_H
#define TM_LINK_H
#include "tree.hpp"

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

/******************************************************************************
* The tm_link class
******************************************************************************/

struct tm_link_rep: abstract_struct {
  bool alive; // link is alive

public:
  inline tm_link_rep () {}
  inline virtual ~tm_link_rep () {}

  virtual string start () = 0;
  virtual void   write (string s, int channel) = 0;
  virtual string read (int channel) = 0;
  virtual void   listen (int msecs) = 0;
  virtual void   interrupt () = 0;
  virtual void   stop () = 0;

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
tm_link make_socket_link (string host, int port);

#endif // TM_LINK_H
