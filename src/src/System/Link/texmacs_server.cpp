
/******************************************************************************
* MODULE     : texmacs_server.cpp
* DESCRIPTION: TeXmacs servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "client_server.hpp"
#include "socket_server.hpp"
#include "socket_link.hpp"
#include "Scheme/object.hpp"

static socket_server_rep* the_server= NULL;

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  if (the_server == NULL) {
    (void) eval ("(use-modules (remote texmacs-server))");
    (void) eval ("(use-modules (remote texmacs-client))");
    the_server= new socket_server_rep (6561);
  }
  if (!the_server->alive)
    cout << "TeXmacs] Starting server... " << the_server->start () << "\n";
}

void
server_stop () {
  if (the_server != NULL) {
    delete the_server;
    the_server= NULL;
  }
}

string
server_read (int fd) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return "";
  if (!ln->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= ln->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
server_write (int fd, string s) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return;
  //cout << "Server write " << s << "\n";
  ln->write_packet (s, LINK_IN);
}
