
/******************************************************************************
* MODULE     : texmacs_server.cpp
* DESCRIPTION: TeXmacs servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_link.hpp"
#include "client_server.hpp"
#include "socket_server.hpp"
#include "scheme.hpp"

static socket_server_rep* the_server= NULL;

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  if (the_server == NULL) {
    (void) eval ("(use-modules (remote texmacs-server))");
    the_server= tm_new<socket_server_rep> (6561);
  }
  if (!the_server->alive)
    cout << "TeXmacs] Starting server... " << the_server->start () << "\n";
}

void
server_stop () {
  if (the_server != NULL) {
    tm_delete (the_server);
    the_server= NULL;
  }
}

string
server_read (int fd) {
  tm_link ln= find_socket_link (fd);
  if (is_nil (ln)) return "";
  if (!ln->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= ln->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
server_write (int fd, string s) {
  tm_link ln= find_socket_link (fd);
  if (is_nil (ln)) return;
  //cout << "Server write " << s << "\n";
  ln->write_packet (s, LINK_IN);
}
