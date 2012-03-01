
/******************************************************************************
* MODULE     : texmacs_client.cpp
* DESCRIPTION: clients of TeXmacs servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "client_server.hpp"
#include "socket_server.hpp"
#include "socket_link.hpp"
#include "scheme.hpp"

static socket_link_rep* the_client= NULL;

/******************************************************************************
* Client side
******************************************************************************/

void
client_start (string host) {
  if (the_client == NULL) {
    (void) eval ("(use-modules (remote texmacs-client))");
    the_client= tm_new<socket_link_rep> (host, 6561, SOCKET_CLIENT, -1);
  }
  if (!the_client->alive)
    cout << "TeXmacs] Starting client... " << the_client->start () << "\n";
}

void
client_stop () {
  if (the_client != NULL) {
    the_client->stop ();
    tm_delete (the_client);
    the_client= NULL;
  }
}

string
client_read () {
  if (the_client == NULL || !the_client->alive) return "";
  if (!the_client->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= the_client->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
client_write (string s) {
  if (the_client == NULL || !the_client->alive) return;
  //cout << "Client write " << s << "\n";
  the_client->write_packet (s, LINK_IN);
}

void
enter_secure_mode () {
  the_client->secure_client ();
}
