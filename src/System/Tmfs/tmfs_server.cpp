
/******************************************************************************
* MODULE     : tmfs_server.cpp
* DESCRIPTION: remote acces to TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "socket_server.hpp"
#include "socket_link.hpp"
#include "Scheme/object.hpp"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/******************************************************************************
* Server side
******************************************************************************/

static socket_server_rep* tmfs_server= NULL;

void
tmfs_start_server () {
  if (tmfs_server == NULL) {
    (void) eval ("(use-modules (remote tmfs-client))");
    tmfs_server= new socket_server_rep (6561);
  }
  if (!tmfs_server->alive)
    cout << "TeXmacs] Starting server... " << tmfs_server->start () << "\n";
}

void
tmfs_stop_server () {
  if (tmfs_server != NULL) {
    delete tmfs_server;
    tmfs_server= NULL;
  }
}

string
tmfs_server_read (int fd) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return "";
  if (!ln->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= ln->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
tmfs_server_write (int fd, string s) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return;
  //cout << "Server write " << s << "\n";
  ln->write_packet (s, LINK_IN);
}

/******************************************************************************
* Client side
******************************************************************************/

static socket_link_rep* tmfs_client= NULL;

void
tmfs_start_client (string host) {
  if (tmfs_client == NULL) {
    (void) eval ("(use-modules (remote tmfs-client))");
    tmfs_client= new socket_link_rep (host, 6561, SOCKET_CLIENT, -1);
  }
  if (!tmfs_client->alive)
    cout << "TeXmacs] Starting client... " << tmfs_client->start () << "\n";
}

void
tmfs_stop_client () {
  if (tmfs_client != NULL) {
    tmfs_client->stop ();
    delete tmfs_client;
    tmfs_client= NULL;
  }
}

string
tmfs_client_read () {
  if (tmfs_client == NULL || !tmfs_client->alive) return "";
  if (!tmfs_client->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= tmfs_client->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
tmfs_client_write (string s) {
  if (tmfs_client == NULL || !tmfs_client->alive) return;
  //cout << "Client write " << s << "\n";
  tmfs_client->write_packet (s, LINK_IN);
}

void
tmfs_secure_mode () {
  tmfs_client->secure_client ();
}
