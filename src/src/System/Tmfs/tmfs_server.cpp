
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

static socket_server_rep* tmfs_server= NULL;
static hashmap<int,string> server_reading ("");

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

static bool
message_complete (string s) {
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return false;
  return (n - (i+1)) >= as_int (s (0, i));
}

static string
message_receive (string& s) {
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return "";
  int l= as_int (s (0, i++));
  string r= s (i, i+l);
  s= s (i+l, n);
  return r;
}

string
tmfs_server_read (int fd) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return "";
  if (!server_reading->contains (fd)) server_reading (fd)= "";
  string& r= server_reading (fd);
  r << ln -> read (LINK_OUT);
  if (!message_complete (r)) return "";
  string back= message_receive (r);
  //cout << "Server read " << back << "\n";
  return back;
}

void
tmfs_server_write (int fd, string s) {
  tm_link ln= find_socket_link (fd);
  if (nil (ln)) return;
  //cout << "Server write " << s << "\n";
  ln->write ((as_string (N(s)) * "\n") * s, LINK_IN);
}

static socket_link_rep* tmfs_client= NULL;
static string client_reading= "";

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
  client_reading << tmfs_client -> read (LINK_OUT);
  if (!message_complete (client_reading)) return "";
  string back= message_receive (client_reading);
  //cout << "Server read " << back << "\n";
  return back;
}

void
tmfs_client_write (string s) {
  if (tmfs_client == NULL || !tmfs_client->alive) return;
  //cout << "Client write " << s << "\n";
  tmfs_client->write ((as_string (N(s)) * "\n") * s, LINK_IN);
}
