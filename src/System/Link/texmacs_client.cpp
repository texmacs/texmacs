
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

#if (defined(QTTEXMACS) || defined(QTWKTEXMACS))
#include "Qt/QTMSockets.hpp"

#define CLT_KO(c) (c == NULL || !c->alive())

static array<socket_link*> the_clients;
static bool clients_started= false;

/******************************************************************************
* Client side
******************************************************************************/

int
client_start (string host) {
  if (!clients_started) {
    (void) eval ("(use-modules (client client-base))");
    clients_started= true;
  }
  socket_link *client= tm_new<socket_link> (host, 6561);
  if(client) {
    string err = client->start ();
    if (client->alive () && !N (err)) {
      int cltid= N (the_clients);
      DBG_IO ("Client started Id:" << cltid);
      the_clients << client;
      call ("client-add", object (cltid));
      return (cltid);
    }
    DBG_IO ("Client Error : " << err);
    tm_delete (client);
  } else DBG_IO ("Can't allocate memory for client");
    return -1;
}

void
client_stop (int fd) {
  call ("client-remove", object (fd));
  socket_link *client= the_clients[fd];
  the_clients[fd]= NULL;
  client->stop ();
  tm_delete (client);
}

string
client_read (int fd) {
  socket_link *client= the_clients[fd];
  if (CLT_KO (client)) return "";
  bool success;
  string back= client->read_packet (LINK_OUT, 0, success);
  DBG_IOS ("Client in:", back);
  return back;
}

void
client_write (int fd, string s) {
  socket_link *client= the_clients[fd];
  if (CLT_KO (client)) return;
  DBG_IOS ("Client out:", s);
  client->write_packet (s, LINK_IN);
}

void
enter_secure_mode (int fd) {
  socket_link *client= the_clients[fd];
  if (client == NULL || !client->alive ()) return;
  client->secure_client ();
}

#else // Non QT part

typedef socket_link_rep* weak_socket_link;
static array<weak_socket_link> the_clients;
static bool clients_started= false;
socket_link_rep* make_weak_socket_link (string h, int p, int t, int f);

/******************************************************************************
* Client side
******************************************************************************/

int
client_start (string host) {
  if (!clients_started) {
    (void) eval ("(use-modules (client client-base))");
    clients_started= true;
  }
  weak_socket_link client=
    make_weak_socket_link (host, 6561, SOCKET_CLIENT, -1);
  if (!client->alive)
    cout << "TeXmacs] Starting client... " << client->start () << "\n";
  if (client->alive) {
    call ("client-add", object (client->io));
    the_clients << client;
    return client->io;
  }
  else return -1;
}

void
client_stop (int fd) {
  for (int i=0; i<N(the_clients); i++)
    if (the_clients[i]->io == fd) {
      weak_socket_link client= the_clients[i];
      client->stop ();
      tm_delete (client);
      client= NULL;
      the_clients= append (range (the_clients, 0, i),
                           range (the_clients, i+1, N(the_clients)));
    }
}

static weak_socket_link
find_client (int fd) {
  for (int i=0; i<N(the_clients); i++)
    if (the_clients[i]->io == fd)
      return the_clients[i];
  return NULL;
}

string
client_read (int fd) {
  weak_socket_link client= find_client (fd);
  if (client == NULL || !client->alive) return "";
  if (!client->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= client->read_packet (LINK_OUT, 0, success);
  //cout << "Server read " << back << "\n";
  return back;
}

void
client_write (int fd, string s) {
  weak_socket_link client= find_client (fd);
  if (client == NULL || !client->alive) return;
  //cout << "Client write " << s << "\n";
  client->write_packet (s, LINK_IN);
}

void
enter_secure_mode (int fd) {
  weak_socket_link client= find_client (fd);
  if (client == NULL || !client->alive) return;
  client->secure_client ();
}
#endif
