
/******************************************************************************
* MODULE     : texmacs_client.cpp
* DESCRIPTION: clients of TeXmacs servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*                  2022  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "client_server.hpp"
#include "scheme.hpp"
#include "boot.hpp"
#include "iterator.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include <thread>
#include <atomic>
#include <cstring>

#ifdef QTTEXMACS
#include "Qt/QTMSockets.hpp"

#define CLT_KO(c) (c == NULL || !c->alive)

static hashset<pointer> the_clients;
static hashmap<int,pointer> client_from_fd; 
static bool clients_started= false;

// v1: tree_cache
constexpr int TM_PROTOCOL_VERSION = 1;

/******************************************************************************
* Utilities
******************************************************************************/

static bool
is_tuple_string (scheme_tree o) {
  if (!is_tuple (o)) return false;
  for (int i= 0; i < N(o); i++)
    if (!is_string (o[i])) return false;
  return true;
}

static array<string>
as_array_string (scheme_tree o) {
  ASSERT (is_tuple_string (o), "wrong argument");
  array<string> ret;
  for (int i= 0; i < N(o); i++)
    ret << scm_unquote (as_string (o[i]));
  return ret;
}

static bool
is_tuple_tuple_string (scheme_tree o) {
  if (!is_tuple (o)) return false;
  for (int i= 0; i < N(o); i++)
    if (!is_tuple_string (o[i])) return false;
  return true;
}

static array<array<string> >
as_array_array_string (scheme_tree o) {
  ASSERT (is_tuple_tuple_string (o), "wrong argument");
  array<array<string> > ret;
  for (int i= 0; i < N(o); i++)
    ret << as_array_string (o[i]);
  return ret;
}

/******************************************************************************
* Client side
******************************************************************************/

static int
_client_start (string host, int port, tm_contact contact) {
  if (port < 0 || port > 65535) {
    io_error << "invalid port number " << port << "\n";
    return TM_NET_INVALID_PORT;
  }
  if (!clients_started) {
    (void) eval ("(use-modules (client client-base))");
    clients_started= true;
  }
  socket_link_rep* client=
    tm_new<socket_link_rep> (host, port, contact);
  if (client == NULL)
    return TM_NET_INTERNAL_ERROR;
  string status= client->start ();
   if (DEBUG_IO)
     debug_io << "client started, its status is now '" << status << "'\n";
  if (status == "" && client->alive) {
    int fd= client->get_socket_id ();
    the_clients->insert (client);
    client_from_fd(fd)= client;
    return fd;
  }
  client->stop ();
  if (status == "contact has not started")
    return TM_NET_CONTACT_DEAD;
  tm_delete (client);
  return TM_NET_CONNECTION_FAILED;
}

int
legacy_client_start (string host, int port) {
  return _client_start (host, port, make_socket_client_contact ());
}

int
tls_client_start (string host, int port, scheme_tree args) {
  if (!gnutls_present ())
    return TM_NET_NO_GNUTLS;
  if (!is_tuple_tuple_string (args)) {
    if (DEBUG_IO)
      debug_io << "wrong arguments in 'tls_client_start': " << args << "\n";
    return TM_NET_WRONG_ARGUMENTS;
  }
  //cout << "tls_client_start, " << args << LF;
  return _client_start (host, port,
    make_tls_client_contact (host, as_array_array_string (args),
        is_tls_no_verify ()));
}

static socket_link_rep*
find_client (int fd) {
  if (client_from_fd->contains (fd))
    return (socket_link_rep*) client_from_fd[fd];
  return (socket_link_rep*) NULL;
}

void
client_stop (int fd) {
  //cout << "--> client_stop, " << fd << LF;
  //cout << client_from_fd << LF;
  //cout << the_clients << LF;
  if (!clients_started) {
    (void) eval ("(use-modules (client client-base))");
    clients_started= true;
  }
  call ("client-remove", object (fd));
  socket_link_rep *client= find_client (fd);
  if (client) {
    client_from_fd->reset (fd);
    if (the_clients->contains (client))
      the_clients->remove (client);
    client->stop ();
    tm_delete (client);
  }
}

string
client_read (int fd) {
  socket_link_rep *client= find_client (fd);
  if (CLT_KO (client)) return "";
  if (!client->complete_packet (LINK_OUT)) return "";
  bool success;
  string back= client->read_packet (LINK_OUT, 0, success);
  DEBUG_SOCKET_DATA ("Client in: ", back);
  return back;
}

int
client_write (int fd, string s) {
  socket_link_rep *client= find_client (fd);
  if (CLT_KO (client)) return -1;
  DEBUG_SOCKET_DATA ("client output: ", s);
  client->write_packet (s, LINK_IN);
  if (client->alive)
    return TM_NET_SUCCESS;
  return -1;
}

void
enter_secure_mode (int fd) {
  socket_link_rep *client= find_client (fd);
  if (client == NULL || !client->alive) return;
  client->secure_client ();
}

void
client_listen_connections (int msecs) {
  //cout << "client_listen_connections " << N(the_clients) << "\n";
  iterator<pointer> it= iterate (the_clients);
  while (it->busy ()) {
    socket_link_rep* c= (socket_link_rep*) it->next ();
    if (c != NULL)
      c->listen (msecs);
  }
}

int client_protocol_version () {
  return TM_PROTOCOL_VERSION;
}

static std::thread checker_thread;
static std::atomic<bool> checker_running (false);

void
tm_check_online (scheme_tree endpoints, int timeout_ms) {
  tm_addr addrs[64];
  int n= 0;
  for (int i= 0; i < N(endpoints) && n < 64; i++) {
    if (!is_tuple (endpoints[i]) || N(endpoints[i]) < 2) continue;
    if (!is_string (endpoints[i][0])) continue;
    if (!is_string (endpoints[i][1])) continue;
    string host= scm_unquote (as_string (endpoints[i][0]));
    string port= scm_unquote (as_string (endpoints[i][1]));
    c_string c_host (host);
    c_string c_port (port);
    snprintf (addrs[n].host, sizeof (addrs[n].host), "%s", (char*) c_host);
    snprintf (addrs[n].port, sizeof (addrs[n].port), "%s", (char*) c_port);
    n++;
  }
  if (n > 0)
    check_online (addrs, n, timeout_ms);
}

class network_availability_checker {
public:
  tm_addr addrs[64];
  bool reachable[64];
  int n;
  int timeout_ms;

  network_availability_checker () : n (0), timeout_ms (5000) {
    memset (reachable, 0, sizeof (reachable));
  }

  void operator() () {
    for (int i= 0; i < n; i++) {
      char errbuf[256];
      int fd= try_connect (addrs[i].host, addrs[i].port,
          timeout_ms, errbuf, sizeof (errbuf));
      reachable[i]= (fd >= 0);
      if (fd >= 0) {
        cout << "got reachable for " << addrs[i].host << ":" << addrs[i].port <<" number " << i << ", " << n << " addrs" << LF;
#ifdef OS_MINGW
        using namespace wsoc;
        wsoc::closesocket (fd);
#else
        close (fd);
#endif
      }
    }
    checker_running.store (false);
  }
};

static network_availability_checker checker;

scheme_tree
tm_get_online_status () {
  if (checker_running.load ()) return tree (TUPLE);
  if (checker_thread.joinable ())
    checker_thread.join ();
  tree result (TUPLE);
  for (int i= 0; i < checker.n; i++) {
    if (checker.reachable[i]) {
      tree entry (TUPLE);
      tree key (TUPLE);
      key << scm_quote (checker.addrs[i].host)
        << scm_quote (checker.addrs[i].port);
      entry << key
        <<  (checker.reachable[i] ? string ("#t") : string ("#f"));
      result << entry;
    }
  }
  return result;
}

void
check_online (tm_addr* addrs, int addr_len, int timeout_ms) {
  if (checker_running.load ()) return;
  if (addr_len <= 0 || addr_len > 64) return;

  if (checker_thread.joinable ())
    checker_thread.join ();

  checker.n= addr_len > 64 ? 64 : addr_len;
  checker.timeout_ms= timeout_ms;
  memcpy (checker.addrs, addrs, checker.n * sizeof (tm_addr));

  checker_running.store (true);
  checker_thread= std::thread (std::ref (checker));
}

#else // Non QT part

int
client_start (string host) {
  io_error << "sockets are not implemented";
  return -1;
}

int
tls_client_start (string host, scheme_tree args) {
  io_error << "sockets are not implemented";
  return -1;
}
void
client_stop (int fd) {
  io_error << "sockets are not implemented";
}

string
client_read (int fd) {
  io_error << "sockets are not implemented";
  return "";
}

int
client_write (int fd, string s) {
  io_error << "sockets are not implemented";
  return -1;
}

void
enter_secure_mode (int fd) {
  io_error << "sockets are not implemented";
}

void
client_listen_connections (int msecs) {
  io_error << "sockets are not implemented";
}
#endif
