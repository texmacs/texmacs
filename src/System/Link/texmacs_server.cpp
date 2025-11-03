
/******************************************************************************
* MODULE     : texmacs_server.cpp
* DESCRIPTION: TeXmacs servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
*                  2022  Gregoire Lecerf
*                  2025  Robin Wils
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_link.hpp"
#include "client_server.hpp"
#include "server_log.hpp"
#include "scheme.hpp"

#ifdef QTTEXMACS
#include "Qt/QTMSockets.hpp"

/******************************************************************************
* Server mode?
******************************************************************************/

static bool server_mode= false;

// CLI arg port override
static int server_port= -1;

bool
is_server () { return server_mode; }

void
set_server () { server_mode= true; }

void
unset_server () { server_mode= false; }

int
get_server_port () {
  if (server_port != -1) {
    return server_port;
  }

  string _port= get_preference ("server port");
  return is_int (_port) ? as_int (_port) : 6561;
}

void
set_server_port (int port) { server_port= port; }

/******************************************************************************
* Server currently active?
******************************************************************************/

static socket_server_rep* net_server= NULL;

int
server_port_in_use () {
  if (net_server == NULL) return 0;
  return net_server->port;
}

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  if (net_server != NULL) {
    io_warning << "Cannot start server because it is already running." << LF;
    return;
  }
  server_log_start ();
  (void) eval ("(use-modules (server server-base))");
  (void) eval ("(use-modules (server server-tmfs))");
  (void) eval ("(use-modules (server server-menu))");
  (void) eval ("(use-modules (server server-live))");
  net_server= tm_new<socket_server_rep> ("", get_server_port ());
  string status= net_server->start ();
  if (status != "") {
    io_error << "cannot start the server: " << status << LF;
    io_error << "see details in the server log file." << LF;
    net_server->stop ();
    tm_delete (net_server);
    net_server= NULL;
  }
}

void
server_stop () {
  if (net_server == NULL) {
    io_warning << "cannot stop server because it isn't running." << LF;
    return;
  }
  net_server->stop ();
  tm_delete (net_server);
  net_server= NULL;
  server_log_stop ();
}

bool
server_started () {
  return net_server != NULL;
}

string
server_read (int fd) {
  if (net_server == NULL) return "";
  string s (net_server->read (fd));
  return s;
}

void
server_write (int fd, string s) {
  if (net_server == NULL) return;
  DEBUG_SOCKET_DATA ("server output:", s);
  net_server->write (fd, s);
}

int
number_of_servers () {
  return net_server == NULL ? 0 : net_server->number_of_connections ();
}

string
server_client_address (int fd) {
  if (net_server == NULL)
    return "";
  if (!net_server->address_from_id->contains (fd))
    return "";
  return net_server->address_from_id (fd);
}

void
server_listen_connections (int msecs) {
  if (net_server != NULL)
    net_server->listen_connections (msecs);
}

#else

/******************************************************************************
* Server mode?
******************************************************************************/

bool
is_server () { return false; }

void
set_server () {}

void
unset_server () {}

int
server_port_in_use () { return 0; }

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  io_error << "server is not implemented.";
}

void
server_stop () {
  io_error << "server is not implemented.";
}

bool
server_started () {
  return false;
}

string
server_read (int fd) {
  (void) fd;
  io_error << "server is not implemented.";
  return "";
}

void
server_write (int fd, string s) {
  io_error << "server is not implemented.";
  (void) fd; (void) s;
}

int
number_of_servers () {
  return 0;
}

void
server_listen_connections (int msecs) {}

#endif
