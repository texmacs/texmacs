
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

#ifdef QTTEXMACS
#include "Qt/QTMSockets.hpp"

#if defined(OS_MACOS)
  #include "MacOS/mac_utilities.h"
#endif

static socket_server* the_server= NULL;
//int socket_link::id= 0;

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  if (the_server == NULL) {
    (void) eval ("(use-modules (server server-base))");
    (void) eval ("(use-modules (server server-tmfs))");
    (void) eval ("(use-modules (server server-menu))");
    (void) eval ("(use-modules (server server-live))");
    the_server= tm_new<socket_server> (INADDR_ANY,6561);
  }
  if (the_server->alive ())
    cout << "TeXmacs] Server started... \n";
#if defined(OS_MACOS)
  mac_begin_server ();
#endif
}

void
server_stop () {
#if defined(OS_MACOS)
  mac_end_server ();
#endif
  if (the_server != NULL) {
    tm_delete (the_server);
    the_server= NULL;
  }
}

bool
server_started () {
  return (the_server != NULL && the_server->alive ());
}

string
server_read (int fd) {
  string s(the_server->read(fd));
  DBG_IOS ("in:", s);
  return s;
}

void
server_write (int fd, string s) {
  DBG_IOS ("out:", s);
  the_server->write (fd,s);
}

int
number_of_servers () {
  return the_server?the_server->srv_count():0;
}


#else

static socket_server_rep* the_server= NULL;

/******************************************************************************
* Server side
******************************************************************************/

void
server_start () {
  if (the_server == NULL) {
    (void) eval ("(use-modules (server server-base))");
    (void) eval ("(use-modules (server server-tmfs))");
    (void) eval ("(use-modules (server server-menu))");
    (void) eval ("(use-modules (server server-live))");
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

bool
server_started () {
  return the_server != NULL;
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
#endif
