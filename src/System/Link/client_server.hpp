
/******************************************************************************
* MODULE     : client_server.hpp
* DESCRIPTION: TeXmacs clients and servers
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*                  2022  Grégoire Lecerf
*                  2025  Robin Wils
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CLIENT_SERVER_H
#define CLIENT_SERVER_H
#include "string.hpp"
#include "scheme.hpp"
#include "server_log.hpp"
#include "gnutls.hpp"

// Error codes
const int TM_NET_SUCCESS = 0;
const int TM_NET_WRONG_ARGUMENTS = -1;
const int TM_NET_INVALID_HOST = -10;
const int TM_NET_INVALID_PORT = -11;
const int TM_NET_INTERNAL_ERROR = -20;
const int TM_NET_CONTACT_DEAD = -60;
const int TM_NET_WRONG_PROTOCOL = -70;
const int TM_NET_NO_GNUTLS = -100;
const int TM_NET_CONNECTION_FAILED = -120;
const int TM_NET_SESSION_INACTIVE = -1024;

// TeXmacs server and client

void   server_define_error_codes ();
void   server_start ();
void   server_stop ();
string server_read (int fd);
void   server_write (int fd, string s);
bool   server_started ();
void   server_listen_connections (int msecs);

int        legacy_client_start (string host, int port);
inline int legacy_client_start (string host) {
  return legacy_client_start(host, 6561);
}

int    tls_client_start (string host, int port, scheme_tree args);
void   client_stop (int fd);
string client_read (int fd);
int    client_write (int fd, string s);
string server_client_address (int fd);
void   client_listen_connections (int msecs);

void   enter_secure_mode (int fd);

// Tells if TeXmacs is running a server
bool is_server ();
void set_server ();
void unset_server ();
int get_server_port ();
void set_server_port (int port);
int  server_port_in_use ();

bool should_reset_preferences ();
bool should_reset_admin_password ();
void set_reset_preferences (bool value);
void set_reset_admin_password (bool value);

#endif // defined CLIENT_SERVER_H
