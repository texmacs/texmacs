
/******************************************************************************
* MODULE     : socket_contact.cpp
* DESCRIPTION: TeXmacs contacts for sockets
* COPYRIGHT  : (C) 2022  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "socket_contact.hpp"
#ifndef __MINGW32__
#include <unistd.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#define WRITE(a, b, c) ::send(a, b, c, 0) 
#define RECV ::recv
#else
#include <stdio.h>
#include <string.h>
#include <errno.h>
namespace wsoc {
#include <sys/types.h>
#include <ws2tcpip.h>
#define WRITE(a, b, c) wsoc::send(a, b, c, 0) 
#define RECV wsoc::recv
}
#endif

/******************************************************************************
* The socket contact class for clients
******************************************************************************/

int
socket_client_contact_rep::send (const void* buffer, size_t length) {
  int r= WRITE(io, (const char*) buffer, length);
  error_number= r < 0 ? errno : 0;
  if (r < 0 &&
      error_number != EAGAIN &&
      error_number != EINTR &&
      error_number != ENOBUFS) stop ();
  return r;
}

int
socket_client_contact_rep::receive (void* buffer, size_t length) {
  int r= RECV(io, (char*) buffer, length, 0);
  error_number= r < 0 ? errno : 0;
  if (r < 0 &&
      error_number != EAGAIN &&
      error_number != EINTR) stop ();
  return r;
}

string
socket_client_contact_rep::last_error () {
  if (error_number == 0) return string ("");
  const char* msg= strerror (error_number);
  if (msg == NULL) return "unknown error";
  return string (msg);
}

/******************************************************************************
* The socket contact class for servers
******************************************************************************/

int
socket_server_contact_rep::send (const void* buffer, size_t length) {
  int r= WRITE(io, (const char*) buffer, length);
  error_number= r < 0 ? errno : 0;
  if (r < 0 &&
      error_number != EAGAIN &&
      error_number != EINTR &&
      error_number != ENOBUFS) stop ();
  return r;
}

int
socket_server_contact_rep::receive (void* buffer, size_t length) {
  int r= RECV(io, (char*) buffer, length, 0);
  error_number= r < 0 ? errno : 0;
  if (r < 0 &&
      error_number != EAGAIN &&
      error_number != EINTR) stop ();
  return r;
}

string
socket_server_contact_rep::last_error () {
  if (error_number == 0) return string ("");
  const char* msg= strerror (error_number);
  if (msg == NULL) return "unknown error";
  return string (msg);
}
