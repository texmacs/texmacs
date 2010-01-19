
/******************************************************************************
* MODULE     : socket_server.cpp
* DESCRIPTION: TeXmacs servers over sockets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "socket_server.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include <stdlib.h>
#include <string.h>
#ifndef __MINGW32__
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#endif

hashset<pointer> socket_server_set;
void socket_server_callback (void *obj, void *info);

/******************************************************************************
* Constructors and destructors for socket_servers
******************************************************************************/

socket_server_rep::socket_server_rep (int port2):
 port (port2), sn ()
{
  socket_server_set->insert ((pointer) this);
  server= -1;
  alive = false;
}

socket_server_rep::~socket_server_rep () {
  stop ();
  socket_server_set->remove ((pointer) this);
}

tm_link
make_socket_server (int port) {
  return tm_new<socket_server_rep> (port);
}

int
number_of_servers () {
  return N (socket_server_set);
}

void
close_all_servers () {
#ifndef __MINGW32__
  iterator<pointer> it= iterate (socket_server_set);
  while (it->busy()) {
    socket_server_rep* ss= (socket_server_rep*) it->next();
    if (ss->alive) {
      // FIXME: cleanly close the connection to the socket here
      ss->alive= false;
    }
  }
#endif
}

/******************************************************************************
* Routines for socket_servers
******************************************************************************/

string
socket_server_rep::start () {
#ifndef __MINGW32__
  // get the server
  if ((server = socket (PF_INET, SOCK_STREAM, 0)) == -1)
    return "Error: call to 'socket' failed";

  // lose the pesky "address already in use" error message
  int yes= 1;
  if (setsockopt (server, SOL_SOCKET, SO_REUSEADDR,
		  &yes, sizeof (int)) == -1)
    return "Error: call to 'setsockopt' failed";

  // bind
  struct sockaddr_in local_address;
  local_address.sin_family = AF_INET;
  local_address.sin_addr.s_addr = INADDR_ANY;
  local_address.sin_port = htons (6561);
  memset (local_address.sin_zero, '\0', sizeof local_address.sin_zero);
  if (bind (server, (struct sockaddr *) &local_address,
	    sizeof (local_address)) == -1)
    return "Error: call to 'bind' failed";

  // listen
  if (::listen (server, 10) == -1)
    return "Error: call to 'listen' failed";

  alive= true;
  
  sn = socket_notifier (server, &socket_server_callback, this, NULL);
  add_notifier (sn);
  
  return "ok";
#else
  return "Error: sockets not implemented";
#endif
}

void
socket_server_rep::start_client () {
#ifndef __MINGW32__
  struct sockaddr_in remote_address;
  socklen_t addrlen= sizeof (remote_address);
  int client= accept (server, (struct sockaddr *) &remote_address, &addrlen);
  if (client == -1) system_warning ("Call to 'accept' failed");
  else {
    string addr= inet_ntoa (remote_address.sin_addr);
    cout << "TeXmacs] opened connection from '" << addr << "'\n";
    array<tm_link> update;
    for (int i=0; i<N(incoming); i++)
      if (incoming[i]->alive)
	update << incoming[i];
    incoming= update;
    incoming << make_socket_link (addr, -1, SOCKET_SERVER, client);
  }
#endif
}

void
socket_server_rep::write (string s, int channel) {
  (void) s; (void) channel;
}

string&
socket_server_rep::watch (int channel) {
  static string empty_string= "";
  (void) channel; return empty_string;
}

string
socket_server_rep::read (int channel) {
  (void) channel; return "";
}

void
socket_server_rep::listen (int msecs) {
  (void) msecs;
}

void
socket_server_rep::interrupt () {
}

void
socket_server_rep::stop () {
#ifndef __MINGW32__
  // FIXME: close children
  if (!alive) return;
  incoming= array<tm_link> ();
  alive= false;
  
  remove_notifier (sn);
  close (server);
  wait (NULL);
#endif
}

/******************************************************************************
* Call back for new information on server
******************************************************************************/

void 
socket_server_callback (void *obj, void *info) {
#ifndef __MINGW32__
  (void) info;
  socket_server_rep* ss = (socket_server_rep*) obj;
  bool busy= true;
  bool news= false;
  while (busy) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= ss->server + 1;
    FD_SET (ss->server, &rfds);
  
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    select (max_fd, &rfds, NULL, NULL, &tv);

    busy= false;
    if (ss->alive && FD_ISSET (ss->server, &rfds)) {
      //cout << "server_callback" << LF;
      ss->start_client ();
      busy= news= true;
    }
  }
  
  if (!is_nil (ss->feed_cmd) && news)
    ss->feed_cmd->apply (); // call the data processor
#endif
}
