
/******************************************************************************
* MODULE     : socket_server.cpp
* DESCRIPTION: TeXmacs servers over sockets
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#ifndef QTTEXMACS
#include "socket_server.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include <stdlib.h>
#include <string.h>
#ifndef OS_MINGW
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#ifdef __FreeBSD__
#include <sys/time.h>
#include <sys/select.h>
#endif
#else
namespace wsoc {
#include <sys/types.h>
#include <ws2tcpip.h>
}

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
iterator<pointer> it= iterate (socket_server_set);
  while (it->busy()) {
    socket_server_rep* ss= (socket_server_rep*) it->next();
    if (ss->alive) {
      // FIXME: cleanly close the connection to the socket here
      ss->alive= false;
    }
  }
}

/******************************************************************************
* Routines for socket_servers
******************************************************************************/

string
socket_server_rep::start () {
  // get the server

#ifdef OS_MINGW
  using namespace wsoc;
  {
    WSAData data;
    int ret;
    ret=WSAStartup(MAKEWORD(2,0), &data);
    if(ret) {
      char buf[32];
      string str="Error: WSAStartup failed code:";
      str << itoa(ret,buf,10);
      return(str);
    }
  }
#endif
  if ((server = socket (PF_INET, SOCK_STREAM, 0)) == -1) {
    string ret="Error: call to 'socket' failed";
#ifdef OS_MINGW
    char buf[32];
    ret <<" code:"<<itoa(WSAGetLastError(),buf,10);
#endif
    return ret;
  }

  // lose the pesky "address already in use" error message
  int yes= 1;
#ifndef OS_MINGW
  if (setsockopt (server, SOL_SOCKET, SO_REUSEADDR,
    &yes, sizeof (int)) == -1)
#else
  if (setsockopt (server, SOL_SOCKET, SO_REUSEADDR,
    (const char*) &yes, sizeof (int)) == -1)
#endif
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
#ifdef OS_MINGW
  if (wsoc::listen (server, 10) == -1)
#else
  if (::listen (server, 10) == -1)
#endif
    return "Error: call to 'listen' failed";

  alive= true;
  
  sn = socket_notifier (server, &socket_server_callback, this, NULL);
  add_notifier (sn);
  
  return "ok";

}

void
socket_server_rep::start_client () {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  struct sockaddr_in remote_address;
  socklen_t addrlen= sizeof (remote_address);
  int client= accept (server, (struct sockaddr *) &remote_address, &addrlen);
  if (client == -1) io_warning << "Call to 'accept' failed\n";
  else {
    string addr= inet_ntoa (remote_address.sin_addr);
    debug_io << "Opened connection from '" << addr << "'\n";
    array<tm_link> update;
    for (int i=0; i<N(incoming); i++)
      if (incoming[i]->alive)
        update << incoming[i];
    incoming= update;
    tm_link new_ln= make_socket_link (addr, -1, SOCKET_SERVER, client);
    incoming << new_ln;
  }
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
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  // FIXME: close children
  if (!alive) return;
  incoming= array<tm_link> ();
  alive= false;
  remove_notifier (sn);
  sn= socket_notifier ();
#ifdef OS_MINGW
  closesocket (server);
  WSACleanup();
#else
  close (server);
  wait (NULL);
#endif
}

/******************************************************************************
* Call back for new information on server
******************************************************************************/

void 
socket_server_callback (void *obj, void *info) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
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
}
#endif
