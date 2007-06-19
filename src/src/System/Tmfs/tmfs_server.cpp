
/******************************************************************************
* MODULE     : tmfs_server.cpp
* DESCRIPTION: remote acces to TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven 
* NOTE       : thanks to Beej's Guide to Network Programming
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "Scheme/object.hpp"
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

void (*server_call_back) (void)= NULL;
static fd_set connected;   // connected file descriptor list
static fd_set processing; // temp file descriptor list for select()
static struct sockaddr_in local_address;  // server address
static struct sockaddr_in remote_address; // client address
static int fdmax;        // maximum file descriptor number
static int listener;     // listening socket descriptor
static char buf[256];    // buffer for client data
static int nbytes;
static bool first_flag= true;

void
tmfs_server_loop () {
  if (first_flag) {
    call ("kill-window");
    first_flag= false;
  }

  struct timeval tv;
  tv.tv_sec  = 0;
  tv.tv_usec = 0;
  processing = connected; // copy it
  if (select (fdmax+1, &processing, NULL, NULL, &tv) == -1) {
    perror ("select");
    exit (1);
  }

  int i, j;
  for (i=0; i<=fdmax; i++) {
    if (FD_ISSET (i, &processing)) {
      if (i == listener) {
	// handle new connections
	socklen_t addrlen= sizeof (remote_address);
	int newfd=
	  accept (listener, (struct sockaddr *) &remote_address, &addrlen);
	if (newfd == -1) perror ("accept");
	else {
	  FD_SET (newfd, &connected); // add to connected set
	  if (newfd > fdmax) fdmax = newfd;
	  //cout << "selectserver: new connection from "
	  //<< inet_ntoa(remote_address.sin_addr)
	  //<< " on " << newfd << "\n";
	}
      }
      else {
	// handle data from a client
	if ((nbytes = recv(i, buf, sizeof(buf), 0)) <= 0) {
	  // got error or connection closed by client
	  if (nbytes == 0) {
	    // connection closed
	    printf ("selectserver: socket %d hung up\n", i);
	  } else {
	    perror ("recv");
	  }
	  close (i); // bye!
	  FD_CLR (i, &connected); // remove from connected set
	}
	else {
	  // we got some data from a client
	  for (j=0; j<=fdmax; j++) {
	    // send to everyone!
	    if (FD_ISSET (j, &connected)) {
	      // except the listener and ourselves
	      if (j != listener && j != i) {
		if (string (buf, 4) == "quit") exit (0);
		if (send (j, buf, nbytes, 0) == -1) {
		  perror ("send");
		}
	      }
	    }
	  }
	}
      }
    }
  }
}

void
tmfs_start_server () {
  server_call_back= tmfs_server_loop;

  FD_ZERO (&connected);
  FD_ZERO (&processing);

  // get the listener
  if ((listener = socket (PF_INET, SOCK_STREAM, 0)) == -1)
    fatal_error ("call to 'socket' failed", "tmfs_start_server");

  // lose the pesky "address already in use" error message
  static int yes=1; // for setsockopt() SO_REUSEADDR, below
  if (setsockopt (listener, SOL_SOCKET, SO_REUSEADDR,
		  &yes, sizeof(int)) == -1)
    fatal_error ("call to 'setsockopt' failed", "tmfs_start_server");

  // bind
  local_address.sin_family = AF_INET;
  local_address.sin_addr.s_addr = INADDR_ANY;
  local_address.sin_port = htons (6561);
  memset (local_address.sin_zero, '\0', sizeof local_address.sin_zero);
  if (bind (listener, (struct sockaddr *) &local_address,
	    sizeof(local_address)) == -1)
    fatal_error ("call to 'bind' failed", "tmfs_start_server");

  // listen
  if (listen (listener, 10) == -1)
    fatal_error ("call to 'listen' failed", "tmfs_start_server");

  // add the listener to the connected set
  FD_SET (listener, &connected);

  // keep track of the biggest file descriptor
  fdmax = listener; // so far, it's this one
}
