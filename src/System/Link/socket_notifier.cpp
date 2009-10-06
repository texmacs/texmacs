
/******************************************************************************
* MODULE     : socket_notifier.cpp
* DESCRIPTION: Notifiers for socket activity
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"

#ifndef __MINGW32__
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif
#include <errno.h>
#ifdef OS_WIN32
#include <sys/misc.h>
#endif

#ifdef QTTEXMACS
#include "Qt/qt_gui.hpp"
#endif

#include "socket_notifier.hpp"
#include "list.hpp"
#include "iterator.hpp"

static hashset<socket_notifier> notifiers;

void
add_notifier (socket_notifier sn)  {
  //cout << "enable notifier " << LF;
  notifiers->insert (sn);
#ifdef QTTEXMACS
  the_gui->add_notifier (sn);
#endif
} 

void
remove_notifier (socket_notifier sn)  {
  //cout << "disable notifier " << LF;
  notifiers->remove (sn);
#ifdef QTTEXMACS
  the_gui->remove_notifier (sn);
#endif
}

void 
perform_select () {
#ifndef __MINGW32__
  //FIXME: this can be optimizied
  while (true) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= 0;
    iterator<socket_notifier> it = iterate (notifiers);
    while (it->busy ()) {
      socket_notifier sn= it->next ();
      FD_SET (sn->fd, &rfds);
      if (sn->fd >= max_fd) max_fd= sn->fd+1;
    }
    if (max_fd == 0) break;
    
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr = select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) break;
    
    it = iterate (notifiers);
    while (it->busy ()) {
      socket_notifier sn=  it->next ();
      if (FD_ISSET (sn->fd, &rfds)) sn->notify ();
    }
  }  
#endif  
}
