
/******************************************************************************
* MODULE     : pipe_link.cpp
* DESCRIPTION: TeXmacs links by pipes
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "pipe_link.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "timer.hpp"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#ifndef __APPLE__
#include <malloc.h>
#endif

hashset<pointer> pipe_link_set;

/******************************************************************************
* Constructors and destructors for pipe_links
******************************************************************************/

pipe_link_rep::pipe_link_rep (string cmd2): cmd (cmd2) {
  pipe_link_set->insert ((pointer) this);
  in     = pp_in [0]= pp_in [1]= -1;
  out    = pp_out[0]= pp_out[1]= -1;
  err    = pp_err[0]= pp_err[1]= -1;
  outbuf = "";
  errbuf = "";
  alive  = false;
}

pipe_link_rep::~pipe_link_rep () {
  stop ();
  pipe_link_set->remove ((pointer) this);
}

tm_link
make_pipe_link (string cmd) {
  return new pipe_link_rep (cmd);
}

/******************************************************************************
* Routines for pipe_links
******************************************************************************/

void
execute_shell (string s) {
  char *_s= as_charp (s);
  char *argv[4];
  argv[0] = "sh";
  argv[1] = "-c";
  argv[2] = _s;
  argv[3] = 0;
#ifdef OS_WIN32
  _system(_s);
#else
  execve ("/bin/sh", argv, environ);
#endif
  delete[] _s;
}

string
pipe_link_rep::start () {
  if (alive) return "busy";
  if (DEBUG_AUTO) cout << "TeXmacs] Launching '" << cmd << "'\n";

  pipe (pp_in );
  pipe (pp_out);
  pipe (pp_err);
  pid= fork ();
  if (pid==0) { // the child
#ifndef OS_WIN32
    close (pp_in  [OUT]);
    close (pp_out [IN ]);
    close (pp_err [IN ]);
    dup2  (pp_in  [IN ], STDIN );
    close (pp_in  [IN ]);
    dup2  (pp_out [OUT], STDOUT);
    close (pp_out [OUT]);
    dup2  (pp_err [OUT], STDERR);
    close (pp_err [OUT]);

    execute_shell (cmd);
    exit (127);
    // exit (system (cmd) != 0);
#endif
  }
  else { // the main process
    in = pp_in  [OUT];
    close (pp_in [IN]);
    out= pp_out [IN ];
    close (pp_out [OUT]);
    err= pp_err [IN ];
    close (pp_err [OUT]);

    alive= true;
    if (/* !banner */ true) return "ok";
    else {
      int r;
      char outbuf[1024];
      r = ::read (out, outbuf, 1024);
      if (r == 1 && outbuf[0] == TERMCHAR) return "ok";
      alive= false;
      recursive_kill (pid);
      wait (NULL);
      if (r == ERROR) return "Error: the application does not reply";
      else
	return "Error: the application did not send its usual startup banner";
    }
  }
}

static string
debug_io_string (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}

void
pipe_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) cout << "---> " << debug_io_string (s) << "\n";
  char* _s= as_charp (s);
  ::write (in, _s, N(s));
  delete[] _s;
}

void
pipe_link_rep::feed (int channel) {
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;

  int r;
  char tempout[1024];
  if (channel == LINK_OUT) r = ::read (out, tempout, 1024);
  else r = ::read (err, tempout, 1024);
  if (r == ERROR) {
    cerr << "TeXmacs] read failed for#'" << cmd << "'\n";
    wait (NULL);
  }
  else if (r == 0) {
    recursive_kill (pid);
    alive= false;
  }
  else {
    if (DEBUG_IO) cout << debug_io_string (string (tempout, r));
    if (channel == LINK_OUT) outbuf << string (tempout, r);
    else errbuf << string (tempout, r);
  }
}

string
pipe_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
}

void
pipe_link_rep::listen (int msecs) {
  int wait_until= texmacs_time () + msecs;
  while ((outbuf == "") && (errbuf == "")) {
    listen_to_pipes (); // FIXME: should listen more specifically
    if (texmacs_time () > wait_until) break;
  }
}

void
pipe_link_rep::interrupt () {
  if (!alive) return;
  kill (pid, SIGINT);
}

void
pipe_link_rep::stop () {
  if (!alive) return;
  recursive_kill (pid);
  alive= false;
  close (in);
  wait (NULL);
}

/******************************************************************************
* Listen to all active pipes (may be optimized for speed)
******************************************************************************/

void
listen_to_pipes () {
  while (true) {
#ifdef OS_WIN32
    fd_set_ rfds;
    FD_ZERO_ (&rfds);
#else
    fd_set rfds;
    FD_ZERO (&rfds);
#endif
    int max_fd= 0;
    iterator<pointer> it= iterate (pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con= (pipe_link_rep*) it->next();
      if (con->alive) {
#ifdef OS_WIN32
	FD_SET_ (con->out, &rfds);
	FD_SET_ (con->err, &rfds);
#else
	FD_SET (con->out, &rfds);
	FD_SET (con->err, &rfds);
#endif
	if (con->out >= max_fd) max_fd= con->out+1;
	if (con->err >= max_fd) max_fd= con->err+1;
      }
    }
    if (max_fd == 0) break;

#ifdef OS_WIN32
    struct timeval_ tv;
#else
    struct timeval tv;
#endif
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) break;

    it= iterate (pipe_link_set);
    while (it->busy()) {
      pipe_link_rep* con= (pipe_link_rep*) it->next();
#ifdef OS_WIN32
      if (con->alive && FD_ISSET_ (con->out, &rfds)) con->feed (LINK_OUT);
      if (con->alive && FD_ISSET_ (con->err, &rfds)) con->feed (LINK_ERR);
#else
      if (con->alive && FD_ISSET (con->out, &rfds)) con->feed (LINK_OUT);
      if (con->alive && FD_ISSET (con->err, &rfds)) con->feed (LINK_ERR);
#endif
    }
  }
}

/******************************************************************************
* Emergency exit for all pipes
******************************************************************************/

void
close_all_pipes () {
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    pipe_link_rep* con= (pipe_link_rep*) it->next();
    if (con->alive) {
      recursive_kill (con->pid);
      con->alive= false;
    }
  }
}
