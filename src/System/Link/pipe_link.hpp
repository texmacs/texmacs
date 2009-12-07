
/******************************************************************************
* MODULE     : pipe_link.cpp
* DESCRIPTION: TeXmacs links by pipes
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PIPE_LINK_H
#define PIPE_LINK_H
#include "tm_link.hpp"
#include "socket_notifier.hpp"

extern char **environ;

#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define IN 0
#define OUT 1
#define TERMCHAR '\1'

/******************************************************************************
* The pipe_link class
******************************************************************************/

struct pipe_link_rep: tm_link_rep {
  string cmd;           // command for launching the pipe
  int    pid;           // process identifier of the child
  int    pp_in [2];     // for data going to the child
  int    pp_out[2];     // for data coming from the child
  int    pp_err[2];     // for error messages coming from the child
  int    in;            // file descriptor for data going to the child
  int    out;           // file descriptor for data coming from the child
  int    err;           // file descriptor for errors coming from the child

  string outbuf;        // pending output from plugin
  string errbuf;        // pending errors from plugin

  socket_notifier snout, snerr;
  
public:
  pipe_link_rep (string cmd);
  ~pipe_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
};

void listen_to_pipes ();
void close_all_pipes ();

#endif // PIPE_LINK_H
