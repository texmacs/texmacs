
/******************************************************************************
* MODULE     : cmdline_link.cpp
* DESCRIPTION: TeXmacs links by pipes, evaluation of single commands
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"

//#if !(defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES)))

#include "tm_link.hpp"
#include "socket_notifier.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include <stdio.h>
#include <string.h>
#ifndef OS_MINGW
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#endif
#if !defined(__APPLE__) && !defined(__FreeBSD__)
#include <malloc.h>
#endif

hashset<pointer> cmdline_link_set;
void cmdline_callback (void *obj, void *info);
extern char **environ;

#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define IN 0
#define OUT 1
#define TERMCHAR '\1'

/******************************************************************************
* The cmdline_link class
******************************************************************************/

struct cmdline_link_rep: tm_link_rep {
  string name;          // name of the plugin
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
  cmdline_link_rep (string name);
  ~cmdline_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
};

cmdline_link_rep::cmdline_link_rep (string name2): name (name2) {
  cmdline_link_set->insert ((pointer) this);
  in     = pp_in [0]= pp_in [1]= -1;
  out    = pp_out[0]= pp_out[1]= -1;
  err    = pp_err[0]= pp_err[1]= -1;
  outbuf = "";
  errbuf = "";
  alive  = false;
}

cmdline_link_rep::~cmdline_link_rep () {
  stop ();
  cmdline_link_set->remove ((pointer) this);
}

tm_link
make_cmdline_link (string name) {
  return tm_new<cmdline_link_rep> (name);
}

void
close_all_cmdlines () {
#ifndef OS_MINGW
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    cmdline_link_rep* con= (cmdline_link_rep*) it->next();
    if (con->alive) {
      if (-1 != killpg(con->pid,SIGTERM)) {
sleep(2);
        killpg(con->pid,SIGKILL);
      }
      con->alive= false;
      //cout << "Alive = false, close all\n";
    }
  }
#endif
}

void
process_all_cmdlines () {
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    cmdline_link_rep* con= (cmdline_link_rep*) it->next();
    //cout << con->name << " ~> " << (con->alive? "true": "false") << "\n";
    if (con->alive) {
      usleep (250000);
      con->apply_command ();
    }
  }
}

/******************************************************************************
* Routines for cmdline_links
******************************************************************************/

#ifndef OS_MINGW
void
execute_shell (string s) {
  c_string _s (s);
  char *argv[4];
  argv[0] = const_cast<char*> ("sh");
  argv[1] = const_cast<char*> ("-c");
  argv[2] = _s;
  argv[3] = NULL;
  execve ("/bin/sh", argv, environ);
}
#endif

string
cmdline_link_rep::start () {
  outbuf= "";
  errbuf= "";
  return "cmdline";
}

#ifndef OS_MINGW
static string
debug_io_string (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_ABORT) r << "[ABORT]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}
#endif

void
cmdline_link_rep::write (string s, int channel) {
  while (ends (s, "\n")) s= s (0, N(s)-1);
  s= replace (s, "\n", " ");
  //cout << "Write[" << name << "] " << s << "\n";
#ifndef OS_MINGW
  if (alive || (channel != LINK_IN)) return;
  string cmd= as_string (call ("connection-cmdline", name, s));
  cmd= cmd * " 2> /dev/null";
  //cout << "Command[" << name << "," << s << "] = " << cmd << "\n";
  if (cmd == "") return;
  if (DEBUG_AUTO) debug_io << "Launching '" << cmd << "'\n";
  int e1= pipe (pp_in ); (void) e1;
  int e2= pipe (pp_out); (void) e2;
  int e3= pipe (pp_err); (void) e3;
  pid= fork ();
  if (pid==0) { // the child
    setsid();
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
  }
  else { // the main process
    in = pp_in  [OUT];
    close (pp_in [IN]);
    out= pp_out [IN ];
    close (pp_out [OUT]);
    err= pp_err [IN ];
    close (pp_err [OUT]);

    alive= true;
    //cout << "Alive = true\n";
    snout = socket_notifier (out, &cmdline_callback, this, NULL);
    snerr = socket_notifier (err, &cmdline_callback, this, NULL);
    add_notifier (snout);
    add_notifier (snerr);
  }
#endif
}

void
cmdline_link_rep::feed (int channel) {
  //cout << "Feed " << channel << "\n";
#ifndef OS_MINGW
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  int r;
  char tempout[1024];
  if (channel == LINK_OUT) r = ::read (out, tempout, 1024);
  else r = ::read (err, tempout, 1024);
  if (r == -1) {
    io_error << "Read failed for '" << name << "'\n";
    wait (NULL);
  }
  else if (r == 0) {
    if (channel != LINK_OUT) return;
    if (-1 != killpg(pid,SIGTERM)) {
      sleep(2);
      killpg(pid,SIGKILL);
    }

    alive= false;
    //cout << "Alive = false, feed\n";
    remove_notifier (snout);      
    remove_notifier (snerr);
  }
  else {
    if (DEBUG_IO) debug_io << debug_io_string (string (tempout, r));
    if (channel == LINK_OUT) outbuf << string (tempout, r);
    else errbuf << string (tempout, r);
  }
#endif
}

string&
cmdline_link_rep::watch (int channel) {
  //cout << "Watch " << channel << "\n";
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
cmdline_link_rep::read (int channel) {
  //cout << "Read " << channel << "\n";
  if (channel == LINK_OUT) {
    if (alive) {
      //cout << "\n--- partial output ---\n" << outbuf << "\n";
      return "";
    }
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return string("");
}

void
cmdline_link_rep::listen (int msecs) {
  //cout << "Listen " << msecs << "\n";
  if (!alive) return;
  time_t wait_until= texmacs_time () + msecs;
  while (true) {
    int nout= N(outbuf), nerr= N(errbuf);
    fd_set rfds;
    FD_ZERO (&rfds);
    FD_SET (out, &rfds);
    FD_SET (err, &rfds);
    struct timeval tv;
    tv.tv_sec  = msecs / 1000;
    tv.tv_usec = 1000 * (msecs % 1000);
    int nr= select (max (out, err) + 1, &rfds, NULL, NULL, &tv);
    if (nr != 0 && FD_ISSET (out, &rfds)) feed (LINK_OUT);
    if (nr != 0 && FD_ISSET (err, &rfds)) feed (LINK_ERR);
    if (texmacs_time () - wait_until > 0) break;
    if (N(outbuf) == nout && N(errbuf) == nerr) break;
  }
}

void
cmdline_link_rep::interrupt () {
  //cout << "Interrupt\n";
#ifndef OS_MINGW
  if (!alive) return;
  killpg (pid, SIGINT);
#endif
}

void
cmdline_link_rep::stop () {
  //cout << "Stop\n";
#ifndef OS_MINGW
  if (!alive) return;
  if (-1 != killpg(pid,SIGTERM)) {
    sleep(2);
    killpg(pid,SIGKILL);
  }
  alive= false;    
  //cout << "Alive = false, stop\n";
  close (in);
  wait (NULL);

  remove_notifier (snout);
  remove_notifier (snerr);
#endif
}

/******************************************************************************
* Call back for new information on pipe
******************************************************************************/

void cmdline_callback (void *obj, void *info) {
  //cout << "Cmd_line_callback\n";
#ifndef OS_MINGW
  (void) info;
  cmdline_link_rep* con= (cmdline_link_rep*) obj;  
  bool busy= true;
  bool news= false;
  while (busy) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= max (con->err, con->out) + 1;
    FD_SET (con->out, &rfds);
    FD_SET (con->err, &rfds);
  
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    select (max_fd, &rfds, NULL, NULL, &tv);

    busy= false;
    if (con->alive && FD_ISSET (con->out, &rfds)) {
      //cout << "cmdline_callback OUT" << LF;
      con->feed (LINK_OUT);
      busy= news= true;
    }
    if (con->alive && FD_ISSET (con->err, &rfds)) {
      //cout << "cmdline_callback ERR" << LF;
      con->feed (LINK_ERR);
      busy= news= true;
    }
  }
  /* FIXME: find out the appropriate place to call the callback
     Currently, the callback is called in tm_server_rep::interpose_handler */
  if (!is_nil (con->feed_cmd) && news) {
    //cout << "cmdline_callback APPLY" << LF;
    if (!is_nil (con->feed_cmd))
      con->feed_cmd->apply (); // call the data processor
  }
#endif
}

//#endif // !(defined (QTTEXMACS) && defined (OS_MINGW))
