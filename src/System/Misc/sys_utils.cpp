
/******************************************************************************
* MODULE     : sys_utils.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "sys_utils.hpp"
#include "file.hpp"
#include "tree.hpp"
#include "parse_string.hpp"
#include <cstring>

#ifndef OS_MINGW
#include <poll.h>
#else
#include <winsock2.h>
#endif

int script_status = 1;

/******************************************************************************
* System functions
******************************************************************************/

int
system (string s, string& result, string& error) {
#if defined (OS_MINGW64)
  int r= windows_system (s, result, error);
#elif defined (OS_MINGW)
  int r= qt_system (s, result, error);
#elif defined (OS_ANDROID)
  int r= qt_system (s, result, error);
#else 
  int r= unix_system (s, result, error);
#endif
  return r;
}

int
system (string s, string& result) {
#if defined (OS_MINGW64)
  int r= windows_system (s, result); 
#elif defined (OS_MINGW)
  int r= qt_system (s, result);
#elif defined (OS_ANDROID)
  int r= qt_system (s, result);
#else
  int r= unix_system (s, result);
#endif
  return r;
}

int
system (string s) {
  if (DEBUG_STD) debug_shell << s << "\n";
  if (DEBUG_VERBOSE) {
    string result;
    int r= system (s, result);
    debug_shell << result;
    return r;
  }
  else {
#if defined (OS_MINGW64)
    return windows_system (s);
#elif defined (OS_MINGW)
    return qt_system (s);
#elif defined (OS_ANDROID)
    return qt_system (s);
#else
    return unix_system (s);
#endif
  }
}

string
eval_system (string s) {
  string result;
  (void) system (s, result);
  return result;
}

string
var_eval_system (string s) {
  string r= eval_system (s);
  while ((N(r)>0) && (r[N(r)-1]=='\n' || r[N(r)-1]=='\r')) r= r (0,N(r)-1);
  return r;
}

string
get_env (string var) {
  string ret;
  bool has_value = texmacs_getenv(var, ret);
  if (!has_value) {
    if (var == "PWD") return get_env ("HOME");
    return "";
  }
  return ret;
}

void
set_env (string var, string with) {
  texmacs_setenv(var, with);
}

url
get_texmacs_path () {
  string tmpath= get_env ("TEXMACS_PATH");
    //FIXME: Why is this?
  while ((N(tmpath)>0) && (tmpath [N(tmpath) - 1] == '/'))
    tmpath= tmpath (0, N(tmpath)-1);
  return url_system (tmpath);
}

url
get_texmacs_home_path () {
  url path= get_env ("TEXMACS_HOME_PATH");
  if (path == "")
    path= url_system ("$HOME/.TeXmacs");
  return path;
}

array<string>
evaluate_system (array<string> arg,
		 array<int> fd_in, array<string> in,
		 array<int> fd_out) {
  array<string> out (N(fd_out));
  array<string*> ptr (N(fd_out));
  for (int i= 0; i < N(fd_out); i++) ptr[i]= &(out[i]);
#ifdef OS_MINGW
  int ret= mingw_system (arg, fd_in, in, fd_out, ptr);
#elif defined (OS_ANDROID)
  int ret = -1;
  //int ret= qt_system (arg, fd_in, in, fd_out, ptr);
#else
  int ret= unix_system (arg, fd_in, in, fd_out, ptr);
#endif
  return append (as_string (ret), out);
}


string 
get_printing_default () {
#if defined (OS_MINGW)
  url embedded ("$TEXMACS_PATH/bin/SumatraPDF.exe");
  if (exists (embedded))
    return sys_concretize (embedded) * " -print-dialog -exit-when-done";
  else return "";
#else
  return "lp";
#endif
}

class PrintCap {
private:
  string prt_cmd;
  bool blank;
public:	
  PrintCap (): blank (true) {};
  friend string get_printing_cmd ();
  friend void set_printing_cmd (string);
} print_cap;

string
get_printing_cmd () {
  if (print_cap.blank) {
    print_cap.prt_cmd= get_printing_default ();
    print_cap.blank= false;
  }
  return print_cap.prt_cmd;
}

void
set_printing_cmd (string cmd) {
  print_cap.prt_cmd= cmd;
  print_cap.blank= false;
}

bool
has_printing_cmd () {
  static bool has= get_printing_cmd () != "";
  return has;
}

int
tm_poll (struct tm_pollfd* fds, int nfds, int timeout_ms) {
#ifndef OS_MINGW
  struct pollfd pfds[64];
  if (nfds > 64) nfds= 64;
  for (int i= 0; i < nfds; i++) {
    pfds[i].fd= fds[i].fd;
    pfds[i].events= 0;
    if (fds[i].events & TM_POLL_READ)  pfds[i].events |= POLLIN;
    if (fds[i].events & TM_POLL_WRITE) pfds[i].events |= POLLOUT;
    pfds[i].revents= 0;
  }
  int ret= poll (pfds, nfds, timeout_ms);
  for (int i= 0; i < nfds; i++) {
    fds[i].revents= 0;
    if (pfds[i].revents & POLLIN)
      fds[i].revents |= TM_POLL_READ;
    if (pfds[i].revents & POLLOUT)
      fds[i].revents |= TM_POLL_WRITE;
    if (pfds[i].revents & (POLLERR | POLLHUP | POLLNVAL))
      fds[i].revents |= TM_POLL_ERROR;
  }
  return ret;
#else
  // Windows select() only supports sockets, not pipes or file handles
  fd_set rfds, wfds, efds;
  FD_ZERO (&rfds);
  FD_ZERO (&wfds);
  FD_ZERO (&efds);
  for (int i= 0; i < nfds; i++) {
    if (fds[i].events & TM_POLL_READ)  FD_SET (fds[i].fd, &rfds);
    if (fds[i].events & TM_POLL_WRITE) FD_SET (fds[i].fd, &wfds);
    FD_SET (fds[i].fd, &efds);
    fds[i].revents= 0;
  }
  struct timeval tv;
  struct timeval* tvp= NULL;
  if (timeout_ms >= 0) {
    tv.tv_sec= timeout_ms / 1000;
    tv.tv_usec= (timeout_ms % 1000) * 1000;
    tvp= &tv;
  }
  int ret= select (0, &rfds, &wfds, &efds, tvp);
  if (ret > 0) {
    int count= 0;
    for (int i= 0; i < nfds; i++) {
      if (FD_ISSET (fds[i].fd, &rfds)) fds[i].revents |= TM_POLL_READ;
      if (FD_ISSET (fds[i].fd, &wfds)) fds[i].revents |= TM_POLL_WRITE;
      if (FD_ISSET (fds[i].fd, &efds)) fds[i].revents |= TM_POLL_ERROR;
      if (fds[i].revents) count++;
    }
    return count;
  }
  return ret;
#endif
}

/******************************************************************************
* Asynchroneous execution of commands
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include "scheme.hpp"

struct async_handle {
  FILE*  fp;
  object call_back;
  bool   done;
  char*  buf;
  int    len;
  int    cap;
  async_handle (FILE* fp2, object call_back2):
    fp (fp2), call_back (call_back2), done (false),
    buf ((char*) malloc (4096)), len (0), cap (4096) {}
};

array<async_handle*> async_busy;

void*
async_read_output (void* arg) {
  typedef FILE* FILEp;
  typedef char* charp;
  async_handle* handle= (async_handle*) arg;
  FILEp& fp  = handle->fp;
  bool&  done= handle->done;
  charp& buf = handle->buf;
  int&   len = handle->len;
  int&   cap = handle->cap;

  while (true) {
    char buffer[4096];
    int bytes_read;
    bytes_read= fread (buffer, 1, sizeof (buffer), fp);
    if (bytes_read <= 0) break;
    if (bytes_read + len > cap) {
      char* buf2= (char*) malloc (2 * cap);
      for (int i=0; i<len; i++) buf2[i]= buf[i];
      free ((void*) buf);
      buf= buf2;
      cap= 2 * cap;
    }
    for (int i=0; i<bytes_read; i++)
      buf[len+i]= buffer[i];
    len += bytes_read;
  }

  pclose (fp);
  fp  = NULL;
  done= true;
  return NULL;
}

bool
async_eval_system (string cmd, object call_back) {
  int i, n= N(cmd);
  char* cmd_= (char*) malloc (n+1);
  for (i=0; i<n; i++) cmd_[i]= cmd[i];
  cmd_[n]= '\0';

  FILE *fp = popen (cmd_, "r");
  if (!fp) return true;
  async_handle* handle= tm_new<async_handle> (fp, call_back);
  async_busy << handle;

  pthread_t thread;
  pthread_create (&thread, NULL, async_read_output, handle);
  pthread_detach (thread);

  free ((void*) cmd_);
  return false;
}

void
async_eval_pending () {
  for (int i=0; i<N(async_busy); )
    if (async_busy[i]->done) {
      async_handle* handle= async_busy[i];
      string out (handle->buf, handle->len);
      call (async_busy[i]->call_back, out);
      free (handle->buf);
      tm_delete<async_handle> (handle);
      async_busy= append (range (async_busy, 0, i),
                          range (async_busy, i + 1, N(async_busy)));
    }
    else i++;
}
