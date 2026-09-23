
/******************************************************************************
* MODULE     : mingw_sys_utils.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2015  Gregoire LECERF, Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mingw_sys_utils.hpp"
#include "analyze.hpp"
#include "tm_timer.hpp"
#include "spawn.hpp"
#include "Windows/win-utf8-compat.hpp"


static void
_unix_system_warn (pid_t pid, ::string which, ::string msg) {
  debug_io << "unix_system, pid " << pid << ", warning: " << msg << "\n";
}

int
mingw_system (::array< ::string> arg,
	    ::array<int> fd_in, ::array< ::string> str_in,
      ::array<int> fd_out, ::array< ::string*> str_out) {
	// Run command arg[0] with arguments arg[i], i >= 1.
  // str_in[i] is sent to the file descriptor fd_in[i].
  // str_out[i] is filled from the file descriptor fd_out[i].
  // If str_in[i] is -1 then $$i automatically replaced by a valid
  // file descriptor in arg.
  if (N(arg) == 0) return 0;
  ::string which= recompose (arg, " ");
  int n_in= N (fd_in), n_out= N (fd_out);
  ASSERT(N(str_in)  == n_in, "size mismatch");
  ASSERT(N(str_out) == n_out, "size mismatch");
  ::array<Channel> ch (n_in + n_out);

  for (int i= 0; i < n_in; i++) {
    int fd= fd_in[i];
    if (fd >= 0) ch[i].Init (fd,Channel::CHIN); 
    else ch[i].Init(Channel::CHIN);
  }
  for (int i= 0; i < n_out; i++) {
    int fd= fd_out[i];
    if (fd >= 0) ch[i + n_in].Init (fd,Channel::CHOUT); 
    else ch[i + n_in].Init(Channel::CHOUT);
  }

  ::array< ::string> arg_= arg;
  for (int j= 0; j < N(arg); j++)
    for (int i= 0; i < n_in; i++)
      if (fd_in[i] < 0) {
        arg_[j]= replace (arg_[j], "$%" * as_string (i),
          as_string (_get_osfhandle (ch[i].getPipe ())));
        arg_[j]= replace (arg_[j], "$$" * as_string (i),
          as_string (ch[i].getPipe ()));
      }
  debug_io << "unix_system, launching: " << arg_ << "\n"; 
  ::array<char*> _arg;
  for (int j= 0; j < N(arg_); j++)
    _arg << as_charp (arg_[j]);
  _arg << (char*) NULL;

  spawn_system process (ch, _arg[0], A(_arg));
  for (int j= 0; j < N(arg_); j++)
    tm_delete_array (_arg[j]);
  if (!process.isRunning ()) {
    debug_io << "unix_system, failed" << "\n";
    return -1;
  }
  debug_io << "unix_system, succeeded to create pid " << \
      process.getpid() <<  LF;

  // receive data from spawn process
  // class string is not thread safe, use std::string instead
  ::array<std::string> str(n_out);
  for (int i= 0; i < n_out; i++) ch[i + n_in].read(&str[i]);

  // send data to spawn process
  ::array<int> pos_in (n_in);
  for (int i= 0; i < n_in; i++) pos_in[i]= 0;
  time_t last_wait_time= texmacs_time ();

  bool busy;
  do {
    busy= false;
    if (texmacs_time () - last_wait_time > 5000) { // FIXME?
      last_wait_time= texmacs_time ();
      _unix_system_warn (process.getpid(), which, "waiting spawn process");
    }
    for (int i= 0; i < n_in; i++) {
      if (N(str_in[i]) > pos_in[i]) {
        int m= min (ch[i].sz, N(str_in[i]) - pos_in[i]); //do not fill the pipe
        int o= ch[i].write (&(str_in[i][pos_in[i]]), m);
        if (o >= 0) { 
          pos_in[i] += o;
          if (N(str_in[i]) == pos_in[i]) ch[i].close (); else busy= true;
        } 
      }
    }
  } while (busy);

  // wait for process
  int wret= process.wait();
  debug_io << "unix_system, pid " << process.getpid ()
           << " terminated with code" << wret << "\n"; 
  for (int i= 0; i < n_out; ++i)
    (*(str_out[i])) << ::string(str[i].data (), str[i].length ()); 
  return (wret);
}

namespace sys_utils {

#ifndef SECURITY_WIN32
#define SECURITY_WIN32
#endif
#include <basetsd.h>
#include <wtypesbase.h>
#include <ntsecapi.h>
#include <secext.h>

  ::string mingw_get_username () {
    const int MAX_LEN= 100;
    WCHAR buffer[MAX_LEN];
    DWORD len;

    // This API must be called twice, otherwise it returns an empty use name
    GetUserNameExW (NameDisplay, buffer, &len);
    GetUserNameExW (NameDisplay, buffer, &len);

    if (len == 0) return ::string ("");
    else return ::string (nowide::narrow(buffer).c_str());
  }
}
