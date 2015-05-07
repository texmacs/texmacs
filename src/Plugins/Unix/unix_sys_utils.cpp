
/******************************************************************************
* MODULE     : unix_sys_utils.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2009  David MICHEL, 2015  Gregoire LECERF
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_sys_utils.hpp"
#include "file.hpp"
#include "timer.hpp"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <spawn.h>
#include <sys/wait.h>

int
unix_system (string s) {
  c_string _s (s * " > /dev/null 2>&1");
  int ret= system (_s);
  return ret;
}

int
unix_system (string cmd, string& result) {
  url temp= url_temp ();
  string temp_s= escape_sh (concretize (temp));
  c_string _cmd (cmd * " > " * temp_s * " 2>&1");
  int ret= system (_cmd);
  bool flag= load_string (temp, result, false);
  remove (temp);
  if (flag) result= "";
  return ret;
}

/******************************************************************************
* Evaluation via specified file descriptors
******************************************************************************/

extern char **environ;

struct _pipe_t {
  int rep[2];
  int st;
  inline _pipe_t () { st= pipe (rep);
    fcntl(rep[0], F_SETFL, fcntl(rep[0], F_GETFL) | O_NONBLOCK);
    fcntl(rep[1], F_SETFL, fcntl(rep[1], F_GETFL) | O_NONBLOCK); }
  inline ~_pipe_t () { close (rep[0]); close (rep[1]); }
  inline int in () const { return rep[0]; }
  inline int out () const { return rep[1]; }
  inline int status () const { return st; }
};

struct _file_actions_t {
  posix_spawn_file_actions_t rep;
  int st;
  inline _file_actions_t () { 
    st= posix_spawn_file_actions_init (&rep); }
  inline ~_file_actions_t () {
    posix_spawn_file_actions_destroy (&rep); }
  inline int status () const { return st; }
};

static void
_unix_system_warn (pid_t pid, string which, string msg) {
  (void) which;
  io_warning << "unix_system, pid " << pid
	     << ", warning: " << msg << "\n";
}

int
unix_system (array<string> arg,
	     array<int> fd_in, array<string> str_in,
	     array<int> fd_out, array<string*> str_out) {
  // Run command arg[0] with arguments arg[i], i >= 1.
  // str_in[i] is sent to the file descriptor fd_in[i].
  // str_out[i] is filled from the file descriptor fd_out[i].
  // If str_in[i] is -1 then $$i automatically replaced by a valid
  // file descriptor in arg.
  if (N(arg) == 0) return 0;
  string which= recompose (arg, " ");
  int n_in= N(fd_in), n_out= N(fd_out);
  ASSERT(N(str_in)  == n_in, "size mismatch");
  ASSERT(N(str_out) == n_out, "size mismatch");
  array<_pipe_t> pp_in (n_in), pp_out (n_out);
  _file_actions_t file_actions;
  for (int i= 0; i < n_in; i++) {
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_in[i].out ()) != 0) return -1;
    if (fd_in[i] >= 0) {
      if (posix_spawn_file_actions_adddup2
	  (&file_actions.rep, pp_in[i].in (), fd_in[i]) != 0) return -1;
      if (posix_spawn_file_actions_addclose
	  (&file_actions.rep, pp_in[i].in ()) != 0) return -1; } }
  for (int i= 0; i < n_out; i++) {
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_out[i].in ()) != 0) return -1;
    if (posix_spawn_file_actions_adddup2
	(&file_actions.rep, pp_out[i].out (), fd_out[i]) != 0) return -1;
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_out[i].out ()) != 0) return -1; }
  array<string> arg_= arg;
  for (int j= 0; j < N(arg); j++)
    for (int i= 0; i < n_in; i++)
      if (fd_in[i] < 0)
        arg_[j]= replace (arg_[j], "$$" * as_string (i),
	  	          as_string (pp_in[i].in ()));
  if (DEBUG_IO)
    debug_io << "unix_system, launching: " << arg_ << "\n"; 
  array<char*> _arg;
  for (int j= 0; j < N(arg_); j++)
    _arg << as_charp (arg_[j]);
  _arg << (char*) NULL;
  pid_t pid;
  int status= posix_spawnp (&pid, _arg[0], &file_actions.rep, NULL,
			    A(_arg), environ);
  for (int j= 0; j < N(arg_); j++)
    tm_delete_array (_arg[j]);
  if (status != 0) {
    if (DEBUG_IO) debug_io << "unix_system, failed" << "\n";
    return -1;
  }
  if (DEBUG_IO)
    debug_io << "unix_system, succeeded to create pid "
	     << pid << "\n";

  // send/receive data to/from spawn process
  array<int> pos_in (n_in);
  for (int i= 0; i < n_in; i++) pos_in[i]= 0;
  const int chunk_size= 256; // FIXME?
  array<char> buffer (chunk_size);
  for (int i= 0; i < n_in ; i++) close (pp_in[i].in ());
  for (int i= 0; i < n_out; i++) close (pp_out[i].out ());
  bool busy= true;
  int transferred= 0;
  time_t last_transfer_time= texmacs_time ();
  for (int i= 0; i < n_in; i++)
    if (N(str_in[i]) == 0)
      if (close (pp_in[i].out ()) != 0) return -1;
  while (busy) {
    if (texmacs_time () - last_transfer_time > 100) usleep (100);
    if (texmacs_time () - last_transfer_time > 5000) { // FIXME?
      last_transfer_time= texmacs_time ();
      _unix_system_warn (pid, which, "silent spawn process");
    }
    busy= false; transferred= 0;
    for (int i= 0; i < n_in; i++) {
      if (N(str_in[i]) > pos_in[i]) {
        int m= min (chunk_size, N(str_in[i]) - pos_in[i]);
        int o= write (pp_in[i].out (), &(str_in[i][pos_in[i]]), m);
        if (o >= 0) { pos_in[i] += o; transferred += o; }
	busy= true;
        if (N(str_in[i]) == pos_in[i]) {
          if (close (pp_in[i].out ()) != 0) return -1; } } }
    for (int i= 0; i < n_out; i++) {
      if (fcntl (pp_out[i].in (), F_GETFL) != -1) {
	busy= true;
        int m= read (pp_out[i].in (), A (buffer), chunk_size);
	if (m > 0) transferred += m;
        if (m == 0) {
          if (close (pp_out[i].in ()) != 0) return -1; }
        if (m > 0) {
          (*(str_out[i])) << string ((const char*) A (buffer), m); } } }
    if (transferred > 0) last_transfer_time= texmacs_time ();
  }
  // wait for process
  int wret;
  time_t last_wait_time= texmacs_time ();
  while ((wret= waitpid (pid, &status, WNOHANG)) == 0) {
    usleep (100);
    if (texmacs_time () - last_wait_time > 5000) { // FIXME?
      last_wait_time= texmacs_time ();
      _unix_system_warn (pid, which, "waiting spawn process");
    }
  }
  if (DEBUG_IO)
    debug_io << "unix_system, pid " << pid
	     << " terminated" << "\n"; 
  if (wret < 0 || WIFEXITED(status) == 0) return -1;
  return WEXITSTATUS(status);
}
