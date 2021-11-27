
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
#include "tm_timer.hpp"
#include <stdlib.h>
#include <fcntl.h>
#include <spawn.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <pthread.h>
#include <pwd.h>

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

int
unix_system (string cmd, string& result, string& error) {
  url temps= url_temp ();
  url tempe= url_temp ();
  string temp_s= escape_sh (concretize (temps));
  string temp_e= escape_sh (concretize (tempe));
  c_string _cmd (cmd * " > " * temp_s * " 2> " * temp_e);
  int ret= system (_cmd);
  bool flag= load_string (temps, result, false);
  remove (temps);
  if (flag) result= "";
  flag= load_string (tempe, error, false);
  remove (tempe);
  if (flag) error= "";
  return ret;
}

/******************************************************************************
* Evaluation via specified file descriptors
******************************************************************************/

#if !defined(OS_MINGW) && !defined(X11TEXMACS)

extern char **environ;

// exception safe mutex
struct _mutex {
  pthread_mutex_t rep;
  inline _mutex () { pthread_mutex_init (&rep, NULL); }
  inline ~_mutex () { pthread_mutex_destroy (&rep); }
};

struct _mutex_lock {
  pthread_mutex_t* rep;
  inline _mutex_lock (_mutex& m): rep (&(m.rep)) {
    pthread_mutex_lock (rep); }
  inline ~_mutex_lock () {
    pthread_mutex_unlock (rep); }
};

// thread safe malloc and free
static _mutex _ts_memory_lock;

void*
_ts_malloc (int n) {
  _mutex_lock lock (_ts_memory_lock);
  return malloc (n);
}

void
_ts_free (void* a) {
  _mutex_lock lock (_ts_memory_lock);
  free (a);
}

// thread safe strings
struct _ts_string {
  int n, l;
  char* a;

  inline _ts_string (): n (0), l (0), a (NULL) {}
  inline ~_ts_string () { if (l != 0) _ts_free ((void*) a); }

  void resize (int m) {
    if (m <= n) return;
    int new_l= max (2 * n, m);
    char* new_a= (char*) _ts_malloc (new_l * sizeof (char));
    memcpy (new_a, a, n);
    _ts_free ((void*) a);
    a= new_a;
    l= new_l; }

  void append (char* b, int m) {
    resize (m + n);
    memcpy (a + n, b, m);
    n += m; }

  void copy (char* b, int m) {
    resize (m);
    memcpy (a, b, m);
    n= m; }
};

// pipe
struct _pipe_t {
  int rep[2];
  int st;
  inline _pipe_t () {
    st= pipe (rep);
    int fl= fcntl (rep[0], F_GETFL);
    fl = fl & (~(int) O_NONBLOCK);
    fcntl (rep[0], F_SETFL, fl);
    fl= fcntl (rep[1], F_GETFL);
    fl= fl & (~(int) O_NONBLOCK);
    fcntl (rep[1], F_SETFL, fl); }
  inline ~_pipe_t () { close (rep[0]); close (rep[1]); }
  inline int in () const { return rep[0]; }
  inline int out () const { return rep[1]; }
  inline int status () const { return st; }
};

// asynchronous channel between spawn process
struct _channel {
  int fd;
  _ts_string data;
  int buffer_size;
  array<char> buffer;
  int status;
  _channel () : status (0) {}
  void _init_in (int fd2, string data2, int chunk_size) {
    fd= fd2;
    data.copy (&data2[0], N(data2));
    buffer_size= chunk_size; }
  void _init_out (int fd2, int buffer_size2) {
    fd= fd2;
    buffer_size= buffer_size2;
    buffer= array<char> (buffer_size2); }
};

// data read from spawn process
static void*
_background_read_task (void* channel_as_void_ptr) {
  _channel* c= (_channel*) channel_as_void_ptr;
  int fd= c->fd;
  int n= c->buffer_size;
  char* b= A (c->buffer);
  int m;
  do {
    m= read (fd, b, n);
    // cout << "read " << m << " bytes from " << fd << "\n";
    if (m > 0) c->data.append (b, m);
    if (m == 0) { if (close (fd) != 0) c->status= -1; }
  } while (m > 0);
  return (void*) NULL;
}

// data written to spawn process
static void*
_background_write_task (void* channel_as_void_ptr) {
  _channel* c= (_channel*) channel_as_void_ptr;
  int fd= c->fd;
  const char* d= c->data.a;
  int n= c->buffer_size;
  int t= (c->data).n, k= 0, o= 0;
  if (t == 0) return (void*) NULL;
  if (n == 0) { c->status= -1; return (void*) NULL; }
  do {
    int m= min (n, t - k);
    // cout << "writting " << m << " bytes / " << t-k << "\n";
    o= write (fd, (void*) (d + k), m);
    // cout << "written " << o << " bytes to " << fd << "\n";
    if (o > 0) k += o;
    if (o < 0) { close (fd); c->status= -1; }
    if (k == t) { if (close (fd) != 0) c->status= -1; }
  } while (o > 0 && k < t);
  return (void*) NULL;
}

// exception safe file actions
struct _file_actions_t {
  posix_spawn_file_actions_t rep;
  int st;
  inline _file_actions_t () { 
    st= posix_spawn_file_actions_init (&rep); }
  inline ~_file_actions_t () {
    posix_spawn_file_actions_destroy (&rep); }
  inline int status () const { return st; }
};

// Texmacs warning for long spawn commands
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

  // close useless ports
  for (int i= 0; i < n_in ; i++) close (pp_in[i].in ());
  for (int i= 0; i < n_out; i++) close (pp_out[i].out ());

  // write to spawn process
  array<_channel> channels_in (n_in);
  array<pthread_t> threads_write (n_in);
  for (int i= 0; i < n_in; i++) {
    channels_in[i]._init_in (pp_in[i].out (), str_in[i], 1 << 12);
    if (pthread_create (&threads_write[i], NULL /* &attr */,
			_background_write_task,
			(void *) &(channels_in[i])))
      return -1;
  }

  // read from spawn process
  array<_channel> channels_out (n_out);
  array<pthread_t> threads_read (n_out);
  for (int i= 0; i < n_out; i++) {
    channels_out[i]._init_out (pp_out[i].in (), 1 << 12); 
    if (pthread_create (&threads_read[i], NULL /* &attr */,
			_background_read_task,
			(void *) &(channels_out[i])))
      return -1;
  }

  int wret;
  time_t last_wait_time= texmacs_time ();
  while ((wret= waitpid (pid, &status, WNOHANG)) == 0) {
    usleep (100);
    if (texmacs_time () - last_wait_time > 5000) {
      last_wait_time= texmacs_time ();
      _unix_system_warn (pid, which, "waiting spawn process");
    }
  }
  if (DEBUG_IO)
    debug_io << "unix_system, pid " << pid << " terminated" << "\n"; 

  // wait for terminating threads
  void* exit_status;
  int thread_status= 0;
  for (int i= 0; i < n_in; i++) {
    pthread_join (threads_write[i], &exit_status);
    if (channels_in[i].status < 0) thread_status= -1;
  }
  for (int i= 0; i < n_out; i++) {
    pthread_join (threads_read[i], &exit_status);
    *(str_out[i])= string (channels_out[i].data.a,
                           channels_out[i].data.n);
    if (channels_out[i].status < 0) thread_status= -1;
  }

  if (thread_status < 0) return thread_status;
  if (wret < 0 || WIFEXITED(status) == 0) return -1;
  return WEXITSTATUS(status);
}

#else

int
unix_system (array<string> arg,
	     array<int> fd_in, array<string> str_in,
	     array<int> fd_out, array<string*> str_out) {
  (void) arg; (void) fd_in; (void) str_in; (void) fd_out; (void) str_out;
  FAILED ("unsupported system call");
}

#endif

string unix_get_login () {
  uid_t uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return string(pwd->pw_name);
}

string unix_get_username () {
  uid_t uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return tokenize (string (pwd->pw_gecos), string(","))[0];
}
