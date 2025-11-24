
/******************************************************************************
* MODULE     : tm_shell.cpp
* DESCRIPTION: TeXmacs shell
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#include <errno.h> 
#ifndef _WIN32
#include <sys/wait.h>
#include <termios.h>
#else
#include "termios.h"
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#include "termios.h" // Ensure you have a dummy termios.h or remove struct usage for Win32

// Mapping standard Unix names to Windows underscore names
#define read _read
#define write _write
#define dup _dup
#define dup2 _dup2
#define close _close
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif

#ifdef __FreeBSD__
#include <sys/types.h>
#include <sys/ioctl.h>
#include <libutil.h>
#else
#if HAVE_PTY_H
#include <pty.h>
#endif
#if HAVE_UTIL_H
#include <util.h>
#endif
#endif

using namespace std;

typedef char* charp;
#ifndef _WIN32
extern "C" charp* environ;
#endif

#define ERROR (-1)
#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define IN 0
#define OUT 1
#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)
//#define DATA_BEGIN   "[BEGIN]"
//#define DATA_END     "[END]"
//#define DATA_ESCAPE  "[ESCAPE]"

static int pid;
static int master;

#ifdef _WIN32
// Windows pipes are unidirectional. We need a separate handle for writing to the child.
static int master_write; 
#endif

static char prompt[] = "tmshell$ ";
static const int promptlen = sizeof(prompt)-1;

static void append (charp &s, char c, int& pos, int& max) {
  if (pos == max) {
    int i;
    charp r= s;
    max <<= 1;
    s= (charp) malloc (max);
    for (i=0; i<pos; i++) s[i]=r[i];
    free (r);
  }
  s[pos++]= c;
}

int output_pos;
charp output;

static void
shell_interrupt (int sig) {
  if (output != NULL) {
    int i;
    for (i=0; i<output_pos; i++) cout << output[i];
    cout << endl;
  }
  cout << DATA_BEGIN << "scheme:(with \"color\" \"red\" \""
       << "Interrupted TeXmacs shell"
       << "\")" << DATA_END
       << DATA_END
       << flush ;
#ifndef _WIN32
  killpg(pid, SIGTERM);
  sleep(1);
  killpg(pid, SIGKILL);
  wait (NULL);
#else
  TerminateProcess ((HANDLE) pid, 1);
#endif
  exit (0);
}

static void
shell_output (bool hide= false) {
  static struct timeval tv;
  int output_max= 1024;
  output = (charp) malloc (output_max);
  output_pos= 0;
  cout << DATA_BEGIN << "verbatim:" << flush;
  while (true) {
    bool data_available= false;
#ifdef _WIN32
    DWORD available = 0;
    cout << "get osf handle" << flush;
    HANDLE hMaster = (HANDLE)_get_osfhandle(master);
    cout << "peek named pipe" << flush;
    if (PeekNamedPipe(hMaster, NULL, 0, NULL, &available, NULL)) {
        if (available > 0) {
            data_available = true;
        }
    } else {
      if (available > 0) {
          data_available = true;
      } else {
        Sleep(10);
      }
    }
    cout << "data available: " << available << flush;
#else
    fd_set rfds;
    FD_ZERO (&rfds);
    FD_SET (master, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = 10000;
    int r = select (master+1, &rfds, NULL, NULL, &tv);
    if (r == 0) continue;
    if (r == -1) continue;

    if (FD_ISSET (master, &rfds))
      data_available= true;
#endif

    if (data_available) {
      int i, r;
      char outbuf[1024];
      r = read (master, outbuf, 1024);
      if (r == ERROR) {
        cerr << "TeXmacs shell] read failed\n" << flush;
#ifndef _WIN32
        killpg(pid, SIGTERM);
        sleep(1);
        killpg(pid, SIGKILL);
        wait (NULL);
#else
        TerminateProcess ((HANDLE) pid, 1);
#endif
        exit (1);
      }
      else if (r == 0) {
#ifndef _WIN32
        wait (NULL);
#endif
        cout << DATA_END << flush;
        exit (0);
      }
      else for (i=0; i<r; i++) {
        append (output, outbuf[i], output_pos, output_max);
        if (outbuf[i]=='\n') {
          append (output, '\0', output_pos, output_max);
          if (hide) hide= false;
          else cout << output << flush;
          output_pos= 0;
        }
      }
    }

    if (output_pos >= promptlen &&
        strncmp(output + output_pos - promptlen, prompt, promptlen) == 0)
      {
        output[output_pos-promptlen]=0;
        if (hide) hide= false;
        else cout << output << flush;
        output_pos= 0;
        break;
      }
  }

  cout << DATA_END << flush;
  free (output);
}

static void
shell_input () {
  char input [10000];
  cin.getline (input, 10000, '\n');
  strcat (input, "\n");
#ifdef _WIN32
  write (master_write, input, strlen (input));
#else
  write (master, input, strlen (input));
#endif
}

static void
child() {
  struct termios t;
  tcgetattr(STDIN, &t);
  t.c_lflag &= ~ECHO;
  tcsetattr(STDIN, TCSANOW, &t);
#ifndef _WIN32
  setenv ("PS1", prompt, true);
  setenv ("TEXMACS_MODE", "shell", true);
#else
  _putenv_s ("PS1", prompt);
  _putenv_s ("TEXMACS_MODE", "shell");
#endif
  const charp argv[] = {
    const_cast<charp> ("sh"),
    const_cast<charp> ("-i"),
    NULL };
  execve("sh", argv, environ);
  exit(127);
}

static void
parent() {
  signal (SIGINT, shell_interrupt);
  cout << DATA_BEGIN << "verbatim:" << "Starting TeXmacs shell..." << flush;
  shell_output (true);
  cout << DATA_END << flush;
  while (true) {
    cout << DATA_BEGIN << "input:" << flush;
    shell_input ();
    cout << DATA_END << flush;
    cout << DATA_BEGIN << "verbatim:" << flush;
    shell_output ();
    cout << DATA_END << flush;
  }
}

int
main () {
  chdir (getenv ("HOME"));
  cout << DATA_BEGIN << "verbatim:" << "Shell session inside TeXmacs" << flush;
#ifndef _WIN32
  pid = forkpty(&master,NULL,NULL,NULL);
#else
  // --- WINDOWS IMPLEMENTATION ---
  
  // 1. Create two pipes (one for input, one for output)
  int pipin[2], pipout[2];
  
  if (_pipe(pipin, 1024, O_BINARY | O_NOINHERIT) == -1) return 1;
  if (_pipe(pipout, 1024, O_BINARY | O_NOINHERIT) == -1) return 1;

  // 2. Save current stdin/stdout/stderr of the parent process
  int stdin_tmp = dup(STDIN_FILENO);
  int stdout_tmp = dup(STDOUT_FILENO);
  int stderr_tmp = dup(STDERR_FILENO);

  // 3. Redirect Standard IO to our pipes
  // Child reads from pipin[0]
  dup2(pipin[0], STDIN_FILENO);
  // Child writes to pipout[1]
  dup2(pipout[1], STDOUT_FILENO);
  dup2(pipout[1], STDERR_FILENO);

  // Close the ends the child doesn't need (the parent's ends)
  close(pipin[1]);
  close(pipout[0]);

  // 4. Setup Environment
  _putenv_s ("PS1", prompt);
  _putenv_s ("TEXMACS_MODE", "shell");

  // 5. Spawn the child process
  const char* argv[] = { "sh", "-i", "--norc", "--noprofile", NULL };
  pid = (int) _spawnvp(_P_NOWAIT, "sh", argv);

  // 6. Restore Standard IO for the parent
  dup2(stdin_tmp, STDIN_FILENO);
  dup2(stdout_tmp, STDOUT_FILENO);
  dup2(stderr_tmp, STDERR_FILENO);
  close(stdin_tmp); 
  close(stdout_tmp); 
  close(stderr_tmp);

  // 7. Establish Parent Communication Handles
  // Parent reads from pipout[0] (which child writes to)
  master = pipout[0]; 
  // Parent writes to pipin[1] (which child reads from)
  master_write = pipin[1]; 
  
  // Close the ends we duplicated to the child
  close(pipin[0]);
  close(pipout[1]);

  if (pid == -1) {
      cerr << "Error spawning shell. Ensure 'sh.exe' is in your PATH." << endl;
      return 1;
  }
#endif
  if (pid==0) child();
  cout << " pid = " << pid << "\n" << flush;
  parent();
}
