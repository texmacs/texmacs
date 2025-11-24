
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
#include <sys/wait.h>
#include <termios.h>

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
extern "C" charp* environ;

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
  killpg(pid, SIGTERM);
  sleep(1);
  killpg(pid, SIGKILL);
  wait (NULL);
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
    fd_set rfds;
    FD_ZERO (&rfds);
    FD_SET (master, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = 10000;
    int r = select (master+1, &rfds, NULL, NULL, &tv);
    if (r == 0) continue;
    if (r == -1) continue;

    if (FD_ISSET (master, &rfds)) {
      int i, r;
      char outbuf[1024];
      r = read (master, outbuf, 1024);
      if (r == ERROR) {
        cerr << "TeXmacs shell] read failed\n" << flush;
        killpg(pid, SIGTERM);
        sleep(1);
        killpg(pid, SIGKILL);
        wait (NULL);
        exit (1);
      }
      else if (r == 0) {
        wait (NULL);
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
  write (master, input, strlen (input));
}

static void
child() {
  struct termios t;
  tcgetattr(STDIN, &t);
  t.c_lflag &= ~ECHO;
  tcsetattr(STDIN, TCSANOW, &t);
  setenv ("PS1", prompt, true);
  setenv ("TEXMACS_MODE", "shell", true);
  const charp argv[] = {
    const_cast<charp> ("sh"),
    const_cast<charp> ("-i"),
    NULL };
  execve("/bin/sh", argv, environ);
  exit(127);
}

static void
parent() {
  signal (SIGINT, shell_interrupt);
  shell_output (true);
  cout << DATA_END << flush;
  while (true) {
    shell_input ();
    shell_output ();
  }
}

int
main () {
  chdir (getenv ("HOME"));
  cout << DATA_BEGIN << "verbatim:" << "Shell session inside TeXmacs" << flush;
  pid = forkpty(&master,NULL,NULL,NULL);
  if (pid==0) child();
  cout << " pid = " << pid << "\n" << flush;
  parent();
}
