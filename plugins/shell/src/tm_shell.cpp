
/******************************************************************************
* MODULE     : tm_shell.cpp
* DESCRIPTION: TeXmacs shell
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/wait.h>
using namespace std;

typedef char* charp;
extern charp* environ;

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

int   pid;            // process identifier of the child
int   tochild[2];     // for data going to the child
int   fromchild[2];   // for data coming from the child
int   in;             // file descriptor for data going to the child
int   out;            // file descriptor for data coming from the child
FILE* fin;            // file associated to in

/******************************************************************************
* Handling shell input and output
******************************************************************************/

void
append (charp &s, char c, int& pos, int& max) {
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

void
shell_output (bool hide= false) {
  int output_pos= 0;
  int output_max= 1024;
  charp output= (charp) malloc (output_max);
  cout << DATA_BEGIN << "verbatim:";

  while (true) {
    fd_set rfds;
    FD_ZERO (&rfds);
    FD_SET (out, &rfds);
    int max_fd= out+1;
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 100;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr==0) {
      fflush (stdout);
      continue;
    }

    if (FD_ISSET (out, &rfds)) {
      int i, r;
      char outbuf[1024];
      r = read (out, outbuf, 1024);
      if (r == ERROR) {
	cerr << "TeXmacs shell] read failed\n";
	wait (NULL);
	exit (1);
      }
      else if (r == 0) {
	kill (pid, SIGKILL);
	cout << DATA_END;
	fflush (stdout);
	exit (0);
      }
      else for (i=0; i<r; i++) {
	append (output, outbuf[i], output_pos, output_max);
	if (outbuf[i]=='\n') {
	  append (output, '\0', output_pos, output_max);
	  if (hide) hide= false;
	  else cout << output;
	  fflush (stdout);
	  output_pos= 0;
	}
      }
    }

    if (strncmp (output, "tmshell$ ", 9) == 0)
      break;
  }

  cout << DATA_END;
  fflush (stdout);
  free (output);
}

void
shell_input () {
  char input [10000];
  cin.getline (input, 10000, '\n');
  strcat (input, "\n");
  write (in, input, strlen (input));
  fflush (fin);
}

void
shell_interrupt (int sig) {
  cout << DATA_BEGIN << "scheme:(with \"color\" \"red\" \"";
  cout << "Interrupted TeXmacs shell";
  cout << "\")" << DATA_END;
  cout << DATA_END;
  // kill (pid, SIGINT); // Why does this not work ???
  kill (pid, SIGKILL);
  exit (0);
}

/******************************************************************************
* Launching the shell using a fork
******************************************************************************/

volatile void
invoke_shell () {
  charp argv[3];
  argv[0] = "sh";
  argv[1] = "-i";
  argv[2] = 0;
  execve("/bin/sh", argv, environ);
  exit(127);
}

int
main () {
  setenv ("PS1", "tmshell$ ", true);
  cout << DATA_BEGIN << "verbatim:";
  cout << "Shell session inside TeXmacs";
  pipe (tochild);
  pipe (fromchild);
  pid= fork ();
  if (pid==0) { // the child
    dup2 (tochild [IN], STDIN);
    close (tochild [IN]);
    close (fromchild [IN]);
    close (tochild [OUT]);
    dup2 (fromchild [OUT], STDOUT);
    dup2 (STDOUT, STDERR);
    close (fromchild [OUT]);
    invoke_shell ();
    exit (127);
  }
  else { // the main process
    int sig;
    out= fromchild [IN];
    close (fromchild [OUT]);
    in= tochild [OUT];
    close (tochild [IN]);
    fin= fdopen (in, "w");
    signal (SIGINT, shell_interrupt);
    shell_output (true);
    cout << DATA_END;
    while (true) {
      shell_input ();
      shell_output ();
    }
  }
  return 0;
}
