
/******************************************************************************
* MODULE     : maple_filter.cpp
* DESCRIPTION: Filter for Maple sessions
* COPYRIGHT  : (C) 2005 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <iostream>
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

/******************************************************************************
* A very simple string class
******************************************************************************/

class string {
public:
  int   l; // reserved number of bytes
  char* s; // the string
  int   n; // length
  string (int reserve= 7):
    l (reserve+1), s (new char[l]), n (0) { s[0]='\0'; }
  string (char *s2):
    l (strlen(s2)+1), s (new char[l]), n (l-1) { strcpy (s, s2); }
  string (const string& s2):
    l (s2.n+1), s (new char[l]), n (l-1) { strcpy (s, s2.s); }
  ~string () { delete[] s; }
  inline char& operator [] (int i) { return s[i]; }
};

string
operator * (const string& s1, const string& s2) {
  string s (s1.n + s2.n);
  strcpy (s.s, s1.s);
  strcpy (s.s + s1.n, s2.s);
  s.n= s1.n + s2.n;
  return s;
}

string&
operator << (string& s, const string& s2) {
  if (s.n + s2.n >= s.l) {
    char* old= s.s;
    s.l= ((s.n + s2.n) << 1) + 1;
    s.s= new char[s.l];
    strcpy (s.s, old);
    delete[] old;
  }
  strcpy (s.s + s.n, s2.s);
  s.n += s2.n;
  return s;
}

string&
operator << (string& s, char c) {
  char s2[2];
  s2[0]= c;
  s2[1]= '\0';
  return s << string (s2);
}

string
get_line () {
  string input;
  while (true) {
    char c;
    cin.get (c);
    if (c == '\n') break;
    input << c; 
  }
  return input;
}

/******************************************************************************
* Handling maple output
******************************************************************************/

static int counter= 0;

void
next_input () {
  counter++;
  cout << DATA_BEGIN << "channel:prompt" << DATA_END;
  cout << DATA_BEGIN << "scheme:(with \"color\" \"brown\" \"";
  cout << "Maple " << counter << "] ";
  cout << "\")" << DATA_END;
}

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
maple_output () {
  bool show_flag= false;
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
      cout.flush ();
      continue;
    }

    if (FD_ISSET (out, &rfds)) {
      int i, r;
      char outbuf[1024];
      bool error_flag= false;
      r = read (out, outbuf, 1024);
      if (r == ERROR) {
	next_input ();
	cout << DATA_END << DATA_END;
	cout.flush ();
	free (output);
	return;
	/*
	cerr << "TeXmacs maple] read failed\n";
	wait (NULL);
	exit (1);
	*/
      }
      else if (r == 0) {
	kill (pid, SIGKILL);
	cout << DATA_END;
	cout.flush ();
	exit (0);
      }
      else for (i=0; i<r; i++) {
	append (output, outbuf[i], output_pos, output_max);
	if (output_pos>=5 && strncmp (output+output_pos-5, "error",5) == 0)
	  error_flag= true;
	if (outbuf[i]=='\n') {
	  append (output, '\0', output_pos, output_max);
	  if (strcmp (output, "tmstart\n") == 0)
	    show_flag= true;
	  else if (strcmp (output, "tmend\n") == 0) {
	    next_input ();
	    cout << DATA_END;
	    cout.flush ();
	    free (output);
	    return;
	  }
	  else if (show_flag) {
	    cout << output;
	    cout.flush ();
	  }
	  output_pos= 0;
	}
      }
      if (error_flag) {
	next_input ();
	cout << DATA_END;
	cout.flush ();
	free (output);
	//char* s= "1;\n2;\n";
	//write (in, s, strlen (s));
	return;
      }
    }
  }
}

/******************************************************************************
* Handling maple input
******************************************************************************/

void
send (const string& s) {
  write (in, s.s, s.n);
}

void
strip (string& s, char c1, char c2) {
  while (s.n>0 && (s[s.n-1] == c1 || s[s.n-1] == c2)) {
    s.s[s.n-1]= '\0';
    s.n--;
  }
}

void
maple_input () {
  string input= get_line ();
  send ("printf(`tmstart\\n`):\n");
  strip (input, ' ', '\n');
  if (input.n == 0 || input[0] == '?')
    send (input * "\n");
  else if (input[input.n-1] == ':') {
    strip (input, ':', ';');
    send (input * ":\n");
  }
  else {
    strip (input, ':', ';');
    send ("tmresult := tmdummy:\n");
    send (input * ":\n");
    send ("if \" <> tmdummy then tmprint(\") fi:\n");
  }
  send ("printf(`tmend\\n`):\n");
}

/******************************************************************************
* Interrupting maple
******************************************************************************/

void
maple_interrupt (int sig) {
  kill (pid, sig);
  cout << DATA_END;// << DATA_END << DATA_END;
  signal (sig, maple_interrupt);
  siginterrupt (sig, 1);
}

/******************************************************************************
* Launching maple
******************************************************************************/

volatile void
invoke_maple () {
  charp argv[3];
  argv[0] = "maple";
  argv[1] = "-q";
  argv[2] = 0;
  char* maple_bin= getenv ("TEXMACS_MAPLE_BIN");
  execve (maple_bin, argv, environ);
  exit(127);
}

void
init_maple () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Maple session inside TeXmacs";
  send ("tmmaple:=5:\n");
  send ("interface(errorbreak=0,screenheight=9999):\n");
  char* tm_path= getenv ("TEXMACS_PATH");
  //send ("read (`");
  //send (tm_path);
  //send ("/plugins/maple/maple/init-maple.mpl`):\n");
  send ("read (`" * string (tm_path) * "/plugins/maple/maple/init-maple.mpl`):\n");
  next_input ();
  cout << DATA_END;
  cout.flush ();
}

int
main () {
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
    invoke_maple ();
    exit (127);
  }
  else { // the main process
    int sig;
    out= fromchild [IN];
    close (fromchild [OUT]);
    in= tochild [OUT];
    close (tochild [IN]);
    signal (SIGINT, maple_interrupt);
    siginterrupt (SIGINT, 1);
    init_maple ();
    while (true) {
      maple_input ();
      maple_output ();
    }
  }
  return 0;
}
