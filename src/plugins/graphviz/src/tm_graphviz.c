
/******************************************************************************
* MODULE     : tm_graphviz.c
* DESCRIPTION: A simple computer algebra system with a link to TeXmacs
* COPYRIGHT  : (C) 2001  Joris van der Hoeven and
*              Jorik Blaas (jrk at Spanic.Pet.Atudelft.Mnl (remove SPAM))
*******************************************************************************
* It now acts as a simple frontend to dot from the graphviz package.
* At the moment it uses temporary files, which is not very elegant,
* I will try to change this to pipes later on.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

static int counter= 0;
static int buffer_len= 4096; // FIXME yes i am aware of the error here
static char buffer [4096];

void
next_input () {
  counter++;
  printf ("%cchannel:prompt%c", DATA_BEGIN, DATA_END);
  printf ("Graphviz %d] ", counter);
}

void
read_input () {
  int i;
  for (i=0; i<buffer_len-1; i++) {
    buffer[i]= fgetc (stdin);
    if (buffer[i] == '\n') break;
  }
  buffer[i]='\0';
}

/******************************************************************************
* Main process       Dotty
* (stdinpipe0)  ---> (stdin, stdinpipe1)
* (stdoutpipe1) <--- (stdout, stdoutpipe0)
* (errpipe1)    <--- (stderr, errpipe0)
*
* int stdinpipe[2];
* int stdoutpipe[2];
* int stderrpipe[2];
* pipe( stdinpipe );  
*
* FIXME (it should be implemented as above, but I really cannot
* be bothered to do this atm..pipes  can be such a kludge)
******************************************************************************/

char *
readfile( FILE * fp ) {
  char * buf = (char*)malloc(1025);
  char *wbuf = buf;
  int buflen=1025;

  while(!feof(fp))
  {
    int br = fread( wbuf, 1, 1024, fp );
    if(br==-1 || br==0)
	break;
    wbuf += br;
    buflen+=1024;
    buf = (char*)realloc( buf, buflen );
  }
  *wbuf='\0';
  return buf;
}

// output and errors are allocated by this routine
void
run_dotty( char * input, char ** output, char ** errors ) {
  FILE *wfp, *fp, *efp;

  wfp = fopen("dotty.tmp.dot", "w");
  fprintf(wfp, "%s", input );
  fclose(wfp);

  fp = popen( "dot -Tps dotty.tmp.dot 2>dotty.tmp.err", "r" );
  *output = readfile(fp);
  fclose(fp);

  efp = fopen("dotty.tmp.err","r");
  *errors = readfile(efp);
  fclose(efp); 
}

void
build_input () {
  FILE *foutp = fopen ("dotty.tmp.in", "a");
  fprintf (foutp, "%s", buffer);
  fclose (foutp);
}

int
main () {
  printf ("%cverbatim:", DATA_BEGIN);
  printf ("Welcome to a simple TeXmacs interface to Graphviz/dot\n");
  printf ("(C) 2002 Jorik Blaas and Joris van der Hoeven\n");
  next_input ();
  printf ("%c", DATA_END);
  fflush (stdout);

  while (!feof (stdin)) {
    char * out;
    char * err;

    read_input ();
    if (strcmp (buffer, "quit") == 0) break;

    printf ("%cps:", DATA_BEGIN);
    run_dotty (buffer, &out, &err);
    build_input ();
    printf ("%s", out);
    free (out);
    if (strlen(err)>0)
    {
      printf ("%cverbatim:", DATA_BEGIN);
      printf ("\nErrors: %s\n", err);
    }
    free(err);
    printf ("%c\n", DATA_END);

    next_input ();
    printf ("%c", DATA_END);
    fflush (stdout);
  }
  return 0;
}
