
/******************************************************************************
* MODULE     : tm_maple_9.c
* DESCRIPTION: Interface with OpenMaple/Maple
* COPYRIGHT  : (C) 2005-2024 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include "maplec.h"

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)
//#define DATA_BEGIN   "[BEGIN]"
//#define DATA_END     "[END]"
//#define DATA_ESCAPE  "[ESCAPE]"

/******************************************************************************
* Handling maple output
******************************************************************************/

void
next_input () {
  static int counter = 0;
  counter++;
  printf ("\2channel:prompt\5");
  printf ("\2scheme:(with \"color\" \"brown\" \"");
  printf ("%s%i%s", "Maple ", counter, "] ");
  printf ("\")\5");
}

/******************************************************************************
* Maple callbacks
******************************************************************************/

#ifdef _MSC_VER
#  define CDECL __cdecl
#else
#  define CDECL
#endif

/* global variable used by queryInterrupt() */
static int Interrupted = 0;

/* interrupt signal handler: sets global variable when user hits Ctrl-C */
void CDECL catch_intr( int signo )
{
  Interrupted = TRUE;
  signal(SIGINT,catch_intr);
#ifdef _MSC_VER
  signal(SIGBREAK,catch_intr);
#endif
}

/* interrupt callback: stops computation when `Interrupted' is true */
static M_BOOL M_DECL queryInterrupt( void *data )
{
  if (Interrupted) {
    Interrupted = 0;
    return (TRUE);
  }
  return (FALSE);
}

/* callback used for directing help output */
static M_BOOL M_DECL writeHelpChar( void *data, int c )
{
    putchar (c);
    return (FALSE);
}

/* callback used for directing result output */
static void M_DECL textCallBack( void *data, int tag, const char *output )
{
  if (tag != MAPLE_TEXT_STATUS)
    printf ("%s\n", output);
}

static void M_DECL errorCallBack( void *data, M_INT offset, const char *msg )
{
  char* in=(char *)data;
  M_INT i;
  /* TODO: too wide (>= 80 characters) user input */

  if (offset < 0)
    fprintf (stderr, "%s\n", msg);
  else {
    /* put ^^^ under the original input to indicate where
       the syntax error probably was
    */
    fprintf (stderr, "Syntax Error, %s\n> %s\n", msg, in);
    for (i=0; i<offset; ++i)
      fputc (' ', stderr);
    fprintf (stderr, "^\n");
  }
}

/******************************************************************************
* Banner
******************************************************************************/

static void banner(MKernelVector kv) {
	const char* kversion=MapleToString(kv,MapleKernelOptions(kv,"version",NULL));
	char* kversionv=strdup(kversion);
	char* kplatform=strchr(kversionv,','); *kplatform='\0'; do ++kplatform; while (*kplatform == ' ');
	char* krelease=strrchr(kversionv,' '); while (*krelease == ' ') ++krelease;
	char* kdate=strchr(kplatform,','); *kdate='\0'; do ++kdate; while (*kdate == ' ');
	char* kvbid=strchr(kdate,','); *kvbid='\0'; do ++kvbid; while (*kvbid == ' ');
	char* kyear=strrchr(kdate,' '); while (*kyear == ' ') ++kyear;
	char* kbid=strrchr(kvbid,' '); while (*kbid == ' ') ++kbid;

  printf("    |\\^/|     OpenMaple/Maple %s+b%s (%s) %s\n",krelease,kbid,kplatform,kdate);
  printf("._|\\|   |/|_. Copyright (c) Maplesoft, a division of Waterloo Maple Inc. %s\n",kyear);
  printf(" \\OPENMAPLE/  All rights reserved. Maple and OpenMaple are trademarks of\n");
  printf(" <____ ____>  Waterloo Maple Inc.\n");
  printf("      |       Type ? for help.\n");
  printf("\nTeXmacs interface by Joris van der Hoeven\n");

	krelease=kplatform=kdate=kvbid=kyear=kbid=NULL;
	free(kversionv); kversionv=NULL;
}

/******************************************************************************
* Launching maple
******************************************************************************/

#define TMMPL_PI_MAPLEINIT_RELPATH "plugins/maple/maple/init-maple.mpl"

int
main (int argc, char *argv[]) {
  const char* tm_path=getenv("TEXMACS_PATH");
  const char* tm_home_path=getenv("TEXMACS_HOME_PATH");
  char in [2148];
  char err[2048];
	char tmmplinit [2048];
  MKernelVector kv;  /* Maple kernel handle */
  MCallBackVectorDesc cb = {
			      textCallBack,
			      errorCallBack,
			      0,   /* statusCallBack not used */
			      0,   /* readLineCallBack not used */
			      0,   /* redirectCallBack not used */
			      0,   /* streamCallBack not used */
			      queryInterrupt,
			      0    /* callBackCallBack not used */
                           };
  ALGEB r, l;  /* Maple data-structures */

  (void) r;  /* silence unused-but-set-variable warning */
  (void) l;  /* silence unused-variable warning */

	snprintf(tmmplinit,sizeof(tmmplinit), "%s/" TMMPL_PI_MAPLEINIT_RELPATH ,tm_path);
	if (access(tmmplinit,R_OK)) {
		printf("Fatal error, unable to read `%s`\n",tmmplinit);
		return( 1 );
	}

  signal(SIGINT,catch_intr);

  /* initialize Maple */
  if( (kv=StartMaple(argc,argv,&cb,(void *)in,NULL,err)) == NULL) {
    printf("Fatal error, %s\n",err);
    return( 1 );
  }

  /* show banner */
  printf("\2verbatim:");
  banner(kv);

  r= EvalMapleStatement (kv, "tmmaple:=9:protect('tmmaple'):");
	snprintf(in,sizeof(in),"read(`%s`);",tmmplinit);
  r= EvalMapleStatement (kv, in);

	snprintf(tmmplinit,sizeof(tmmplinit), "%s/" TMMPL_PI_MAPLEINIT_RELPATH ,tm_home_path);
	if (!(access(tmmplinit,R_OK))) {
		snprintf(in,sizeof(in),"read(`%s`);",tmmplinit);
		r= EvalMapleStatement (kv, in);
	}

  while (1) {
    next_input ();
    printf("\5");
    fflush (stdout);
    int i= 0;
    for (i=0; i<2047; i++) {
      char c= getchar ();
      if (c == '\n') break;
      in[i]= c;
    }
    in[i]= '\0';
    while (in[strlen(in)-1] == ';') in[strlen(in)-1]= '\0';
    if (strncmp (in, "quit", 4) == 0) break;
    printf("%c%s", DATA_BEGIN, "verbatim:");
    if (in[0] == '?')
      MapleHelp (kv, in+1, NULL, writeHelpChar, NULL, 80, NULL);
    else {
      if (in[strlen(in)-1] != ':')
        strcat (in, ":tmresult:=\%:tmprint(tmresult):tmresult:");
      r = EvalMapleStatement (kv, in);
    }
  }

  StopMaple(kv);

  return (0);
}

#undef TMMPL_PI_MAPLEINIT_RELPATH
