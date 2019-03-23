
/******************************************************************************
* MODULE     : tm_mathematica.c
* DESCRIPTION: Interface with Mathematica
* COPYRIGHT  : (C) 2005  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mathlink.h"

#define CLOSED 11L
#define PRE_NAME  "/plugins/mathematica/ps/pre"
#define POST_NAME "/plugins/mathematica/ps/post"
/* #define LOG "/tmp/log.mma"   */
/* #define LOG_PS "/tmp/log.ps" */

#ifdef LOG
static FILE *log;
#endif
#ifdef LOG_PS
static FILE *psfile;
#endif

MLENV env =(MLENV)0;
MLINK link=(MLINK)0;
static size_t size=128;
static char *input,*pre_name,*post_name;
static int protect_hat=0;

static void texput(char *s) {
  char c; int n,nl;
  nl=0;
  while (c=*(s++)) {
    if (c=='\\') {
      c=*(s++);
      if ((c>='0')&&(c<='9')) {
	n=c-'0';
	while (1) {
	  c=*(s++);
	  if ((c<'0')||(c>'9')) {
	    if (n==0x0a) { putchar(' '); nl=1; }
	    else putchar(n);
	    if (c)
	      if (nl)
		if (c=='>') nl=0;
		else if (c>' ') { putchar(c); nl=1; }
	      else if (protect_hat&&(c=='^')) putchar(' ');
	      else putchar(c);
	    break;
	  } else n=8*n+(c-'0');
	}
	if (c=='\0') break;
      } else if (protect_hat&&(c=='^')) putchar(' ');
      else putchar(c);
    } else if (nl) {
      if (c=='>') nl=0;
      else if (c>' ') {
	if (protect_hat&&(c=='^')) putchar(' ');
	else putchar(c);
	nl=1;
      }
    } else if (protect_hat&&(c=='^')) putchar(' ');
    else putchar(c);
  }
}

static void psput(char *s) {
  char c; int n,l;
  while (c=*(s++)) {
    if (c=='\\') {
      c=*(s++);
      if ((c>='0')&&(c<='7')) {
	l=0; n=c-'0';
	while (1) {
	  l++; c=*(s++);
	  if ((l>=3)||(c<'0')||(c>'7')) {
	    putchar(n);
#ifdef LOG_PS
	    fputc(n,psfile);
#endif
	    if (c) {
	      putchar(c);
#ifdef LOG_PS
	      fputc(c,psfile);
#endif
	    }
	    break;
	  } else n=8*n+(c-'0');
	}
	if (c=='\0') break;
      } else {
	putchar(c);
#ifdef LOG_PS
	fputc(c,psfile);
#endif
      }
    } else {
      putchar(c);
#ifdef LOG_PS
      fputc(c,psfile);
#endif
    }
  }
}

static void prelude(char *name) {
  FILE *ps=fopen(name,"r");
  char(c);
  while (1) {
    c=getc(ps);
    if (c==EOF) break;
    putchar(c);
#ifdef LOG_PS
    fputc(c,psfile);
#endif
  }
  fclose(ps);
}

static void command(char *s) {
  int pkt,more,non_ps,msg; long err;
  char *result,*symbol;
  fputs("\2latex:",stdout);
  MLPutFunction(link,"EvaluatePacket",1L);
  MLPutFunction(link,"Print",1L);
  MLPutFunction(link,"TeXForm",1L);
  MLPutFunction(link,"ToExpression",1L);
  MLPutString(link,s);
  MLEndPacket(link);
  more=1; non_ps=1; msg=0;
  do {
    switch (pkt=MLNextPacket(link)) {
    case RETURNPKT:
#ifdef LOG
      fputs("RETURNPKT\n",log); fflush(log);
#endif
      more=0;
      break;
    case RETURNTEXTPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"RETURNTEXTPKT: \"%s\"\n",result); fflush(log);
#endif
      MLReleaseString(link,result);
      more=0;
      break;
    case INPUTNAMEPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"INPUTNAMEPKT: \"%s\"\n",result); fflush(log);
#endif
      MLReleaseString(link,result);
      break;
    case OUTPUTNAMEPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"OUTPUTNAMEPKT: \"%s\"\n",result); fflush(log);
#endif
      MLReleaseString(link,result);
      break;
    case TEXTPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"TEXTPKT: \"%s\"\n",result); fflush(log);
#endif
      if (msg) {
	fputs("{\\magenta ",stdout);
	protect_hat=1; texput(result);
	fputs("}\n\n",stdout);
	protect_hat=0; msg=0;
      } else {
	fputs("$\\displaystyle ",stdout);
	texput(result);
	fputs("$",stdout);
      }
      MLReleaseString(link,result);
      break;
    case MESSAGEPKT:
      MLGetSymbol(link,&symbol);
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"MESSAGEPKT: \"%s\"  \"%s\"\n",symbol,result); fflush(log);
#endif
      MLReleaseSymbol(link,symbol);
      MLReleaseString(link,result);
      msg=1;
      break;
    case DISPLAYPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"DISPLAYPK: \"%s\"\n",result); fflush(log);
#endif
      if (non_ps) {
	fputs("\2ps:",stdout);
#ifdef LOG_PS
	psfile=fopen(LOG_PS,"w");
#endif
	prelude(pre_name); non_ps=0;
      }
      psput(result);
      MLReleaseString(link,result);
      break;
    case DISPLAYENDPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"DISPLAYENDPKT: \"%s\"\n",result); fflush(log);
#endif
      psput(result);
      prelude(post_name);
      fputs("\5{}{}\n\n",stdout);
#ifdef LOG_PS
      fclose(psfile);
#endif
      non_ps=1;
      MLReleaseString(link,result);
      break;
    case INPUTPKT:
      MLGetString(link,&result);
#ifdef LOG
      fprintf(log,"INPUTPKT: \"%s\"\n",result); fflush(log);
#endif
      printf("\2prompt#\\red %s{}\5\5",result);
      fflush(stdout);
      MLReleaseString(link,result);
      if (getline(&input,&size,stdin)>=0) command(input);
      break;
    case CALLPKT:
#ifdef LOG
      fputs("CALLPKT\n",log); fflush(log);
#endif
      break;
    default:
#ifdef LOG
      fprintf(log,"UNKNOWN PACKET: %1d\n",pkt); fflush(log);
#endif
      break;
    }
    MLNewPacket(link);
    err=MLError(link);
    if (err==CLOSED) {
      fputs("\\red The end\5",stdout);
      MLClose(link);
      MLDeinitialize(env);
      exit(0);
    } else if (err) {
      printf("\\red Error %1d: %s\5",err,MLErrorMessage(link));
      exit(1);
    }
  } while (more);
}

int main(int argc, char *argv[]) {
  long err;
  size_t InNum=1,l;
  char *tm_path;
#ifdef LOG
  log=fopen(LOG,"w");
#endif

  tm_path=getenv("TEXMACS_PATH");
  if (tm_path==NULL) exit(1);
  l=strlen(tm_path);
  pre_name=malloc(l+strlen(PRE_NAME)+1);
  post_name=malloc(l+strlen(POST_NAME)+1);
  strcpy(pre_name,tm_path);  strcpy(pre_name+l,PRE_NAME);
  strcpy(post_name,tm_path); strcpy(post_name+l,POST_NAME);

  input=(char*)malloc(size);

  env=MLInitialize((MLParametersPointer)0);
  if (env==(MLENV)0) {
    fputs("\2latex:\\red Initialization of MathLink failed\5",stdout);
    exit(1);
  }

  link=MLOpenString(env,"-linkname \"math -mathlink\"",&err);
  if (link==(MLINK)0) {
    fputs("\2latex:\\red Link with Mathematica failed",stdout);
    MLDeinitialize(env);
    exit(1);
  }

  fputs("\2latex:\\red Mathematica",stdout);

  while (1) {
    /* Prompt */
    printf("\2prompt#\\red In[%1d]:= {}\5\5",InNum++);
    fflush(stdout);
    if (getline(&input,&size,stdin)>=0) command(input);
  }

  return 0;
}
