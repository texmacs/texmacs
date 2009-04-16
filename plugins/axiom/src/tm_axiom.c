
/******************************************************************************
* MODULE     : tm_axiom.c
* DESCRIPTION: Glue between TeXmacs and Axiom
* COPYRIGHT  : (C) 1999  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define NORMAL 0
#define LONG   1
#define END    2
#define PROMPT 3
#define MATH   4
#define TYPE   5

#define LEN 256
/* #define LOG "/tmp/tm_axiom.log" */

char buf[LEN];
char mathbuf[4096];
int len,code,writing=0,wait_type=0,mmode=0; /* was writing=1 */
char prompt[]="-> ";
char math[]="$$\n";
char Type[]="Type: ";
FILE *axin,*axout;

#ifdef LOG
FILE *log;
int details=0;

void lline(void)
{ switch (code)
  { case NORMAL: fputs("NORMAL",log); break;
    case LONG:   fputs("LONG  ",log); break;
    case END:    fputs("END   ",log); break;
    case PROMPT: fputs("PROMPT",log); break;
    case MATH:   fputs("MATH  ",log); break;
    case TYPE:   fputs("TYPE  ",log); break;
  }
  fprintf(log," %3d: ",len);
  fputs(buf,log);
  if ((code==PROMPT)||(code==MATH)) fputs("\n",log);
  fflush(log);
}
#endif

int ch(void)
{ char c;
  while (1)
  { c=getc(axout);
#ifdef LOG
    if (details)
    { fprintf(log,"%02x",c);
      if (c>=' ') fprintf(log," [%c]",c);
      fprintf(log,"\n"); fflush(log);
    }
#endif
    if (c!='\r') return c;
  }
}

void tail(void)
{ int c;
  while (1)
  { c=ch();
    if (c==EOF) { code=END; break; }
    else if (c=='\r')
    { c=ch();
      if (c==EOF) { code=END; break; }
      else if (c=='\n') { if (writing) putchar('\n'); break; }
      else if (writing) putchar(c);
    }
    else if (writing) putchar(c);
  }
}

void iline(void)
{ int i=0,j=0,k,c;
  while (1)
  { c=ch();
    if (c==prompt[j])
      if (j==2) { code=PROMPT; break; } else j++;
    else
    { for (k=0;k<j;) buf[i++]=prompt[k++];
      j=0;
      if (i>LEN-4) { code=LONG; break; }
      else if (c==EOF) { code=END; break; }
      else if (c=='\n') { buf[i++]='\n'; code=NORMAL; break; }
      else buf[i++]=c;
    }
  }
  buf[i]='\0'; len=i;
  if (len==78)
  if (buf[77]=='\n')
  { j=1;
    for (k=0;k<76;k++) if (buf[k]!='-') { j=0; break; }
    if (j) code=TYPE;
  }
#ifdef LOG
  lline();
#endif
  if (code==PROMPT) return;
  if (writing) fputs(buf,stdout);
  if (code==LONG) tail();
  if (code==END)
  { fputs("\n\2latex:\\red Unexpected end\\black5\5",stdout);
    exit(1);
  }
  if (code==LONG) code=NORMAL;
}

void line(void)
{ int i=0,j,k,c; char *s;
  code=NORMAL; c=ch();
  if (c==EOF) code=END;
  else if (c=='-')
  { j=1;
    while (1)
    { c=ch();
      if (c==prompt[j])
        if (j==2) { code=PROMPT; break; } else j++;
      else
      { for (k=0;k<j;) buf[i++]=prompt[k++];
        break;
      }
    }
  }
  else if (c=='$')
  { j=1;
    while (1)
    { c=ch();
      if (c==math[j])
        if (j==2) { code=MATH; break; } else j++;
      else
      { for (k=0;k<j;) buf[i++]=math[k++];
        buf[i++]=c;
      }
    }
  }
  if (c!=EOF) buf[i++]=c;
  if ((code==PROMPT)||(code==MATH)||(code==END))
  { buf[i]='\0'; len=i;
#ifdef LOG
    lline();
#endif
    return;
  }
  while (1)
  { if (i>LEN-2) { code=LONG; break; }
    else if (c=='\n') break;
    c=ch();
    if (c==EOF) { code=END; break; }
    else buf[i++]=c;
  }
  buf[i]='\0'; len=i;
#ifdef LOG
  lline();
#endif
  if (/*wait_type && */(code==NORMAL))
  { if (len==1) return;
    wait_type=0;
    if (len==78)
    { for (s=buf,k=0;k<len-7;s++,k++) if ((*s)!=' ') break;
      for (k=0;k<6;s++,k++) if ((*s)!=Type[k]) break;
      if (k==6)
      { /* buf[77]='\0'; */
	wait_type=1;
        printf("\2latex:\\axiomtype{%s}\5",s);
        return;
      }
    }
  };
  if (wait_type) printf("\2latex:\\red$\\rightarrow$\\black\\ \5");
  if (mmode) {
    strcat(mathbuf,buf);
  } else {
    fputs(buf,stdout);
  };
  if (code==LONG) tail();
  if (code==LONG) code=NORMAL;
}

void must_be_prompt(char *mess)
{ iline();
  if (code!=PROMPT)
  { printf("\2latex:\\red Cannot get prompt %s\\black\5\5",mess);
    exit(1);
  }
}

void tex_to_latex(char buf[])
{
  char *ptr1, *ptr2;

  while ((ptr1=strchr(buf,'\n'))) { strcpy(ptr1,ptr1+1); };
  while ((ptr1=strstr(buf,"\\root {"))) {
    if ((ptr2=strstr(ptr1,"} \\of "))) {
      strncpy(ptr1,"\\sqrt[",6);
      strncpy(ptr1+6,ptr1+7,ptr2-ptr1-7);
      strncpy(ptr1+6+(ptr2-ptr1-7),"]",1);
      strcpy(ptr1+7+(ptr2-ptr1-7),ptr2+6);
    }
  }
}

void session(void)
{ int c,delims=0;
#ifdef LOG
  log=fopen(LOG,"w");
#endif
  /* Write initial lines up to (but not including)
     the line with the first prompt
  */
  fputs("\2verbatim:",stdout);
  while (1)
  { iline();
    if (code==TYPE) { if ((++delims)==2) writing=0; }
    else if (code==PROMPT) break;
  }
  /* force-feeding */
  fputs(")set messages prompt plain\n",axin); fflush(axin);
#ifdef LOG
  fputs("SENT )set messages prompt plain\n",log); fflush(log);
#endif
  must_be_prompt("0");
  fputs(")set messages autoload off\n",axin); fflush(axin);
#ifdef LOG
  fputs("SENT )set messages autoload off\n",log); fflush(log);
#endif
  must_be_prompt("1");
  fputs(")set quit unprotected\n",axin); fflush(axin);
#ifdef LOG
  fputs("SENT )set quit unprotected\n",log); fflush(log);
#endif
  must_be_prompt("2");
  fputs(")set output tex on\n",axin); fflush(axin);
#ifdef LOG
  fputs("SENT )set output tex on\n",log); fflush(log);
#endif
  must_be_prompt("3");
  fputs(")set output algebra off\n",axin); fflush(axin);
#ifdef LOG
  fputs("SENT )set output algebra off\n",log); fflush(log);
#endif
  must_be_prompt("4");
  /* Main prompt-read-write loop */
  while (1)
  { fputs("\2channel:prompt\5\2latex:\\red$\\rightarrow$\\ \5\5",stdout);
    fflush(stdout);
    fputs("\2verbatim:",stdout);
    while (1)
    { c=getchar();
      if ((c==EOF)||(c=='\n')) break;
      putc(c,axin);
    }
    if (c==EOF) fputs(")quit\n",axin);
    else putc('\n',axin);
    fflush(axin);
    mmode=0;
    while (1)
    { line();
      if ((code==PROMPT)||(code==END)) break;
      if (code==MATH) {
        if (mmode) { /* convert TeX to LaTex etc. */
	  tex_to_latex(mathbuf);
	  fputs(mathbuf,stdout);
	  mmode=0; wait_type=1; fputs("$\5\n",stdout);
       	} else {
	  strcpy(mathbuf,"");
	  mmode=1; fputs("\2latex:$\\displaystyle\n",stdout);
       	}
     };
    }
    if (code==END) break;
  }
  fputs("\2latex:\\red The end\\black\5\5",stdout);
}

void fatal(char *mess)
{ fprintf(stdout,"\2latex:\\red Cannot %s\\black\5\5",mess);
  exit(1);
}

int main()
{ int p1[2],p2[2];
  if (pipe(p1)<0) fatal("create pipe");
  if (pipe(p2)<0) fatal("create pipe");
  switch (fork())
  { case -1: fatal("fork");
    case 0: /* Axiom */
      dup2(p1[1],1); close(p1[1]); close(p1[0]);
      dup2(p2[0],0); close(p2[0]); close(p2[1]);
      execlp("AXIOMsys","AXIOMsys","-noclef",NULL);
      fatal("exec AXIOMsys");
    default: /* parent */
      close(p1[1]); close(p2[0]);
      axin=fdopen(p2[1],"w"); axout=fdopen(p1[0],"r");
      session();
  }
  return 0;
}
