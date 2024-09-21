#include <stdlib.h>
#include <stdio.h>
#include <stdio_ext.h>

int hex(int c, int *n)
{ if ((c>='0')&&(c<='9')) { *n=c-'0'; return 1; }
  else if ((c>='a')&&(c<='f')) { *n=c-'a'+10; return 1; }
  else if ((c>='A')&&(c<='F')) { *n=c-'A'+10; return 1; }
  else return 0;
}

int main(int argc, char **argv)
{ FILE *in,*out; int c,c2,c3,n,n2,ok=1;
  if (argc>2)
  { in=fopen(argv[2],"r");
    if (!in)
    { fprintf(stderr,"cannot read from %s\n",argv[2]); exit(1); }
  }
  else
    { in=stdin;
    if (argc>1)
    { out=fopen(argv[1],"w");
      if (!out)
      {fprintf(stderr,"cannot write to %s\n",argv[1]); exit(1); }
    }
    else out=stdout;
  }
  while (1)
  { c=fgetc(in);
    if (c==EOF) break;
    else if (c=='[')
    { c=fgetc(in);
      if (c=='[') fputc('[',out);
      else if (hex(c,&n))
      { c2=fgetc(in);
	if (hex(c2,&n2))
        { n=16*n+n2; c3=fgetc(in);
	  if (c3==']') fputc(n,out);
	  else { fprintf(stderr,"error: [%c%c%c\n",c,c2,c3); ok=0; }
        }
	else { fprintf(stderr,"error: [%c%c\n",c,c2); ok=0; }
      }
      else { fprintf(stderr,"error: [%c\n",c); ok=0; }
    }
    else if (c==']')
    { c=fgetc(in);
      if (c==']') fputc(']',out);
      else { fprintf(stderr,"error: ]%c\n",c); ok=0; }
    }
    else if (c=='\n')
      if (ok) fflush(out); else { __fpurge(out); ok=1; }
    else if ((c>=' ')&&(c<=0x7e)) fputc(c,out);
  }
}
