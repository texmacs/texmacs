#include <stdlib.h>
#include <stdio.h>

int main(int argc,char **argv)
{ FILE *in,*out; int c;
  if (argc>2)
  { out=fopen(argv[2],"w");
    if (!out)
    { fprintf(stderr,"cannot write to %s\n",argv[2]); exit(1); }
  }
  else
  { out=stdout;
    if (argc>1)
    { in=fopen(argv[1],"r");
      if (!in)
      { fprintf(stderr,"cannot read from %s\n",argv[1]); exit(1); }
    }
    else in=stdin;
  }
  while (1)
  { c=fgetc(in);
    if (c==EOF) break;
    else if (c=='[') fputs("[[",out);
    else if (c==']') fputs("]]",out);
    else if (c=='\n') fprintf(out,"[%02x]\n",c);
    else if ((c>=' ')&&(c<=0x7e)) fputc(c,out);
    else fprintf(out,"[%02x]",c);
    fflush(out);
  }
}
