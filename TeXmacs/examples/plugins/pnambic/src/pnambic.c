#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv)
{ FILE *in,*out; int c1,c2;
  if (argc!=3) { fprintf(stderr,"usage: pnambic infile outfile\n"); exit(1); }
  in=fopen(argv[1],"r");
  if (!in) { fprintf(stderr,"cannot read from %s\n",argv[1]); exit(1); }
  out=fopen(argv[2],"w");
  if (!out) { fprintf(stderr,"cannot write to %s\n",argv[2]); exit(1); }
  if (fork())
  { fclose(out); fclose(stdin);
    while (1)
    { c1=fgetc(in);
      if (c1==EOF) break;
      fputc(c1,stdout); fflush(stdout);
    }
    fclose(in); fclose(stdout); exit(0);
  }
  else
  { fclose(in); fclose(stdout);
    while (1)
    { c2=fgetc(stdin);
      if (c2==EOF) break;
      fputc(c2,out); fflush(out);
    }
    fclose(out); fclose(stdin); exit(0);
  }
}
