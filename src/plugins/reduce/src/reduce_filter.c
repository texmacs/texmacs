
/******************************************************************************
* MODULE     : reduce_filter.c
* DESCRIPTION: Glue between TeXmacs and Reduce
* COPYRIGHT  : (C) 1999  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdio.h>

int main()
{ int c,s;
  /* first */
  fputs("\2verbatim:",stdout);
  s=0;
  while(1)
  { c=getchar();
    if (c==1) break;
    if (c>=' ') { putchar(c); s=1; }
    else if ((c=='\n')&&s) {putchar(c); s=0; }
  }
  /* other */
  while(1)
  { /* prompt */
    fputs("\2channel:prompt\5\2latex:\\red ",stdout);
    while(1)
    { c=getchar();
      if (c==2) break;
      if (c>=' ') putchar(c);
    }
    fputs(" \\black\5\5",stdout);
    fflush(stdout);
    /* main output loop */
    fputs("\2verbatim:",stdout);
    s=0;
    while(1)
    { c=getchar();
      if ((c==EOF)||(c==1)) break;
      if (c==0x10)
      { if (s) putchar('\n');
        fputs("\2latex:$\\displaystyle ",stdout);
        while(1)
        { c=getchar();
          if ((c==EOF)||(c==0x11)) break;
          if (c>=' ') putchar(c);
        }
        fputs("$\5",stdout);
        s=2;
      }
      if ((c==EOF)||(c==1)) break;
      if ((c=='\n')&&(s==1)) putchar(c);
      else if (c>=' ')
      { if (s==2) putchar('\n');
        putchar(c); s=1;
      }
    }
    if (c==EOF) break;
  }
  fputs("\2latex:\\red The end\\black\5\5",stdout);
}
