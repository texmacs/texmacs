
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
{ int c;
  /* first */
  fputs("\2verbatim:",stdout);
  while(1)
  { c=getchar();
    if (c==1) break;
    if (c>=' ' || c=='\n') putchar(c);
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
    while(1)
    { c=getchar();
      if ((c==EOF)||(c==1)) break;
      if (c==0x10)
      { fputs("\2latex:$\\displaystyle ",stdout);
        while(1)
        { c=getchar();
          if ((c==EOF)||(c==0x11)) break;
          if (c>=' ') putchar(c);
        }
        fputs("$\5",stdout);
      }
      if ((c==EOF)||(c==1)) break;
      if (c>=' ') putchar(c);
    }
    if (c==EOF) break;
  }
  fputs("\2latex:\\red The end\\black\5\5",stdout);
}
