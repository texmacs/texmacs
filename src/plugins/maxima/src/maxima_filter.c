
/******************************************************************************
* MODULE     : maxima_filter.c
* DESCRIPTION: Glue between TeXmacs and Maxima
* COPYRIGHT  : (C) 1999  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdio.h>

int main()
{ int c,first;
  /* first */
  fputs("\2verbatim:",stdout);
  while (1)
  { c=getchar();
    if (c==3) break;
    putchar(c);
  }
  /* other */
  while (1)
  { /* prompt */
    fputs("\2channel:prompt\5\2latex:\\red ",stdout);
    while (1)
    { c=getchar();
      if (c==4) break;
      putchar(c);
    }
    fputs("\\black\5\5",stdout);
    fflush(stdout);
    /* main output loop */
    first=1;
    c=getchar();
    while (1)
    { if (c==EOF) break;
      if (first)
      { fputs("\2verbatim:",stdout);
        if (c==3) break;
        if (c!='\n') putchar(c);
        first=0;
      }
      else if (c==3) break;
      else if (c=='%') { putchar('\\'); putchar(c); }
      else putchar(c);
      c=getchar();
    }
    if (c==EOF) break;
  }
  if (first) fputs("\2verbatim:",stdout);
  fputs("\2latex:\\red The end\\black\5\5",stdout);
}
