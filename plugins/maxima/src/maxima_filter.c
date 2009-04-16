
/******************************************************************************
* MODULE     : maxima_filter.c
* DESCRIPTION: Glue between TeXmacs and Maxima
* COPYRIGHT  : (C) 1999  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  return 0;
}
