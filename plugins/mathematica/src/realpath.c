
/******************************************************************************
* MODULE     : realpath.c
* DESCRIPTION: finding the real path of a file
* COPYRIGHT  : (C) 2005  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#define SIZE 4096

int main(int argc, char **argv)
{ char path[SIZE];
  if (argc<2) exit(1);
  if (realpath(argv[1],path)) printf("%s\n",path);
  else exit(1);
  return 0;
}
