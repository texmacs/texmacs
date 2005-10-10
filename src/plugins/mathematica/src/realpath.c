
/******************************************************************************
* MODULE     : realpath.c
* DESCRIPTION: finding the real path of a file
* COPYRIGHT  : (C) 2005  Andrey Grozin
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#define SIZE 4096

int main(int argc, char **argv)
{ char path[SIZE];
  if (argc<2) exit(1);
  if (realpath(argv[1],path)) printf("%s\n",path);
  else exit(1);
}
