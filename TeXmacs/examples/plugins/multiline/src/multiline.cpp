
/******************************************************************************
* MODULE     : multiline.cpp
* DESCRIPTION: A plugin which uses multiline input as long as
*              the input is not terminated by ';'
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdio.h>
#include <iostream.h>

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_COMMAND ((char) 16)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Terminate your input by ';'";
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    if (buffer[0] != DATA_COMMAND) {
      cout << DATA_BEGIN << "verbatim:";
      cout << buffer;
      cout << DATA_END;
    }
    else {
      int  n = strlen (buffer);
      cout << DATA_BEGIN << "scheme:";
      if (buffer[n-3] == ';') cout << "#t";
      else cout << "#f";
      cout << DATA_END;
    }
    fflush (stdout);
  }
  return 0;
}
