
/******************************************************************************
* MODULE     : handler.cpp
* DESCRIPTION: Shows how to deal with cerr
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <iostream.h>

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Type commands ending with ';'";
  cout << DATA_END;
  cout.flush ();

  while (1) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    cout << DATA_BEGIN << "verbatim:";
    if (buffer[strlen(buffer)-1] == ';')
      cout << "You typed " << buffer;
    else {
      cerr << DATA_BEGIN << "verbatim:";
      cerr << "Commands have to end with ';'";
      cerr << DATA_END;
    }
    cout << DATA_END;
    cout.flush ();
  }
  return 0;
}
