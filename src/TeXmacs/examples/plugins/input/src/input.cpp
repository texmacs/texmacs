
/******************************************************************************
* MODULE     : menus.cpp
* DESCRIPTION: A plugin which changes the menus dynamically
*              The routine menus-add is defined in ../progs/init-menus.scm
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
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << DATA_BEGIN << "command:(session-use-math-input #t)" << DATA_END;
  cout << "Convert mathematical input into plain text";
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    cout << DATA_BEGIN << "verbatim:";
    cout << buffer;
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
