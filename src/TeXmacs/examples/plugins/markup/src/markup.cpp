
/******************************************************************************
* MODULE     : markup.cpp
* DESCRIPTION: This plugin demonstrates the interaction between
*              a plugin and a style file (../packages/session/markup.ts).
*              The style file both contains customizations of the input
*              and output environments and an additional tag 'foo'.
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
  cout << "Enter a LaTeX expression at each prompt";
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    cout << DATA_BEGIN << "latex:";
    cout << "$\\foo{" << buffer << "}$";
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
