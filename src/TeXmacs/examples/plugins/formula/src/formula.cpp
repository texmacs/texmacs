
/******************************************************************************
* MODULE     : formula.cpp
* DESCRIPTION: A plugin capable of displaying mathematical formulas
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
  cout << "Please enter a positive integer at each input";
  cout << DATA_END;
  fflush (stdout);

  while (true) {
    int i, nr;
    cin >> nr;
    cout << DATA_BEGIN << "latex:";
    cout << "$";
    for (i=1; i<nr; i++)
      cout << "x_{" << i << "}+";
    cout << "x_{" << i << "}$";
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
