
/******************************************************************************
* MODULE     : formula.cpp
* DESCRIPTION: A plugin capable of displaying mathematical formulas
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <iostream>
using namespace std;

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Please enter a positive integer at each input";
  cout << DATA_END;
  cout.flush ();

  while (true) {
    int i, nr;
    cin >> nr;
    cout << DATA_BEGIN << "latex:";
    cout << "$";
    for (i=1; i<nr; i++)
      cout << "x_{" << i << "}+";
    cout << "x_{" << i << "}$";
    cout << DATA_END;
    cout.flush ();
  }
  return 0;
}
