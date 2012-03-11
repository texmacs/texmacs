
/******************************************************************************
* MODULE     : complete.cpp
* DESCRIPTION: Shows how to program tab-completion
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
#define DATA_COMMAND ((char) 16)
#define DATA_ESCAPE  ((char) 27)

void
format_plugin () {
  // The configuration of a plugin can be completed at startup time.
  // This is for instance interesting if you add tab-completion a posteriori.
  cout << DATA_BEGIN << "command:";
  cout << "(plugin-configure complete (:tab-completion #t))";
  cout << DATA_END;
}

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  format_plugin ();
  cout << "We know how to complete 'h'";
  cout << DATA_END;
  cout.flush ();

  while (true) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    if (buffer[0] != DATA_COMMAND) {
      cout << DATA_BEGIN << "verbatim:";
      cout << "You typed " << buffer;
      cout << DATA_END;
    }
    else {
      cout << DATA_BEGIN << "scheme:";
      cout << "(tuple \"h\" \"ello\" \"i there\" \"ola\" \"opsakee\")";
      cout << DATA_END;
    }
    cout.flush ();
  }
  return 0;
}
