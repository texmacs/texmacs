
/******************************************************************************
* MODULE     : mycas.cpp
* DESCRIPTION: A simple computer algebra system with a link to TeXmacs
* COPYRIGHT  : (C) 2001  Joris van der Hoeven
*******************************************************************************
* In order to test this program, you should first compile it using
*    g++ mycas.cpp -o mycas
* Next, move the binary to a location in your path.
* After this, TeXmacs will automatically recognize the presence of mycas.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_ESCAPE  ((char) 27)

static int counter= 0;

void
next_input () {
  counter++;
  cout << DATA_BEGIN << "channel:prompt" << DATA_END;
  cout << "Input " << counter << "] ";
}

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "------------------------------------------------------\n";
  cout << "Welcome to my test computer algebra system for TeXmacs\n";
  cout << "This software comes with no warranty whatsoever\n";
  cout << "(c) 2001  by Joris van der Hoeven\n";
  cout << "------------------------------------------------------\n";
  next_input ();
  cout << DATA_END;
  fflush (stdout);

  while (1) {
    char buffer[100];
    cin.getline (buffer, 100, '\n');
    if (strcmp (buffer, "quit") == 0) break;
    cout << DATA_BEGIN << "verbatim:";
    cout << "You typed " << buffer << "\n";

    cout << "And now a LaTeX formula: ";
    cout << DATA_BEGIN << "latex:" << "$x^2+y^2=z^2$" << DATA_END;
    cout << "\n";

    cout << "And finally a fraction ";
    cout << DATA_BEGIN << "scheme:" << "(frac \"a\" \"b\")" << DATA_END;
    cout << ".\n";

    next_input ();
    cout << DATA_END;
    fflush (stdout);
  }
  return 0;
}
