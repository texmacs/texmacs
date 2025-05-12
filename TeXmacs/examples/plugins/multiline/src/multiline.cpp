
/******************************************************************************
* MODULE     : multiline.cpp
* DESCRIPTION: A plugin which uses multiline input as long as
*              the input is not terminated by ';'
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <iostream>
#include <cstring>
using namespace std;

#define DATA_BEGIN   ((char) 2)
#define DATA_END     ((char) 5)
#define DATA_COMMAND ((char) 16)
#define DATA_ESCAPE  ((char) 27)

int
main () {
  cout << DATA_BEGIN << "verbatim:";
  cout << "Terminate your input by ';'";
  cout << DATA_END;
  cout.flush ();

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
      if (n>0 && buffer[n-1] == ')') n--;
      if (n>0 && buffer[n-1] == '\"') n--;
      if (n>0 && buffer[n-1] == ';') cout << "#t";
      else cout << "#f";
      cout << DATA_END;
    }
    cout.flush ();
  }
  return 0;
}
