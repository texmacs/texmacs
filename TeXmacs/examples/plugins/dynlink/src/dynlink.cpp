
/******************************************************************************
* MODULE     : dynlink.cpp
* DESCRIPTION: Example of a plugin which is dynamically linked to TeXmacs
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <TeXmacs.h>

//#include <iostream>
//using namespace std;

static char* output= NULL;

char*
dynlink_install (TeXmacs_exports_1* TM, char* options, char** errors) {
  // cout << ">>> Install: " << options << "\n";
  output= (char*) malloc (50);
  strcpy (output, "\2verbatim:Started dynamic link\5");
  return output;
}

char*
dynlink_eval (char* what, char* session, char** errors) {
  // cout << ">>> Evaluate: " << what << ", " << session << "\n";
  free (output);
  output= (char*) malloc (50 + strlen (what));
  strcpy (output, "\2verbatim:You typed ");
  strcat (output, what);
  strcat (output, "\5");
  return output;
}

package_exports_1 dynlink_exports= {
  (char*) "TeXmacs communication protocol 1",
  (char*) "Dynlink 1",
  dynlink_install,
  dynlink_eval
};
