
/******************************************************************************
* MODULE     : dynlink.cpp
* DESCRIPTION: Example of a plugin which is dynamically linked to TeXmacs
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <TeXmacs.h>

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
  "TeXmacs communication protocol 1",
  "Dynlink 1",
  dynlink_install,
  dynlink_eval
};
