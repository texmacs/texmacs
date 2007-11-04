
/******************************************************************************
* MODULE     : dictionary.hpp
* DESCRIPTION: used for translations and analysing text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DICTIONARY_H
#define DICTIONARY_H
#include "resource.hpp"
#include "url.hpp"

RESOURCE(dictionary);

/******************************************************************************
* The dictionary structure
******************************************************************************/

class dictionary_rep: public rep<dictionary> {
  hashmap<string,string> table;
  string from, to;

public:
  dictionary_rep (string from, string to);

  void   load (url fname);
  void   load (string fname);
  string translate (string s);
};

dictionary load_dictionary (string from, string to);
void set_input_language (string s);
string get_input_language ();
void set_output_language (string s);
string get_output_language ();
string translate (string lan, string from, string to);

#endif // defined DICTIONARY_H
