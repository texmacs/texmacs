
/******************************************************************************
* MODULE     : dictionary.hpp
* DESCRIPTION: used for translations and analysing text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

string translate (string s, string from, string to);
string translate (string s);
string translate (const char* s);
tree   tree_translate (tree t, string from, string to);
tree   tree_translate (tree s);
string translate (tree t, string from, string to);
string translate (tree t);

#endif // defined DICTIONARY_H
