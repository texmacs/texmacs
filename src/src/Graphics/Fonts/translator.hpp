
/******************************************************************************
* MODULE     : translator.hpp
* DESCRIPTION: used for the translation of tokens, mainly to name symbols
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TRANSLATOR_H
#define TRANSLATOR_H
#include "resource.hpp"
#include "tree.hpp"

RESOURCE(translator);

/******************************************************************************
* The translator structure
******************************************************************************/

struct translator_rep: rep<translator> {
  int                  cur_c;
  hashmap<string,int>  dict;
  array<tree>          virt_def;

  inline translator_rep (string s);
};

inline translator_rep::translator_rep (string s):
  rep<translator> (s), cur_c (0), dict (-1), virt_def (0) {}

translator& operator << (translator& trl, int i);
translator& operator << (translator& trl, string s);
translator& operator << (translator& trl, translator trm);

translator load_translator (string name);
translator load_virtual (string name);

#endif // defined TRANSLATOR_H
