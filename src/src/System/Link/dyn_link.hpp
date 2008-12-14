
/******************************************************************************
* MODULE     : dyn_link.hpp
* DESCRIPTION: Linkion of extern packages to TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DYN_LINK_H
#define DYN_LINK_H
#include "tm_link.hpp"

string symbol_install (string lib, string symb, pointer& f);
string symbols_install (string lib, string* symb, pointer* f, int n);

/******************************************************************************
* Dynamic links
******************************************************************************/

struct dyn_link_rep: tm_link_rep {
  string  lib;       // Name of the library
  string  symbol;    // Name of the function which determines exported routines
  string  init;      // Initialization string
  pointer routs;     // Routines exported by package
  string  session;   // Name of the session
  string  ret;       // the last answer returned after 'write'

public:
  dyn_link_rep (string lib, string symbol, string init, string session);
  ~dyn_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();
};

#endif // defined DYN_LINK_H
