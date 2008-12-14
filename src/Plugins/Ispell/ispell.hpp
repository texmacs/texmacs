
/******************************************************************************
* MODULE     : ispell.hpp
* DESCRIPTION: interface with the ispell spell checker
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ISPELL_H
#define ISPELL_H
#include "tree.hpp"

string ispell_start (string lan);
tree   ispell_check (string lan, string s);
void   ispell_accept (string lan, string s);
void   ispell_insert (string lan, string s);
void   ispell_done (string lan);

#endif // ISPELL_H
