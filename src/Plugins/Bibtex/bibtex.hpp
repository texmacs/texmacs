
/******************************************************************************
* MODULE     : bibtex.hpp
* DESCRIPTION: generating bibliographies using BiBTeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BIBTEX_H
#define BIBTEX_H
#include "url.hpp"

void set_bibtex_command (string cmd);
bool bibtex_present ();
tree bibtex_load_bbl (string bib, url bbl_file);
tree bibtex_run (string bib, string style, url bib_file, tree bib_t);

#endif // BIBTEX_H
