
/******************************************************************************
* MODULE     : bibtex.hpp
* DESCRIPTION: generating bibliographies using BiBTeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef BIBTEX_H
#define BIBTEX_H
#include "tree.hpp"

void set_bibtex_command (string cmd);
tree bibtex_run (string bib, string style, string dir, string fname,
		 tree bib_t);

#endif // BIBTEX_H
