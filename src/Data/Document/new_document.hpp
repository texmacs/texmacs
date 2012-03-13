
/******************************************************************************
* MODULE     : new_document.hpp
* DESCRIPTION: Management of the global TeXmacs tree
* COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_DOCUMENT_H
#define NEW_DOCUMENT_H
#include "tree.hpp"
#include "path.hpp"

extern tree the_et;

path new_document ();
void delete_document (path rp);
void set_document (path rp, tree t);

#endif // NEW_DOCUMENT_H
