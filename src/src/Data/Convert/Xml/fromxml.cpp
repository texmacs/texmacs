
/******************************************************************************
* MODULE     : fromxml.cpp
* DESCRIPTION: conversion of logical xml and html trees into edit trees
* COPYRIGHT  : (C) 2003  Joris van der Hoeven, David Allouche
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "convert.hpp"

tree
tmml_upgrade (tree doc) {
  if ((!is_tuple (doc, "document")) || (N(doc)<2))
    return tree (ERROR, "bad format or data");
  string version= "1.0.1.24";
  if (is_tuple (doc[1], "TeXmacs", 1) && is_atomic (doc[1][1]))
    version= doc[1][1]->label;
  return scheme_tree_to_tree (doc, version);
}
