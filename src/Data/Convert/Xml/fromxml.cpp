
/******************************************************************************
* MODULE     : fromxml.cpp
* DESCRIPTION: conversion of logical xml and html trees into edit trees
* COPYRIGHT  : (C) 2003  Joris van der Hoeven, David Allouche
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"

tree
tmml_upgrade (tree doc) {
  if ((!is_tuple (doc, "document")) || (N(doc)<2))
    return tree (TMERROR, "bad format or data");
  string version= "1.0.1.24";
  if (is_tuple (doc[1], "TeXmacs", 1) && is_atomic (doc[1][1]))
    version= doc[1][1]->label;
  return scheme_tree_to_tree (doc, version);
}
