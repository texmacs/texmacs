
/******************************************************************************
* MODULE     : font_database.cpp
* DESCRIPTION: Database with the available fonts
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "iterator.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Global management of the font database
******************************************************************************/

static bool fonts_loaded= false;
hashmap<tree,tree> font_table (UNINIT);

#ifndef QTTEXMACS

void
font_database_build (url u) {
  (void) u;
}

#endif

void
font_database_load () {
  if (fonts_loaded) return;
  fonts_loaded= true;
  url home ("$TEXMACS_HOME_PATH");
  url def ("$TEXMACS_PATH");
  url u= (home | def) * url ("fonts/font-database.scm");
  u= resolve (u);
  if (!is_none (u)) {
    string s;
    if (!load_string (u, s, false)) {
      tree t= block_to_scheme_tree (s);
      for (int i=0; i<N(t); i++)
        if (is_func (t[i], TUPLE, 2))
          font_table (t[i][0])= t[i][1];
    }
  }
}

void
font_database_save () {
  scheme_tree r (TUPLE);
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    r << tuple (key, font_table [key]);
  }
  string s= scheme_tree_to_block (r);
  save_string ("$TEXMACS_HOME_PATH/fonts/font-database.scm", s);
}

/******************************************************************************
* Querying the database
******************************************************************************/

array<string>
font_database_families () {
  hashmap<string,bool> families;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2))
      families (key[0]->label)= true;
  }
  array<string> r;
  iterator<string> it2= iterate (families);
  while (it2->busy ())
    r << it2->next ();
  merge_sort (r);
  return r;
}

array<string>
font_database_styles (string family) {
  array<string> r;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && key[0]->label == family)
      r << key[1]->label;
  }
  merge_sort (r);
  return r;
}

array<string>
font_database_search (string family, string style) {
  array<string> r;
  tree key= tuple (family, style);
  if (font_table->contains (key)) {
    tree im= font_table [key];
    for (int i=0; i<N(im); i++)
      r << im[i]->label;
  }
  return r;
}

array<string>
font_database_search (string fam, string var, string series, string shape) {
  string family= fam;
  (void) var;
  string style = "Normal";
  if (series == "bold") {
    style= "Bold";
    if (shape == "italic") style= "Bold Italic";
  }
  else {
    if (shape == "italic") style= "Italic";
  }
  return font_database_search (family, style);
}
