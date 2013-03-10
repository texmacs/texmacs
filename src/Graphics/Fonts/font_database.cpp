
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
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_tools.hpp"
#include "Metafont/tex_files.hpp"

void font_database_filter_features ();
void font_database_filter_characteristics ();

/******************************************************************************
* Additional comparison operators
******************************************************************************/

bool
locase_less_eq (string s1, string s2) {
  string l1= locase_all (s1);
  string l2= locase_all (s2);
  return l1 <= l2 || (l1 == l2 && s1 <= s2);
}

struct locase_less_eq_operator {
  static bool leq (string s1, string s2) {
    return locase_less_eq (s1, s2);
  }
};

struct font_less_eq_operator {
  static bool leq (scheme_tree t1, scheme_tree t2) {
    if (is_atomic (t1) && is_atomic (t2))
      return locase_less_eq (t1->label, t2->label);
    if (is_atomic (t1)) return true;
    if (is_atomic (t2)) return false;
    if (N(t1) < N(t2)) return true;
    if (N(t2) > N(t1)) return false;
    for (int i=0; i<N(t1); i++) {
      if (leq (t1[i], t2[i]) && t1[i] != t2[i]) return true;
      if (leq (t2[i], t1[i]) && t2[i] != t1[i]) return false;
    }
    return true;
  }
};

/******************************************************************************
* Global management of the font database
******************************************************************************/

static bool fonts_loaded= false;
hashmap<tree,tree> font_table (UNINIT);
hashmap<tree,tree> font_features (UNINIT);
hashmap<tree,tree> font_variants (UNINIT);
hashmap<tree,tree> font_characteristics (UNINIT);

void
font_database_load (url u) {
  if (!exists (u)) return;
  string s;
  if (!load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2))
        font_table (t[i][0])= t[i][1];
  }
}

void
font_database_load_features (url u) {
  if (!exists (u)) return;
  string s;
  if (!load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE) && (N(t[i]) >= 2)) {
        tree key= t[i][0];
        tree im = t[i] (1, N(t[i]));
        font_features (key)= im;
        tree vars (TUPLE);
        if (font_variants->contains (t[i][1]))
          vars= font_variants [t[i][1]];
        vars << t[i][0];
        font_variants (t[i][1])= vars;
      }
  }
}

void
font_database_load_characteristics (url u) {
  if (!exists (u)) return;
  string s;
  if (!load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2))
        font_characteristics (t[i][0])= t[i][1];
  }
}

void
font_database_save (url u) {
  array<scheme_tree> r;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    r << tuple (key, font_table [key]);
  }
  merge_sort_leq<scheme_tree,font_less_eq_operator> (r);
  string s= scheme_tree_to_block (tree (TUPLE, r));
  save_string (u, s);
}

void
font_database_save_features (url u) {
  array<scheme_tree> r;
  iterator<tree> it= iterate (font_features);
  while (it->busy ()) {
    tree key  = it->next ();
    tree entry= tuple (key);
    entry << A (font_features [key]);
    r << entry;
  }
  merge_sort_leq<scheme_tree,font_less_eq_operator> (r);
  string s= scheme_tree_to_block (tree (TUPLE, r));
  save_string (u, s);
}

void
font_database_save_characteristics (url u) {
  array<scheme_tree> r;
  iterator<tree> it= iterate (font_characteristics);
  while (it->busy ()) {
    tree key= it->next ();
    r << tuple (key, font_characteristics [key]);
  }
  merge_sort_leq<scheme_tree,font_less_eq_operator> (r);
  string s= scheme_tree_to_block (tree (TUPLE, r));
  save_string (u, s);
}

void
font_database_load () {
  if (fonts_loaded) return;
  font_database_load ("$TEXMACS_HOME_PATH/fonts/font-database.scm");
  if (N (font_table) == 0) {
    font_database_load ("$TEXMACS_PATH/fonts/font-database.scm");
    font_database_filter ();
    font_database_save ("$TEXMACS_HOME_PATH/fonts/font-database.scm");
  }
  font_database_load_features ("$TEXMACS_HOME_PATH/fonts/font-features.scm");
  if (N (font_features) == 0) {
    font_database_load_features ("$TEXMACS_PATH/fonts/font-features.scm");
    font_database_filter_features ();
    font_database_save_features ("$TEXMACS_HOME_PATH/fonts/font-features.scm");
  }
  font_database_load_characteristics ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm");
  if (N (font_features) == 0) {
    font_database_load_characteristics ("$TEXMACS_PATH/fonts/font-characteristics.scm");
    font_database_filter_characteristics ();
    font_database_save_characteristics ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm");
  }
  fonts_loaded= true;
}

void
font_database_save () {
  font_database_save ("$TEXMACS_HOME_PATH/fonts/font-database.scm");
}

/******************************************************************************
* Building the database
******************************************************************************/

void
font_database_build (url u) {
  if (is_or (u)) {
    font_database_build (u[1]);
    font_database_build (u[2]);
  }
  else if (is_directory (u)) {
    bool err;
    array<string> a= read_directory (u, err);
    for (int i=0; i<N(a); i++)
      if (!starts (a[i], "."))
        if (ends (a[i], ".ttf") ||
            ends (a[i], ".ttc") ||
            ends (a[i], ".otf"))
          font_database_build (u * url (a[i]));
  }
  else if (is_regular (u)) {
    cout << "Process " << u << "\n";
    scheme_tree t= tt_font_name (u);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2) &&
          is_atomic (t[i][0]) &&
          is_atomic (t[i][1]))
        {
          tree key= t[i];
          tree im = tuple (as_string (tail (u)), as_string (i));
          tree all= tree (TUPLE);
          if (font_table->contains (key))
            all= font_table [key];
          int j;
          for (j=0; j<N(all); j++)
            if (all[j] == im) break;
          if (j >= N(all)) all << im;
          font_table (key)= all;
        }
  }
}

void
font_database_build_local () {
  font_database_load ();
  font_database_build (tt_font_path ());
  font_database_save ();
}

void
font_database_build_global () {
  fonts_loaded= false;
  font_table= hashmap<tree,tree> (UNINIT);
  font_database_load ("$TEXMACS_PATH/fonts/font-database.scm");
  font_database_build (tt_font_path ());
  font_database_save ("$TEXMACS_PATH/fonts/font-database.scm");
  fonts_loaded= false;
}

void
font_database_build_global (url u) {
  fonts_loaded= false;
  font_table= hashmap<tree,tree> (UNINIT);
  font_database_load ("$TEXMACS_PATH/fonts/font-database.scm");
  font_database_build (u);
  font_database_save ("$TEXMACS_PATH/fonts/font-database.scm");
  fonts_loaded= false;
}

/******************************************************************************
* Only keep existing files in database
******************************************************************************/

static hashmap<tree,tree> new_font_table (UNINIT);
static hashmap<tree,tree> back_font_table (UNINIT);

void
build_back_table () {
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && is_tuple (font_table [key])) {
      tree im= font_table [key];
      for (int i=0; i<N(im); i++) {
        tree loc = im[i];
        tree names (TUPLE);
        if (back_font_table->contains (loc))
          names= back_font_table [loc];
        names << key;
        back_font_table (loc)= names;
      }
    }
  }  
}

void
font_database_collect (url u) {
  if (is_or (u)) {
    font_database_collect (u[1]);
    font_database_collect (u[2]);
  }
  else if (is_directory (u)) {
    bool err;
    array<string> a= read_directory (u, err);
    for (int i=0; i<N(a); i++)
      if (!starts (a[i], "."))
        if (ends (a[i], ".ttf") ||
            ends (a[i], ".ttc") ||
            ends (a[i], ".otf") ||
            ends (a[i], ".tfm"))
          for (int j=0; j<65536; j++) {
            tree ff= tuple (a[i], as_string (j));
            if (back_font_table->contains (ff)) {
              tree keys= back_font_table [ff];
              for (int j=0; j<N(keys); j++) {
                tree key= keys[j];
                tree im (TUPLE);
                if (new_font_table->contains (key))
                  im= new_font_table [key];
                im << ff;
                new_font_table (key)= im;
              }
            }
            else break;
          }
  }
}

void
font_database_filter () {
  new_font_table = hashmap<tree,tree> (UNINIT);
  back_font_table= hashmap<tree,tree> (UNINIT);
  build_back_table ();
  font_database_collect (tt_font_path ());
  font_database_collect (tfm_font_path ());
  font_table= new_font_table;
  new_font_table = hashmap<tree,tree> (UNINIT);
  back_font_table= hashmap<tree,tree> (UNINIT);
}

void
font_database_filter_features () {
  hashmap<string,bool> families;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && is_atomic (key[0]))
      families (key[0]->label)= true;
  }
  hashmap<tree,tree> new_font_features (UNINIT);
  it= iterate (font_features);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_atomic (key) && families->contains (key->label))
      new_font_features (key)= font_features [key];
  }
  font_features= new_font_features;
}

void
font_database_filter_characteristics () {
  hashmap<tree,tree> new_font_characteristics (UNINIT);
  iterator<tree> it= iterate (font_table);
  it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (font_characteristics->contains (key))
      new_font_characteristics (key)= font_characteristics [key];
  }
  font_characteristics= new_font_characteristics;
}

/******************************************************************************
* Additional font characteristics (automatically generated)
******************************************************************************/

void
font_database_build_characteristics () {
  font_database_load ();
  bool changed= false;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    tree im = font_table[key];
    for (int i=0; i<N(im); i++)
      if (!font_characteristics->contains (key))
        if (is_func (im[i], TUPLE, 2)) {
          string name= as_string (im[i][0]);
          string nr  = as_string (im[i][1]);
          if (ends (name, ".ttc"))
            name= (name (0, N(name)-4) * "." * nr * ".ttf");
          if (ends (name, ".ttf") ||
              ends (name, ".otf") ||
              ends (name, ".tfm")) {
            name= name (0, N(name)-4);
            if (tt_font_exists (name)) {
              array<string> a= tt_analyze (name);
              tree t (TUPLE, N(a));
              for (int j=0; j<N(a); j++) t[j]= a[j];
              font_characteristics (key)= t;
              changed= true;
            }
          }
        }
  }
  if (changed) font_database_save_characteristics ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm");
}

/******************************************************************************
* Querying the database
******************************************************************************/

array<string>
font_database_families () {
  font_database_load ();
  hashmap<string,bool> families;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && is_atomic (key[0]))
      families (key[0]->label)= true;
  }
  array<string> r;
  iterator<string> it2= iterate (families);
  while (it2->busy ())
    r << it2->next ();
  merge_sort_leq<string,locase_less_eq_operator> (r);
  return r;
}

array<string>
font_database_styles (string family) {
  font_database_load ();
  array<string> r;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && key[0]->label == family)
      r << key[1]->label;
  }
  merge_sort_leq<string,locase_less_eq_operator> (r);
  return r;
}

array<string>
font_database_search (string family, string style) {
  font_database_load ();
  array<string> r;
  tree key= tuple (family, style);
  if (font_table->contains (key)) {
    tree im= font_table [key];
    for (int i=0; i<N(im); i++)
      if (is_func (im[i], TUPLE, 2)) {
        string name= im[i][0]->label;
        string nr  = im[i][1]->label;
        if (!ends (name, ".ttc")) r << name;
        else r << (name (0, N(name)-4) * "." * nr * ".ttf");
      }
  }
  return r;
}

array<string>
font_database_search (string fam, string var, string series, string shape) {
  //cout << "Database search: " << fam << ", " << var
  //     << ", " << series << ", " << shape << "\n";
  array<string> lfn= logical_font (fam, var, series, shape);
  array<string> pfn= search_font (lfn, false);
  //cout << "Physical font: " << pfn << "\n";
  return font_database_search (pfn[0], pfn[1]);
}
