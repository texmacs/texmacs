
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
#include "data_cache.hpp"

void font_database_filter_features ();
void font_database_filter_characteristics ();
static array<string> font_database_families (hashmap<tree,tree> ftab);

#define GLOBAL_DATABASE "$TEXMACS_PATH/fonts/font-database.scm"
#define GLOBAL_FEATURES "$TEXMACS_PATH/fonts/font-features.scm"
#define GLOBAL_FEATURES_BIS "$TEXMACS_PATH/fonts/font-features.bis.scm"
#define GLOBAL_CHARACTERISTICS "$TEXMACS_PATH/fonts/font-characteristics.scm"
#define GLOBAL_SUBSTITUTIONS "$TEXMACS_PATH/fonts/font-substitutions.scm"
#define LOCAL_DATABASE "$TEXMACS_HOME_PATH/fonts/font-database.scm"
#define LOCAL_FEATURES "$TEXMACS_HOME_PATH/fonts/font-features.scm"
#define LOCAL_CHARACTERISTICS \
  "$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"
#define DELTA_DATABASE "$TEXMACS_HOME_PATH/fonts/delta-database.scm"
#define DELTA_FEATURES "$TEXMACS_HOME_PATH/fonts/delta-features.scm"
#define DELTA_CHARACTERISTICS \
  "$TEXMACS_HOME_PATH/fonts/delta-characteristics.scm"

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
    for (int i=0; i<min(N(t1),N(t2)); i++) {
      if (leq (t1[i], t2[i]) && t1[i] != t2[i]) return true;
      if (leq (t2[i], t1[i]) && t2[i] != t1[i]) return false;
    }
    if (N(t1) < N(t2)) return true;
    if (N(t2) > N(t1)) return false;
    return true;
  }
};

/******************************************************************************
* Global management of the font database
******************************************************************************/

bool new_fonts= false;
static bool fonts_loaded= false;
static bool fonts_global_loaded= false;
hashmap<tree,tree> font_table (UNINIT);
hashmap<tree,tree> font_global_table (UNINIT);
hashmap<tree,tree> font_features (UNINIT);
hashmap<tree,tree> font_variants (UNINIT);
hashmap<tree,tree> font_characteristics (UNINIT);
hashmap<string,tree> font_substitutions (UNINIT);

void set_new_fonts (bool new_val) { new_fonts= new_val; }
bool get_new_fonts () { return new_fonts; }

void
tuple_insert (tree& t, tree x) {
  for (int i=0; i<N(t); i++)
    if (t[i] == x) return;
  t << x;
}

void
font_database_load_database (url u, hashmap<tree,tree>& ftab= font_table) {
  if (!exists (u)) return;
  string s;
  if (!load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2))
        ftab (t[i][0])= t[i][1];
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
        tuple_insert (vars, t[i][0]);
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
font_database_save_database (url u) {
  array<scheme_tree> r;
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    r << tuple (key, font_table [key]);
  }
  merge_sort_leq<scheme_tree,font_less_eq_operator> (r);
  string s= scheme_tree_to_block (tree (TUPLE, r));
  save_string (u, s);
  // FIXME: this should not be necessary
  remove ("$TEXMACS_PATH/system/cache/file_cache");
  cache_refresh ();
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
  // FIXME: this should not be necessary
  remove ("$TEXMACS_PATH/system/cache/file_cache");
  cache_refresh ();
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
  // FIXME: this should not be necessary
  remove ("$TEXMACS_PATH/system/cache/file_cache");
  cache_refresh ();
}

static array<string>
font_database_styles (string family, hashmap<tree,tree> ftab);

void
font_database_load_substitutions (url u) {
  if (!exists (u)) return;
  string s;
  if (!load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2) &&
          is_func (t[i][0], TUPLE) &&
          is_func (t[i][1], TUPLE) &&
          N(t[i][0]) > 0 &&
          N(t[i][1]) > 0 &&
          is_atomic (t[i][0][0]) &&
          is_atomic (t[i][1][0])) {
        string key= t[i][0][0]->label;
        string im = t[i][1][0]->label;
        if (N(font_database_styles (im, font_table)) != 0) {
          if (!font_substitutions->contains (key))
            font_substitutions (key)= tree (TUPLE);
          font_substitutions (key) << t[i];
        }
      }
  }
}

void
font_database_load () {
  if (fonts_loaded) return;
  font_database_load_database (LOCAL_DATABASE);
  if (N (font_table) == 0) {
    font_database_load_database (GLOBAL_DATABASE);
    font_database_filter ();
    font_database_save_database (LOCAL_DATABASE);
  }
  font_database_load_features (LOCAL_FEATURES);
  if (N (font_features) == 0) {
    font_database_load_features (GLOBAL_FEATURES);
    font_database_filter_features ();
    font_database_save_features (LOCAL_FEATURES);
  }
  font_database_load_characteristics (LOCAL_CHARACTERISTICS);
  if (N (font_characteristics) == 0) {
    font_database_load_characteristics (GLOBAL_CHARACTERISTICS);
    font_database_filter_characteristics ();
    font_database_save_characteristics (LOCAL_CHARACTERISTICS);
  }
  font_database_load_substitutions (GLOBAL_SUBSTITUTIONS);
  fonts_loaded= true;
}

void
font_database_global_load () {
  if (fonts_global_loaded) return;
  cout << "TeXmacs] warning, missing font, loading global substitution list\n";
  font_database_load_database (GLOBAL_DATABASE, font_global_table);
  font_database_load_features (GLOBAL_FEATURES);
  font_database_load_characteristics (GLOBAL_CHARACTERISTICS);
  font_database_load_substitutions (GLOBAL_SUBSTITUTIONS);
  fonts_global_loaded= true;
}

void
font_database_save () {
  font_database_save_database (LOCAL_DATABASE);
  font_database_save_features (LOCAL_FEATURES);
  font_database_save_characteristics (LOCAL_CHARACTERISTICS);
}

/******************************************************************************
* Building the database
******************************************************************************/

bool
on_blacklist (string name) {
  return
    name == "AppleMyungjo.ttf" ||
    name == "NISC18030.ttf" ||
    name == "Gungseouche.ttf" ||
    name == "blex.ttf" ||
    name == "blsy.ttf" ||
    name == "rblmi.ttf" ||
    starts (name, "FonetikaDania");
}

void
font_database_build (url u) {
  if (is_none (u));
  else if (is_or (u)) {
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
    if (on_blacklist (as_string (tail (u)))) return;
    cout << "Process " << u << "\n";
    scheme_tree t= tt_font_name (u);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE, 2) &&
          is_atomic (t[i][0]) &&
          is_atomic (t[i][1]))
        {
          int  sz = file_size (u);
          tree key= t[i];
          tree im = tuple (as_string (tail (u)), as_string (i), as_string (sz));
          tree all= tree (TUPLE);
          if (font_table->contains (key))
            all= font_table [key];
          tuple_insert (all, im);
          font_table (key)= all;
        }
  }
}

static void
font_database_guess_features () {
  array<string> families= font_database_families (font_table);
  for (int i=0; i<N(families); i++)
    if (!font_features->contains (families[i])) {
      array<string> a= guessed_features (families[i], false);
      tree t (TUPLE);
      for (int j=0; j<N(a); j++) t << tree (encode_feature (a[j]));
      font_features (families[i])= t;
    }
}

void
font_database_build_local () {
  font_database_load ();
  font_database_build (tt_font_path ());
  font_database_build_characteristics (false);
  font_database_guess_features ();
  font_database_save ();
}

void
font_database_extend_local (url u) {
  tt_extend_font_path (u);
  font_database_load ();
  font_database_build (u);
  font_database_build_characteristics (false);
  font_database_guess_features ();
  font_database_save ();
}

void
font_database_build_global (url u) {
  fonts_loaded= fonts_global_loaded= false;
  font_table= hashmap<tree,tree> (UNINIT);
  font_database_load_database (GLOBAL_DATABASE);
  font_database_load_features (GLOBAL_FEATURES);
  font_database_load_characteristics (GLOBAL_CHARACTERISTICS);
  fonts_loaded= fonts_global_loaded= true;
  font_database_build (u);
  font_database_build_characteristics (false);
  font_database_guess_features ();
  font_database_save_database (GLOBAL_DATABASE);
  font_database_save_features (GLOBAL_FEATURES_BIS);
  font_database_save_characteristics (GLOBAL_CHARACTERISTICS);
  fonts_loaded= fonts_global_loaded= false;
}

void
font_database_build_global () {
  font_database_build_global (tt_font_path ());
}

/******************************************************************************
* Build increments with respect to existing database
******************************************************************************/

void
keep_delta (hashmap<tree,tree>& new_t, hashmap<tree,tree> old_t) {
  iterator<tree> it= iterate (old_t);
  while (it->busy ()) {
    tree key= it->next ();
    if (new_t->contains (key) && old_t->contains (key))
      new_t->reset (key);
  }
}

void
font_database_save_local_delta () {
  fonts_loaded= fonts_global_loaded= false;
  font_table= hashmap<tree,tree> (UNINIT);
  font_features= hashmap<tree,tree> (UNINIT);
  font_variants= hashmap<tree,tree> (UNINIT);
  font_characteristics= hashmap<tree,tree> (UNINIT);
  font_database_load_database (GLOBAL_DATABASE);
  font_database_load_features (GLOBAL_FEATURES);
  font_database_load_characteristics (GLOBAL_CHARACTERISTICS);
  hashmap<tree,tree> old_font_table= font_table;
  hashmap<tree,tree> old_font_features= font_features;
  hashmap<tree,tree> old_font_characteristics= font_characteristics;
  fonts_loaded= fonts_global_loaded= false;
  font_table= hashmap<tree,tree> (UNINIT);
  font_features= hashmap<tree,tree> (UNINIT);
  font_variants= hashmap<tree,tree> (UNINIT);
  font_characteristics= hashmap<tree,tree> (UNINIT);
  font_database_load_database (GLOBAL_DATABASE);
  font_database_load_features (GLOBAL_FEATURES);
  font_database_load_characteristics (GLOBAL_CHARACTERISTICS);
  font_database_load_database (LOCAL_DATABASE);
  font_database_load_features (LOCAL_FEATURES);
  font_database_load_characteristics (LOCAL_CHARACTERISTICS);
  keep_delta (font_table, old_font_table);
  keep_delta (font_features, old_font_features);
  keep_delta (font_characteristics, old_font_characteristics);
  font_database_save_database (DELTA_DATABASE);
  font_database_save_features (DELTA_FEATURES);
  font_database_save_characteristics (DELTA_CHARACTERISTICS);
  font_table= hashmap<tree,tree> (UNINIT);
  font_features= hashmap<tree,tree> (UNINIT);
  font_variants= hashmap<tree,tree> (UNINIT);
  font_characteristics= hashmap<tree,tree> (UNINIT);
  fonts_loaded= fonts_global_loaded= false;
}

/******************************************************************************
* Only keep existing files in database
******************************************************************************/

static hashmap<tree,tree> new_font_table (UNINIT);
static hashmap<tree,tree> back_font_table (UNINIT);

void
build_back_entry (tree key, tree loc) {
  tree names (TUPLE);
  if (back_font_table->contains (loc))
    names= back_font_table [loc];
  tuple_insert (names, key);
  back_font_table (loc)= names;
}

void
build_back_table () {
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && is_tuple (font_table [key])) {
      tree im= font_table [key];
      for (int i=0; i<N(im); i++) {
        tree loc = im[i];
        build_back_entry (key, loc);
        if (is_func (loc, TUPLE, 3))
          build_back_entry (key, loc (0, 2));
      }
    }
  }
}

tree
find_best_approximation (tree ff) {
  int ref_sz= as_int (ff[2]);
  tree best= tree (TUPLE);
  tree keys= back_font_table [ff (0, 2)];
  for (int i=0; i<N(keys); i++) {
    tree key= keys[i];
    tree ims= font_table [key];
    if (!is_func (ims, TUPLE)) continue;
    for (int j=0; j<N(ims); j++) {
      if (is_tuple (ims[j]) && N(ims[j]) == 3 && ims[j] (0, 2) == ff (0, 2)) {
        if (N(best) == 0) best= ims[j];
        else {
          int old_sz= as_int (best[2]);
          int new_sz= as_int (ims[j][2]);
          if (max (new_sz - ref_sz, ref_sz - new_sz) <
              max (old_sz - ref_sz, ref_sz - old_sz))
            best= ims[j];
        }
      }
    }
  }
  for (int i=0; i<N(keys); i++) {
    tree key= keys[i];
    tree ims= font_table [key];
    if (!is_func (ims, TUPLE)) continue;
    for (int j=0; j<N(ims); j++) {
      if (is_tuple (ims[j]) && N(ims[j]) == 2 && ims[j] == ff (0, 2)) {
        if (N(best) == 0) best= ims[j];
        else if (N(best) == 3) {
          int old_sz= as_int (best[2]);
          if (old_sz > ref_sz + 2) best= ims[j];
        }
        break;
      }
    }
  }
  cout << "TeXmacs] approximating font " << ff << " ~> " << best << "\n";
  if (N(best) >= 2) return best;
  return ff (0, 2);
}

void
font_database_collect (url u) {
  if (is_none (u));
  else if (is_or (u)) {
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
            int  sz= file_size (u * a[i]);
            tree ff= tuple (a[i], as_string (j), as_string (sz));
            if (!back_font_table->contains (ff) &&
                 back_font_table->contains (ff (0, 2)))
              ff= find_best_approximation (ff);
            if (back_font_table->contains (ff)) {
              tree keys= back_font_table [ff];
              for (int j=0; j<N(keys); j++) {
                tree key= keys[j];
                tree im (TUPLE);
                if (new_font_table->contains (key))
                  im= new_font_table [key];
                tuple_insert (im, ff);
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
font_database_build_characteristics (bool force) {
  iterator<tree> it= iterate (font_table);
  while (it->busy ()) {
    tree key= it->next ();
    tree im = font_table[key];
    if (!(is_func (key, TUPLE) && N(key) >= 2)) continue;
    cout << "Analyzing " << key[0] << " " << key[1] << "\n";
    for (int i=0; i<N(im); i++)
      if (force || !font_characteristics->contains (key))
        if (is_func (im[i], TUPLE, 2)) {
          string name= as_string (im[i][0]);
          string nr  = as_string (im[i][1]);
          cout << "| Processing " << name << ", " << nr << "\n";
          if (ends (name, ".ttc"))
            name= (name (0, N(name)-4) * "." * nr * ".ttf");
          if (ends (name, ".ttf") ||
              ends (name, ".otf") ||
              ends (name, ".tfm")) {
            name= name (0, N(name)-4);
            if (!tt_font_exists (name) && ends (name, "10"))
              name= name (0, N(name)-2);
            if (tt_font_exists (name)) {
              array<string> a= tt_analyze (name);
              cout << name << " ~> " << a << "\n";
              tree t (TUPLE, N(a));
              for (int j=0; j<N(a); j++) t[j]= a[j];
              font_characteristics (key)= t;
            }
          }
        }
  }
}

/******************************************************************************
* Querying the database
******************************************************************************/

static array<string>
font_database_families (hashmap<tree,tree> ftab) {
  hashmap<string,bool> families;
  iterator<tree> it= iterate (ftab);
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
font_database_families () {
  font_database_load ();
  return font_database_families (font_table);
}

array<string>
font_database_delta_families () {
  hashmap<tree,tree> t;
  font_database_load_database (DELTA_DATABASE, t);
  return font_database_families (t);
}

static array<string>
font_database_styles (string family, hashmap<tree,tree> ftab) {
  array<string> r;
  iterator<tree> it= iterate (ftab);
  while (it->busy ()) {
    tree key= it->next ();
    if (is_func (key, TUPLE, 2) && key[0]->label == family)
      r << key[1]->label;
  }
  merge_sort_leq<string,locase_less_eq_operator> (r);
  return r;
}

array<string>
font_database_styles (string family) {
  font_database_load ();
  return font_database_styles (family, font_table);
}

array<string>
font_database_global_styles (string family) {
  font_database_global_load ();
  return font_database_styles (family, font_global_table);
}

array<string>
font_database_search (string family, string style) {
  font_database_load ();
  array<string> r;
  tree key= tuple (family, style);
  if (font_table->contains (key)) {
    tree im= font_table [key];
    for (int i=0; i<N(im); i++)
      if (is_func (im[i], TUPLE, 3) || is_func (im[i], TUPLE, 2)) {
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
  array<string> pfn= search_font (lfn);
  //cout << "Physical font: " << pfn << "\n";
  return font_database_search (pfn[0], pfn[1]);
}

array<string>
font_database_characteristics (string family, string style) {
  font_database_load ();
  array<string> r;
  tree key= tuple (family, style);
  if (font_characteristics->contains (key)) {
    tree im= font_characteristics [key];
    for (int i=0; i<N(im); i++)
      if (is_atomic (im[i]))
	r << im[i]->label;
  }
  return r;
}

tree
font_database_substitutions (string family) {
  font_database_load ();
  if (font_substitutions->contains (family))
    return font_substitutions [family];
  else return tree (TUPLE);
}
