
/******************************************************************************
* MODULE     : new_style.cpp
* DESCRIPTION: Style and DRD computation and caching
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_style.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "../../Typeset/env.hpp"

/******************************************************************************
* Global data
******************************************************************************/

struct style_data_rep {
  hashmap<tree,hashmap<string,tree> > style_cache;
  hashmap<tree,tree> style_drd;
  hashmap<string,bool> style_busy;
  hashmap<string,tree> style_void;
  drd_info drd_void;
  hashmap<tree,hashmap<string,tree> > style_cached;
  hashmap<tree,drd_info> drd_cached;

  style_data_rep ():
    style_cache (hashmap<string,tree> (UNINIT)),
    style_drd (tree (COLLECTION)),
    style_busy (false),
    style_void (UNINIT),
    drd_void ("void"),
    style_cached (style_void),
    drd_cached (drd_void) {}
};

static style_data_rep* sd= NULL;

static void
init_style_data () {
  if (sd == NULL) sd= tm_new<style_data_rep> ();
}

/******************************************************************************
* Caching style files on disk
******************************************************************************/

static string
cache_file_name (tree t) {
  if (is_atomic (t)) return t->label;
  else {
    string s;
    int i, n= N(t);
    for (i=0; i<n; i++)
      s << "__" << cache_file_name (t[i]);
    return s * "__";
  }
}

void
style_invalidate_cache () {
  init_style_data ();
  sd->style_cache=
    hashmap<tree,hashmap<string,tree> > (hashmap<string,tree> (UNINIT));
  remove ("$TEXMACS_HOME_PATH/system/cache" * url_wildcard ("__*"));
}

void
style_set_cache (tree style, hashmap<string,tree> H, tree t) {
  init_style_data ();
  // cout << "set cache " << style << LF;
  sd->style_cache (copy (style))= H;
  sd->style_drd   (copy (style))= t;
  url name ("$TEXMACS_HOME_PATH/system/cache", cache_file_name (style));
  if (!exists (name)) {
    save_string (name, tree_to_scheme (tuple ((tree) H, t)));
    // cout << "saved " << name << LF;
  }
}

void
style_get_cache (tree style, hashmap<string,tree>& H, tree& t, bool& f) {
  init_style_data ();
  //cout << "get cache " << style << LF;
  if ((style == "") || (style == tree (TUPLE))) { f= false; return; }
  f= sd->style_cache->contains (style);
  if (f) {
    H= sd->style_cache [style];
    t= sd->style_drd   [style];
  }
  else {
    string s;
    url name ("$TEXMACS_HOME_PATH/system/cache", cache_file_name (style));
    if (exists (name) && (!load_string (name, s, false))) {
      //cout << "loaded " << name << LF;
      tree p= scheme_to_tree (s);
      H= hashmap<string,tree> (UNINIT, p[0]);
      t= p[1];
      sd->style_cache (copy (style))= H;
      sd->style_drd   (copy (style))= t;
      f= true;
    }
  }
}

/******************************************************************************
* Get environment and drd of style files
******************************************************************************/

bool
compute_env_and_drd (tree style) {
  init_style_data ();
  ASSERT (is_tuple (style), "style tuple expected");
  bool busy= false;
  for (int i=0; i<N(style); i++)
    busy= busy || sd->style_busy->contains (as_string (style[i]));
  hashmap<string,bool> old_busy= copy (sd->style_busy);
  for (int i=0; i<N(style); i++)
    sd->style_busy (as_string (style[i]))= true;

  //cout << "Get environment of " << style << INDENT << LF;
  hashmap<string,tree> H;
  drd_info drd ("none", std_drd);
  url none= url ("$PWD/none");
  hashmap<string,tree> lref;
  hashmap<string,tree> gref;
  hashmap<string,tree> laux;
  hashmap<string,tree> gaux;
  edit_env env (drd, none, lref, gref, laux, gaux);
  if (!busy) {
    tree t;
    bool ok;
    style_get_cache (style, H, t, ok);
    if (ok) {
      env->patch_env (H);
      ok= drd->set_locals (t);
      drd->set_environment (H);
    }
    if (!ok) {
      env->exec (tree (USE_PACKAGE, A (style)));
      env->read_env (H);
      drd->heuristic_init (H);
    }
    sd->style_cached (style)= H;
    sd->drd_cached (style)= drd;
  }
  //cout << UNINDENT << "Got environment of " << style << LF;

  sd->style_busy= old_busy;
  return !busy;
}

hashmap<string,tree>
get_style_env (tree style) {
  //cout << "get_style_env " << style << "\n";
  init_style_data ();
  if (sd->style_cached->contains (style))
    return sd->style_cached [style];
  else if (compute_env_and_drd (style))
    return sd->style_cached [style];
  else {
    //cout << "Busy style: " << style << "\n";
    return hashmap<string,tree> ();
  }
}

drd_info
get_style_drd (tree style) {
  //cout << "get_style_drd " << style << "\n";
  init_style_data ();
  init_std_drd ();
  if (sd->drd_cached->contains (style))
    return sd->drd_cached [style];
  else if (compute_env_and_drd (style))
    return sd->drd_cached [style];
  else {
    //cout << "Busy drd: " << style << "\n";
    return std_drd;
  }
}

tree
get_document_preamble (tree t) {
  init_style_data ();
  if (is_atomic (t)) return "";
  else if (is_compound (t, "hide-preamble", 1)) return t[0];
  else if (is_compound (t, "show-preamble", 1)) return t[0];
  else {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree p= get_document_preamble (t[i]);
      if (p != "") return p;
    }
    return "";
  }
}

drd_info
get_document_drd (tree doc) {
  init_style_data ();
  tree style= extract (doc, "style");
  if (extract (doc, "TeXmacs") == "") {
    if (the_drd->get_syntax (make_tree_label ("theorem")) != tree (UNINIT))
      return the_drd;
    style= tree (TUPLE, "generic");
  }
  //cout << "style= " << style << "\n";
  drd_info drd= get_style_drd (style);
  tree p= get_document_preamble (doc);
  if (p != "") {
    drd= drd_info ("preamble", drd);
    url none= url ("$PWD/none");
    hashmap<string,tree> lref;
    hashmap<string,tree> gref;
    hashmap<string,tree> laux;
    hashmap<string,tree> gaux;
    edit_env env (drd, none, lref, gref, laux, gaux);
    hashmap<string,tree> H;
    env->exec (tree (USE_PACKAGE, A (style)));
    env->exec (p);
    env->read_env (H);
    drd->heuristic_init (H);
  }
  return drd;
}

/******************************************************************************
* The style and package menus
******************************************************************************/

static string
compute_style_menu (url u, int kind) {
  if (is_or (u)) {
    string sep= "\n";
    if (is_atomic (u[1]) &&
	((is_concat (u[2]) && (u[2][1] != "CVS") && (u[2][1] != ".svn")) ||
	 (is_or (u[2]) && is_concat (u[2][1]))))
      sep= "\n---\n";
    return
      compute_style_menu (u[1], kind) * sep *
      compute_style_menu (u[2], kind);
  }
  if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    string sub= compute_style_menu (u[2], kind);
    if ((dir == "Test") || (dir == "Obsolete") ||
	(dir == "CVS") || (dir == ".svn")) return "";
    return "(-> \"" * dir * "\" " * sub * ")";
  }
  if (is_atomic (u)) {
    string l  = as_string (u);
    if (!ends (l, ".ts")) return "";
    l= l(0, N(l)-3);
    string cmd ("init-style");
    if (kind == 1) cmd= "init-add-package";
    if (kind == 2) cmd= "init-remove-package";
    return "((verbatim \"" * l * "\") (" * cmd * " \"" * l * "\"))";
  }
  return "";
}

object
get_style_menu () {
  url sty_u= descendance ("$TEXMACS_STYLE_ROOT");
  string sty= compute_style_menu (sty_u, 0);
  return eval ("(menu-dynamic " * sty * ")");
}

object
get_add_package_menu () {
  url pck_u= descendance ("$TEXMACS_PACKAGE_ROOT");
  string pck= compute_style_menu (pck_u, 1);
  return eval ("(menu-dynamic " * pck * ")");
}

object
get_remove_package_menu () {
  url pck_u= descendance ("$TEXMACS_PACKAGE_ROOT");
  string pck= compute_style_menu (pck_u, 2);
  return eval ("(menu-dynamic " * pck * ")");
}
