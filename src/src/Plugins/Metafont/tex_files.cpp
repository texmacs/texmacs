
/******************************************************************************
* MODULE     : tex_files.cpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tex_files.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include "timer.hpp"
#include "data_cache.hpp"

static url the_tfm_path= url_none ();
static url the_pk_path = url_none ();
static url the_pfb_path= url_none ();

/******************************************************************************
* Finding a TeX font
******************************************************************************/

static string
kpsewhich (string name) {
  bench_start ("kpsewhich");
  string which= var_eval_system ("kpsewhich " * name);
  bench_cumul ("kpsewhich");
  return which;
}

static url
resolve_tfm (url name) {
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
  return resolve (the_tfm_path * name);
}

static url
resolve_pk (url name) {
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pk fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return resolve (the_pk_path * name);
}

static url
resolve_pfb (url name) {
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pfb fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return resolve (the_pfb_path * name);
}

/******************************************************************************
* Caching results
******************************************************************************/

url
resolve_tex (url name) {
  string s= as_string (name);
  if (is_cached ("font_cache.scm", s)) {
    url u= url_system (cache_get ("font_cache.scm", s) -> label);
    if (exists (u)) return u;
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("resolve tex");
  url u= url_none ();
  if (ends (s, "mf" )) {
    u= resolve_tfm (name);
#ifdef OS_WIN32
    if (is_none (u))
      u= resolve_tfm (replace (s, ".mf", ".tfm"));
#endif
  }
  if (ends (s, "tfm")) u= resolve_tfm (name);
  if (ends (s, "pk" )) u= resolve_pk  (name);
  if (ends (s, "pfb")) u= resolve_pfb (name);
  bench_cumul ("resolve tex");

  if (!is_none (u)) cache_set ("font_cache.scm", s, as_string (u));
  //cout << "Resolve " << name << " -> " << u << "\n";
  return u;
}

bool
exists_in_tex (url u) {
  return !is_none (resolve_tex (u));
}

/******************************************************************************
* Automatically generate missing fonts
******************************************************************************/

void
make_tex_tfm (string name) {
  string s;
  if (get_setting ("MAKETFM") == "MakeTeXTFM") {
    s= "MakeTeXTFM " * name;
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKETFM") == "mktextfm") {
    url tfm_dir ("$TEXMACS_HOME_PATH/fonts/tfm");
    s= "mktextfm " *
      string ("--destdir ") * as_string (tfm_dir) * " " *
      name;
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
    string superfluous= name * ".600pk";
    if (ends (name, ".tfm")) superfluous= name (0, N(name)-4) * ".600pk";
    remove (tfm_dir * superfluous);
  }
  if (get_setting ("MAKETFM") == "maketfm"){
    if (name(N(name) - 4, N(name)) == ".tfm")
      name = name (0, N(name) - 4);
    s = "maketfm --dest-dir \"" * get_env("$TEXMACS_HOME_PATH")
      * "\\fonts\\tfm\" " * name;
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
}

void
make_tex_pk (string name, int dpi, int design_dpi) {
  string s;
  if (get_setting ("MAKEPK") == "MakeTeXPK") {
    s="MakeTeXPK " * name * " " *
      as_string (dpi) * " " * as_string (design_dpi) * " " *
      as_string (dpi) * "/" * as_string (design_dpi) * " localfont";
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKEPK") == "mktexpk") {
    url pk_dir ("$TEXMACS_HOME_PATH/fonts/pk");
    s="mktexpk " *
      string ("--dpi ") * as_string (dpi) * " " *
      string ("--bdpi ") * as_string (design_dpi) * " " *
      string ("--mag ") * as_string (dpi) *"/"* as_string (design_dpi) * " " *
      string ("--destdir ") * as_string (pk_dir) * " " *
      name;
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKEPK") == "makepk") {
#ifdef OS_WIN32
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "%//" * as_string(design_dpi);
#else
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "/" * as_string(design_dpi);
#endif
    if (DEBUG_VERBOSE) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
}

/******************************************************************************
* Automatic determination of paths where TeX fonts might have been generated
******************************************************************************/

static url
get_kpsepath (string s) {
  // FIXME: adapt to WIN32
  if (get_setting ("KPSEPATH") != "true") return url_none ();
  string r= var_eval_system ("kpsepath " * s);
  if (N(r)==0) return url_none ();

  int i, start, end;
  url p= url_none ();
  for (i=0; i<N(r); i++) {
    while ((i<N(r)) && (r[i]=='!')) i++;
    start=i;
    while ((i<N(r)) && (r[i]!=':')) i++;
    end=i;
    while ((end>start) && (r[end-1]=='/')) end--;
    string dir= r (start, end);
    if (dir == ".") continue;
    p= expand (complete (dir * url_wildcard (), "dr")) | p;
  }
  return p;
}

static url
search_sub_dirs (url root) {
  url dirs= complete (root * url_wildcard (), "dr");
  return expand (dirs);
}

void
reset_tfm_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string tfm= get_setting ("TFM");
  the_tfm_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/tfm") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/tfm") |
    "$TEX_TFM_PATH" |
    (tfm == ""? url_none (): tfm);
  if ((get_setting ("MAKETFM") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_tfm_path= the_tfm_path | get_kpsepath ("tfm");
  the_tfm_path= expand (factor (the_tfm_path));
}

void
reset_pk_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string pk= get_setting ("PK");
  the_pk_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/pk") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/pk") |
    "$TEX_PK_PATH" |
    (pk == ""? url_none (): pk);
  if ((get_setting ("MAKEPK") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_pk_path= the_pk_path | get_kpsepath ("pk");
  the_pk_path= expand (factor (the_pk_path));
}

void
reset_pfb_path () {
  string pfb= get_setting ("PFB");
  the_pfb_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/type1") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/type1") |
    "$TEX_PFB_PATH" |
    (pfb == ""? url_none (): url_system (pfb));
  the_pfb_path= expand (factor (the_pfb_path));
}
